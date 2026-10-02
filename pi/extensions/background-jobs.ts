/**
 * Background Jobs Extension
 *
 * Gives the model model-managed concurrency: background shell commands and
 * background subagents that run while the agent keeps working, with an
 * event-driven `wait` (no sleep-polling), disk-spooled output, a one-line
 * status footer plus a per-turn ambient digest, and a "no survivors" cleanup
 * guarantee.
 *
 * Unifying idea: every job is a background job. There is no separate blocking
 * subagent path — a "synchronous subagent" is spawn_agent immediately followed
 * by wait. Shell jobs and agent jobs share one registry, one wait, one jobs,
 * one kill, and one cleanup path.
 *
 * See ~/.pi/agent/background-jobs-design.md for the full design + verified
 * constraints. This replaces the live-render subagent.ts; it reuses that file's
 * spawn guts (getPiInvocation, stdout JSON-event parsing, abort ladder) and
 * deletes its entire live-render layer.
 *
 * NO-SURVIVORS CAVEAT: the hard guarantee that background jobs never outlive pi
 * is a property of the sbox launch (--unshare-all --die-with-parent + PID
 * namespace: pi is PID 2, so when it dies the namespace reaps every descendant,
 * even kill -9). If pi is ever run OUTSIDE sbox, or a wrapper drops
 * --die-with-parent, survivors become possible; the session_shutdown +
 * process-exit sweeps below are the graceful path, not the hard guarantee.
 *
 * Built incrementally (see design's Build Plan). Stage 1: registry +
 * spawn_shell + jobs + session_shutdown reaper.
 */

import { type ChildProcess, spawn } from "node:child_process";
import { EventEmitter } from "node:events";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { Type } from "typebox";
import { StringEnum } from "@earendil-works/pi-ai";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

type JobKind = "shell" | "agent";
type JobState = "running" | "done";

interface JobRecord {
  jobId: string;
  kind: JobKind;
  state: JobState;
  pid: number;
  cwd: string;
  command?: string; // shell
  prompt?: string; // agent
  startTime: number;
  exitCode?: number; // when done
  logPath?: string; // shell
  finalAnswerPath?: string; // agent, when done
  sessionFile?: string; // agent, when known
  // internal (not part of the public contract)
  _child?: ChildProcess;
}

/** Per-job status as returned by wait / jobs (no internal fields). */
interface JobStatus {
  jobId: string;
  kind?: JobKind;
  state: JobState | "unknown";
  pid?: number;
  cwd?: string;
  command?: string;
  prompt?: string;
  startTime?: number;
  exitCode?: number;
  logPath?: string;
  finalAnswerPath?: string;
  sessionFile?: string;
}

interface SessionState {
  sessionId: string;
  registry: Map<string, JobRecord>;
  /** Emits `done:<jobId>` when a job transitions to done; `change` on any change. */
  events: EventEmitter;
  seq: number;
  /** Session-scoped UI handle for the footer; re-acquired each session_start. */
  ui?: { setStatus: (key: string, text: string | undefined) => void };
  /** jobIds whose completion has already been surfaced in a digest (Option A). */
  surfaced: Set<string>;
}

// ---------------------------------------------------------------------------
// Module-level session-scoped state
// ---------------------------------------------------------------------------

// Re-acquired per session in session_start (a captured ctx throws after the
// session is replaced/reloaded — design footgun "session-replacement
// staleness"). Never started from the extension factory.
let state: SessionState | null = null;

const SPOOL_DIR = "/tmp/pi-bg";

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function ensureSpoolDir(): void {
  fs.mkdirSync(SPOOL_DIR, { recursive: true });
}

function shellLogPath(sessionId: string, jobId: string): string {
  return path.join(SPOOL_DIR, `${sessionId}-${jobId}.log`);
}

function agentAnswerPath(sessionId: string, jobId: string): string {
  return path.join(SPOOL_DIR, `${sessionId}-${jobId}.answer.txt`);
}

/**
 * Locate the pi binary to spawn a child agent. Handles: bun-compiled binary
 * (argv[1] is a virtual /$bunfs path -> use bare `pi`), node-script install
 * (re-run the same script with node), and generic runtime fallback.
 * Ported from subagent.ts.
 */
function getPiInvocation(args: string[]): { command: string; args: string[] } {
  const currentScript = process.argv[1];
  const isBunVirtualScript = currentScript?.startsWith("/$bunfs/root/");
  if (currentScript && !isBunVirtualScript && fs.existsSync(currentScript)) {
    return { command: process.execPath, args: [currentScript, ...args] };
  }
  const execName = path.basename(process.execPath).toLowerCase();
  const isGenericRuntime = /^(node|bun)(\.exe)?$/.test(execName);
  if (!isGenericRuntime) {
    return { command: process.execPath, args };
  }
  return { command: "pi", args };
}

/**
 * Encode a cwd into pi's sessions-dir name. Verified against pi v0.83.0:
 * a path like /tmp/pi-probe becomes `--tmp-pi-probe--` (leading/trailing `--`,
 * non-alphanumerics collapsed to `-`). We only use this to find the dir; the
 * new .jsonl inside it is claimed by matching the child's session `id`.
 */
function sessionsDirForCwd(cwd: string): string {
  const encoded = cwd.replace(/[^a-zA-Z0-9]+/g, "-").replace(/^-+|-+$/g, "");
  return path.join(os.homedir(), ".pi", "agent", "sessions", `--${encoded}--`);
}

function nextJobId(s: SessionState): string {
  s.seq += 1;
  // short unique-ish id: sequence + a little entropy
  return `j${s.seq}${Math.random().toString(36).slice(2, 6)}`;
}

function toStatus(r: JobRecord): JobStatus {
  return {
    jobId: r.jobId,
    kind: r.kind,
    state: r.state,
    pid: r.pid,
    cwd: r.cwd,
    command: r.command,
    prompt: r.prompt,
    startTime: r.startTime,
    exitCode: r.exitCode,
    logPath: r.logPath,
    finalAnswerPath: r.finalAnswerPath,
    sessionFile: r.sessionFile,
  };
}

/** Counts for the awareness tiers. */
function counts(s: SessionState): { running: number; done: number } {
  let running = 0;
  let done = 0;
  for (const r of s.registry.values()) {
    if (r.state === "running") running += 1;
    else done += 1;
  }
  return { running, done };
}

/** One-line human footer text. Empty string means "clear the footer". */
function statusLine(s: SessionState): string {
  const { running, done } = counts(s);
  // Option A: silence when nothing is in flight. Clear the footer once idle
  // rather than leaving a stale "0 running" line forever.
  if (running === 0) return "";
  return `\u25b6 ${running} running \u00b7 \u2713 ${done} done`;
}

/**
 * Terse ambient digest for the model (Option A). Regenerated each turn from
 * live registry state; never persisted (fact 7). Surfaces completions exactly
 * once: while jobs are in flight it lists running jobs (ambient awareness), and
 * it announces each newly-finished job a single time. When nothing is running
 * and there is nothing new to report it returns an empty digest so we inject
 * nothing at all — no permanent per-turn tax.
 *
 * Returns the text plus the jobIds to mark surfaced (the caller marks them only
 * if it actually injects, keeping generation side-effect free).
 */
function buildDigest(s: SessionState): { text: string; toSurface: string[] } {
  const jobs = [...s.registry.values()];
  const running = jobs.filter((r) => r.state === "running");
  const newlyDone = jobs.filter((r) => r.state === "done" && !s.surfaced.has(r.jobId));

  // Nothing in flight and nothing new -> stay silent.
  if (running.length === 0 && newlyDone.length === 0) return { text: "", toSurface: [] };

  const line = (r: JobRecord): string => {
    const tag = r.kind === "agent" ? "agent" : "shell";
    if (r.state === "running") return `  ${r.jobId} [${tag}] running`;
    return `  ${r.jobId} [${tag}] finished exit=${r.exitCode ?? "?"}`;
  };

  const MAX = 6;
  const parts: string[] = [];
  if (running.length > 0) parts.push(`${running.length} running`);
  if (newlyDone.length > 0) parts.push(`${newlyDone.length} newly finished`);
  const header = `Background jobs: ${parts.join(", ")}. Use wait/jobs/read (paths from spawn) for detail.`;

  // Show newly-finished first (the thing to notice), then still-running.
  const ordered = [...newlyDone, ...running];
  const shown = ordered.slice(0, MAX).map(line);
  const more = ordered.length > MAX ? `\n  \u2026 ${ordered.length - MAX} more` : "";

  return {
    text: `${header}\n${shown.join("\n")}${more}`,
    toSurface: newlyDone.map((r) => r.jobId),
  };
}

/**
 * Send a signal to the whole process group (job + descendants). Jobs are
 * spawned detached (setsid), so the child is a process-group leader and
 * process.kill(-pid, ...) hits the entire group in one atomic signal.
 * Best-effort and idempotent: ignores ESRCH (already gone).
 */
function signalGroup(pid: number, signal: NodeJS.Signals): void {
  try {
    process.kill(-pid, signal);
  } catch {
    // group gone or never existed; ignore
  }
}

/** SIGTERM the whole registry, then SIGKILL survivors after a short grace. */
async function reapAll(s: SessionState, graceMs = 500): Promise<void> {
  const running = [...s.registry.values()].filter((r) => r.state === "running");
  for (const r of running) signalGroup(r.pid, "SIGTERM");
  if (running.length === 0) return;
  await new Promise((res) => setTimeout(res, graceMs));
  for (const r of running) {
    if (r.state === "running") signalGroup(r.pid, "SIGKILL");
  }
}

// ---------------------------------------------------------------------------
// spawn_shell
// ---------------------------------------------------------------------------

/**
 * Launch a background shell job. Throws synchronously on launch failure
 * (invalid cwd, cannot create logfile, cannot spawn) so the tool can return an
 * immediate error and create NO job record.
 */
function spawnShell(s: SessionState, command: string, cwd: string): JobRecord {
  ensureSpoolDir();
  const jobId = nextJobId(s);
  const logPath = shellLogPath(s.sessionId, jobId);

  // Open the combined logfile up front. stdout+stderr both redirect to this fd
  // at the OS level, preserving true terminal order. Throwing here (before
  // spawn) counts as a synchronous launch failure -> no job record.
  const logFd = fs.openSync(logPath, "a");

  let child: ChildProcess;
  try {
    child = spawn("/bin/sh", ["-c", command], {
      cwd,
      detached: true, // setsid -> process-group leader, for group-kill
      stdio: ["ignore", logFd, logFd],
      env: process.env,
    });
  } finally {
    // Child dups the fd; close our copy so we don't hold it open.
    fs.closeSync(logFd);
  }

  if (typeof child.pid !== "number") {
    throw new Error("failed to spawn shell (no pid)");
  }

  const record: JobRecord = {
    jobId,
    kind: "shell",
    state: "running",
    pid: child.pid,
    cwd,
    command,
    startTime: Date.now(),
    logPath,
    _child: child,
  };
  s.registry.set(jobId, record);

  let exitCode: number | undefined;
  // 'exit' gives the code; 'close' guarantees stdio is fully drained/flushed.
  child.on("exit", (code, signal) => {
    exitCode = code ?? (signal ? 128 : 0);
  });
  child.on("error", () => {
    // async spawn error after we already returned; mark done nonzero
    exitCode = exitCode ?? 1;
    finalizeDone(s, record, exitCode);
  });
  child.on("close", () => {
    finalizeDone(s, record, exitCode ?? 0);
  });

  return record;
}

function finalizeDone(s: SessionState, record: JobRecord, exitCode: number): void {
  if (record.state === "done") return;
  record.state = "done";
  record.exitCode = exitCode;
  record._child = undefined;
  s.events.emit(`done:${record.jobId}`, record);
  s.events.emit("change");
}

// ---------------------------------------------------------------------------
// spawn_agent
// ---------------------------------------------------------------------------

/**
 * Extract the last assistant text from a parsed message_end event's message.
 * Agent final-answer capture per the design's tool_result_end correction:
 * the answer is the last assistant message's text, tracked as events stream.
 */
function assistantText(message: any): string | undefined {
  if (!message || message.role !== "assistant" || !Array.isArray(message.content)) return undefined;
  const texts = message.content
    .filter((p: any) => p?.type === "text" && typeof p.text === "string")
    .map((p: any) => p.text);
  return texts.length ? texts.join("") : undefined;
}

/**
 * Launch a background agent job (a child pi in headless json mode). Throws
 * synchronously on launch failure -> immediate tool error, no record.
 *
 * Two flavors, one code path (the design's "one registry, one wait, one kill"
 * spirit):
 *  - fresh spawn: no `sessionFile` given. The child writes a NEW resumable
 *    session file (fact 6); we don't know its path at spawn, so we claim the
 *    new .jsonl by matching the child's `session` event `id` in the encoded
 *    sessions dir for its cwd.
 *  - resume: `sessionFile` given. We pass `--session <file>` so the child
 *    appends to that exact session (verified race-free vs --continue), and we
 *    set record.sessionFile up front since we already know it.
 */
function launchAgentJob(
  s: SessionState,
  opts: { prompt: string; model: string | undefined; cwd: string; sessionFile?: string },
): JobRecord {
  const { prompt, model, cwd, sessionFile } = opts;
  ensureSpoolDir();
  const jobId = nextJobId(s);
  const finalAnswerPath = agentAnswerPath(s.sessionId, jobId);

  // Drop --no-session so the child writes/keeps a resumable session file.
  const args: string[] = ["--mode", "json", "-p"];
  if (sessionFile) args.push("--session", sessionFile); // resume exact session
  if (model) args.push("--model", model);
  args.push(prompt);
  const invocation = getPiInvocation(args);

  // Only needed for fresh-spawn discovery; harmless otherwise.
  const sessDir = sessionsDirForCwd(cwd);

  const child = spawn(invocation.command, invocation.args, {
    cwd,
    detached: true, // process-group leader, for group-kill
    stdio: ["ignore", "pipe", "pipe"],
    env: process.env,
  });

  if (typeof child.pid !== "number") {
    throw new Error("failed to spawn agent (no pid)");
  }

  const record: JobRecord = {
    jobId,
    kind: "agent",
    state: "running",
    pid: child.pid,
    cwd,
    prompt,
    startTime: Date.now(),
    finalAnswerPath,
    // On resume we already know the session file; on fresh spawn we discover it.
    sessionFile: sessionFile,
    _child: child,
  };
  s.registry.set(jobId, record);

  let lastAnswer = "";
  let sessionId: string | undefined;
  let buffer = "";

  const claimSessionFile = (id: string) => {
    if (record.sessionFile) return; // already known (resume)
    // The on-disk filename embeds the session id: <timestamp>_<id>.jsonl.
    try {
      const match = fs
        .readdirSync(sessDir)
        .find((f) => f.includes(id) && f.endsWith(".jsonl"));
      if (match) record.sessionFile = path.join(sessDir, match);
    } catch {
      // dir may not exist yet on the very first session for this cwd; the child
      // creates it, so retry lazily on subsequent header/lines is unnecessary
      // because the header comes after the dir/file exist.
    }
  };

  const processLine = (line: string) => {
    const trimmed = line.trim();
    if (!trimmed) return;
    let event: any;
    try {
      event = JSON.parse(trimmed);
    } catch {
      return;
    }
    if (event.type === "session" && typeof event.id === "string") {
      sessionId = event.id;
      claimSessionFile(event.id);
    }
    // Capture the agent's final answer from the last assistant message_end.
    // (tool_result_end does not exist in pi v0.83.0 — do not parse it.)
    if (event.type === "message_end") {
      const t = assistantText(event.message);
      if (t !== undefined) lastAnswer = t;
    }
  };

  child.stdout?.on("data", (data) => {
    buffer += data.toString();
    const lines = buffer.split("\n");
    buffer = lines.pop() ?? "";
    for (const line of lines) processLine(line);
  });
  // stderr is drained to avoid backpressure; not spooled in v1.
  child.stderr?.on("data", () => {});

  let exitCode: number | undefined;
  child.on("exit", (code, signal) => {
    exitCode = code ?? (signal ? 128 : 0);
  });
  child.on("error", () => {
    exitCode = exitCode ?? 1;
    finalizeAgent(s, record, exitCode, () => {
      if (buffer.trim()) processLine(buffer);
      return lastAnswer;
    });
  });
  child.on("close", () => {
    // close guarantees stdio drained; flush any partial trailing line.
    if (buffer.trim()) processLine(buffer);
    if (sessionId && !record.sessionFile) claimSessionFile(sessionId);
    finalizeAgent(s, record, exitCode ?? 0, () => lastAnswer);
  });

  return record;
}

/**
 * Complete an agent job: write the final answer to disk BEFORE marking done,
 * so a subsequent read of finalAnswerPath never sees a truncated file (design
 * completion-boundary requirement).
 */
function finalizeAgent(
  s: SessionState,
  record: JobRecord,
  exitCode: number,
  getAnswer: () => string,
): void {
  if (record.state === "done") return;
  try {
    if (record.finalAnswerPath) {
      fs.writeFileSync(record.finalAnswerPath, `${getAnswer()}\n`);
    }
  } catch {
    // best-effort; the record still transitions to done with its exit code
  }
  finalizeDone(s, record, exitCode);
}

// ---------------------------------------------------------------------------
// Tool parameter schemas
// ---------------------------------------------------------------------------

const SpawnShellParams = Type.Object({
  command: Type.String({ description: "Shell command to run in the background (via /bin/sh -c)." }),
  cwd: Type.Optional(
    Type.String({
      description: "Working directory. Defaults to the session cwd; relative paths resolve against it.",
    }),
  ),
});

const JobsParams = Type.Object({});

const SpawnAgentParams = Type.Object({
  prompt: Type.String({ description: "Prompt for the background subagent (a fresh pi)." }),
  model: Type.Optional(
    Type.String({
      description: "Optional model override (provider/id). Defaults to the current session's model.",
    }),
  ),
  cwd: Type.Optional(
    Type.String({
      description:
        "Working directory for the subagent. Defaults to the session cwd; relative paths resolve against it. " +
        "Use this to run the subagent in a git worktree.",
    }),
  ),
});

const ResumeAgentParams = Type.Object({
  sessionFile: Type.String({
    description:
      "Absolute path to the subagent session file to resume (the sessionFile from a prior " +
      "spawn_agent/resume_agent job). The resumed run appends to this exact session.",
  }),
  prompt: Type.String({ description: "Follow-up prompt to send into the resumed subagent session." }),
  model: Type.Optional(
    Type.String({
      description: "Optional model override (provider/id). Defaults to the current session's model.",
    }),
  ),
});

const KillParams = Type.Object({
  jobId: Type.String({ description: "Job to terminate (along with its descendants)." }),
});

const WaitParams = Type.Object({
  jobIds: Type.Array(Type.String(), {
    description: "Job ids to wait on. Empty array returns immediately with [].",
  }),
  mode: Type.Optional(
    StringEnum(["all", "any"] as const, {
      description:
        "'all' (default): resolve when every requested job is done. 'any': resolve when the first finishes.",
    }),
  ),
  timeoutMs: Type.Optional(
    Type.Number({
      description:
        "Set only when a concrete deadline is required: a user-specified time budget, an external deadline, or an explicitly planned progress check. Never invent a duration. If elapsed before the condition is met, resolve early and still-running jobs appear with state 'running'.",
    }),
  ),
});

/**
 * Block until the wait condition is met. Event-driven (listens for
 * `done:<jobId>`) — no sleep-polling. Honors the abort signal so Esc cancels a
 * hanging wait promptly (the jobs themselves keep running). Always resolves
 * with a per-job status array covering ALL requested jobs.
 */
function waitForJobs(
  s: SessionState,
  rawJobIds: string[],
  mode: "all" | "any",
  timeoutMs: number | undefined,
  signal: AbortSignal | undefined,
): Promise<{ statuses: JobStatus[]; aborted: boolean; timedOut: boolean }> {
  // De-duplicate, preserve order.
  const jobIds = [...new Set(rawJobIds)];

  const statusFor = (id: string): JobStatus => {
    const r = s.registry.get(id);
    return r ? toStatus(r) : { jobId: id, state: "unknown" };
  };
  const snapshot = () => jobIds.map(statusFor);

  // A job counts as "settled" for wait purposes if it is done or unknown
  // (unknown will never complete, so blocking on it would hang forever).
  const isSettled = (id: string): boolean => {
    const r = s.registry.get(id);
    return !r || r.state === "done";
  };
  const conditionMet = (): boolean =>
    mode === "all" ? jobIds.every(isSettled) : jobIds.some(isSettled);

  return new Promise((resolve) => {
    if (jobIds.length === 0 || conditionMet()) {
      resolve({ statuses: snapshot(), aborted: false, timedOut: false });
      return;
    }

    let settledOnce = false;
    const cleanup: Array<() => void> = [];
    const finish = (aborted: boolean, timedOut: boolean) => {
      if (settledOnce) return;
      settledOnce = true;
      for (const fn of cleanup) fn();
      resolve({ statuses: snapshot(), aborted, timedOut });
    };

    // One listener per pending job's completion.
    for (const id of jobIds) {
      if (isSettled(id)) continue;
      const onDone = () => {
        if (conditionMet()) finish(false, false);
      };
      s.events.on(`done:${id}`, onDone);
      cleanup.push(() => s.events.off(`done:${id}`, onDone));
    }

    if (typeof timeoutMs === "number" && timeoutMs >= 0) {
      const t = setTimeout(() => finish(false, true), timeoutMs);
      cleanup.push(() => clearTimeout(t));
    }

    if (signal) {
      const onAbort = () => finish(true, false);
      if (signal.aborted) {
        finish(true, false);
        return;
      }
      signal.addEventListener("abort", onAbort, { once: true });
      cleanup.push(() => signal.removeEventListener("abort", onAbort));
    }
  });
}

// ---------------------------------------------------------------------------
// Extension
// ---------------------------------------------------------------------------

export default function (pi: ExtensionAPI) {
  function requireState(): SessionState {
    if (!state) throw new Error("background-jobs: no active session state");
    return state;
  }

  pi.on("session_start", async (_event, ctx: ExtensionContext) => {
    // Defensive: reap any leftover state from a prior session in this process.
    if (state) {
      await reapAll(state).catch(() => {});
    }
    const sessionId = ctx.sessionManager.getSessionId() ?? `nosess-${process.pid}`;
    const s: SessionState = {
      sessionId,
      registry: new Map(),
      events: new EventEmitter(),
      seq: 0,
      surfaced: new Set(),
    };
    // EventEmitter default max listeners is 10; a wait on many jobs adds many.
    s.events.setMaxListeners(0);
    // Capture this session's UI for out-of-band footer updates. setStatus is
    // safe from async callbacks while idle (fact 10); a no-op in print/json.
    if (ctx.hasUI) {
      s.ui = { setStatus: (k, t) => ctx.ui.setStatus(k, t) };
    }
    // Footer reflects live registry state on every change. statusLine returns
    // "" when idle (Option A); pass undefined to clear rather than show it.
    const updateFooter = () => {
      try {
        const line = statusLine(s);
        s.ui?.setStatus("bg", line === "" ? undefined : line);
      } catch {
        // stale/replaced session; ignore
      }
    };
    s.events.on("change", updateFooter);
    state = s;
  });

  pi.on("session_shutdown", async (_event) => {
    if (state) {
      try {
        state.ui?.setStatus("bg", undefined);
      } catch {
        // ignore
      }
      await reapAll(state).catch(() => {});
      state = null;
    }
  });

  // Per-turn ambient digest: injected non-destructively via the context event.
  // Regenerated each turn from live registry state; never persisted (fact 7).
  // Option A: surfaces each completion exactly once, then goes silent when idle.
  pi.on("context", async (event) => {
    if (!state) return;
    const { text, toSurface } = buildDigest(state);
    if (!text) return;
    // Mark surfaced now that we're actually injecting, so a finished job is
    // announced once and never re-listed on later idle turns.
    for (const id of toSurface) state.surfaced.add(id);
    return {
      messages: [
        ...event.messages,
        {
          role: "user" as const,
          content: [{ type: "text" as const, text: `[background-jobs]\n${text}` }],
          timestamp: Date.now(),
        },
      ],
    };
  });

  // Backstop sweeps for hard exits (belt-and-suspenders with the sbox PID
  // namespace). Synchronous best-effort SIGTERM+SIGKILL of the current registry.
  const processSweep = () => {
    if (!state) return;
    for (const r of state.registry.values()) {
      if (r.state === "running") {
        signalGroup(r.pid, "SIGTERM");
        signalGroup(r.pid, "SIGKILL");
      }
    }
  };
  process.on("exit", processSweep);
  process.on("SIGTERM", processSweep);

  pi.registerTool({
    name: "spawn_shell",
    label: "Spawn Shell Job",
    description:
      "Launch a shell command as a background job and return immediately with { jobId, logPath }. " +
      "stdout+stderr are combined into logPath (read it with the read tool). Use `wait` to block on " +
      "completion, `jobs` for a snapshot, `kill` to terminate.",
    parameters: SpawnShellParams,
    async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
      const s = requireState();
      const command = params.command;
      const resolvedCwd = params.cwd
        ? path.resolve(ctx.cwd, params.cwd)
        : ctx.cwd;

      // Synchronous launch-failure validation -> immediate tool error, no record.
      if (!fs.existsSync(resolvedCwd) || !fs.statSync(resolvedCwd).isDirectory()) {
        return {
          content: [{ type: "text", text: `spawn_shell failed: cwd not a directory: ${resolvedCwd}` }],
          details: { error: "invalid_cwd", cwd: resolvedCwd },
          isError: true,
        };
      }

      let record: JobRecord;
      try {
        record = spawnShell(s, command, resolvedCwd);
      } catch (err) {
        return {
          content: [{ type: "text", text: `spawn_shell failed: ${(err as Error).message}` }],
          details: { error: "spawn_failed", message: (err as Error).message },
          isError: true,
        };
      }

      s.events.emit("change");
      return {
        content: [
          {
            type: "text",
            text: JSON.stringify({ jobId: record.jobId, logPath: record.logPath }),
          },
        ],
        details: { jobId: record.jobId, logPath: record.logPath, pid: record.pid },
      };
    },
  });

  pi.registerTool({
    name: "wait",
    label: "Wait For Jobs",
    description:
      "Block until background jobs finish, then return a per-job status array (with exitCode and " +
      "result paths). mode 'all' (default) waits for all; 'any' returns when the first finishes. " +
      "Set timeoutMs only for a concrete deadline: a user-specified time budget, an external deadline, or an explicitly " +
      "planned progress check. Never invent a duration. It returns partial status when it elapses. Read-only and repeatable: " +
      "observing a completion does not consume it. Esc cancels a waiting call; the jobs keep running.",
    parameters: WaitParams,
    async execute(_toolCallId, params, signal) {
      const s = requireState();
      const mode = params.mode === "any" ? "any" : "all";
      const { statuses, aborted, timedOut } = await waitForJobs(
        s,
        params.jobIds,
        mode,
        params.timeoutMs,
        signal,
      );
      return {
        content: [{ type: "text", text: JSON.stringify(statuses, null, 2) }],
        details: { jobs: statuses, mode, aborted, timedOut },
        isError: aborted,
      };
    },
  });

  pi.registerTool({
    name: "jobs",
    label: "List Jobs",
    description:
      "Non-blocking snapshot of all background jobs in this session (running and done), including " +
      "every relevant path so the info survives even if an earlier spawn response scrolled away.",
    parameters: JobsParams,
    async execute() {
      const s = requireState();
      const snapshot = [...s.registry.values()].map(toStatus);
      return {
        content: [{ type: "text", text: JSON.stringify(snapshot, null, 2) }],
        details: { jobs: snapshot },
      };
    },
  });

  pi.registerTool({
    name: "spawn_agent",
    label: "Spawn Agent Job",
    description:
      "Launch a background subagent (a fresh pi) as a job and return immediately with { jobId }. " +
      "Runs in the current cwd by default (pass cwd to run in a git worktree). On completion the job " +
      "gains an exitCode, a finalAnswerPath (the subagent's final answer, read it with the read tool) " +
      "and a resumable sessionFile. Use resume_agent with that sessionFile to continue the same " +
      "subagent; wait to block, jobs for a snapshot, kill to terminate.",
    parameters: SpawnAgentParams,
    async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
      const s = requireState();
      let model = params.model;
      if (!model && ctx.model) model = `${ctx.model.provider}/${ctx.model.id}`;
      if (!model) {
        return {
          content: [{ type: "text", text: "spawn_agent failed: no active model and no override." }],
          details: { error: "no_model" },
          isError: true,
        };
      }
      const resolvedCwd = params.cwd ? path.resolve(ctx.cwd, params.cwd) : ctx.cwd;
      if (!fs.existsSync(resolvedCwd) || !fs.statSync(resolvedCwd).isDirectory()) {
        return {
          content: [{ type: "text", text: `spawn_agent failed: cwd not a directory: ${resolvedCwd}` }],
          details: { error: "invalid_cwd", cwd: resolvedCwd },
          isError: true,
        };
      }
      let record: JobRecord;
      try {
        record = launchAgentJob(s, { prompt: params.prompt, model, cwd: resolvedCwd });
      } catch (err) {
        return {
          content: [{ type: "text", text: `spawn_agent failed: ${(err as Error).message}` }],
          details: { error: "spawn_failed", message: (err as Error).message },
          isError: true,
        };
      }
      s.events.emit("change");
      return {
        content: [{ type: "text", text: JSON.stringify({ jobId: record.jobId }) }],
        details: { jobId: record.jobId, pid: record.pid },
      };
    },
  });

  pi.registerTool({
    name: "resume_agent",
    label: "Resume Agent Job",
    description:
      "Resume a prior subagent by its sessionFile, sending a follow-up prompt into that exact same " +
      "session (its full context is preserved). Returns immediately with { jobId }; on completion the " +
      "job gains an exitCode, a finalAnswerPath, and the same sessionFile (so you can resume again). " +
      "Use this for persistent subagents that continue across multiple rounds. wait/jobs/kill as usual.",
    parameters: ResumeAgentParams,
    async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
      const s = requireState();
      let model = params.model;
      if (!model && ctx.model) model = `${ctx.model.provider}/${ctx.model.id}`;
      if (!model) {
        return {
          content: [{ type: "text", text: "resume_agent failed: no active model and no override." }],
          details: { error: "no_model" },
          isError: true,
        };
      }
      const sessionFile = params.sessionFile;
      // Synchronous launch-failure gate: the target session must exist -> else
      // an immediate tool error with no job record.
      if (!fs.existsSync(sessionFile) || !fs.statSync(sessionFile).isFile()) {
        return {
          content: [
            { type: "text", text: `resume_agent failed: session file not found: ${sessionFile}` },
          ],
          details: { error: "session_not_found", sessionFile },
          isError: true,
        };
      }
      // The resumed child restores the session's OWN recorded cwd for its tool
      // operations (verified: launching from a different cwd still ran the
      // subagent's bash in the session's original directory, and appended to the
      // same file). So the launch cwd is immaterial here — a resumed worktree
      // agent stays in its worktree automatically. We launch from ctx.cwd; pi
      // and --session decide the rest. record.sessionFile is set up front, so
      // the fresh-spawn dir-diff discovery is skipped entirely.
      let record: JobRecord;
      try {
        record = launchAgentJob(s, {
          prompt: params.prompt,
          model,
          cwd: ctx.cwd,
          sessionFile,
        });
      } catch (err) {
        return {
          content: [{ type: "text", text: `resume_agent failed: ${(err as Error).message}` }],
          details: { error: "spawn_failed", message: (err as Error).message },
          isError: true,
        };
      }
      s.events.emit("change");
      return {
        content: [{ type: "text", text: JSON.stringify({ jobId: record.jobId, sessionFile }) }],
        details: { jobId: record.jobId, pid: record.pid, sessionFile },
      };
    },
  });

  pi.registerTool({
    name: "kill",
    label: "Kill Job",
    description:
      "Terminate a background job and its descendants (process-group SIGTERM, then SIGKILL after a " +
      "grace). Idempotent: already-finished / unknown jobs return their current state without error. " +
      "Returns the job's current state.",
    parameters: KillParams,
    async execute(_toolCallId, params) {
      const s = requireState();
      const record = s.registry.get(params.jobId);
      if (!record) {
        return {
          content: [{ type: "text", text: JSON.stringify({ jobId: params.jobId, state: "unknown" }) }],
          details: { jobId: params.jobId, state: "unknown" },
        };
      }
      if (record.state === "running") {
        // Dispatch signals without awaiting exit; the close-driven path updates
        // the record. Group-kill takes down the job + its descendants.
        signalGroup(record.pid, "SIGTERM");
        setTimeout(() => {
          if (record.state === "running") signalGroup(record.pid, "SIGKILL");
        }, 500);
      }
      return {
        content: [{ type: "text", text: JSON.stringify(toStatus(record)) }],
        details: toStatus(record),
      };
    },
  });
}
