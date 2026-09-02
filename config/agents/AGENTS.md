# Shared Agent Configuration

The user is Abhay Saxena, a software engineer with over 25 years of experience.

- He always wants a newline at the end of every file he might ever see in his terminal.
- In conversations he prefers to see project-relative paths over absolute paths when they refer to project files and directories.

## Discussion vs. Action Protocol

The user commits to using **clear imperative phrasing** when they want the agent to *change* anything (files, code, environment, external systems, etc).

**If the user uses imperative phrasing:** Act on it.
You may explore and also perform changes.

- "Please commit these changes"
- "Run the tests"
- "Add a comment explaining X"
- "Refactor this function to..."
- "Let's remove this file"
- "Go ahead and implement that"

**If the user does *not* use imperative phrasing:** Discuss only, don't change anything.
You may inspect, analyze, search, and reason freely, but you must not perform any actions that modify code, data, or systems.

**During discussion mode: Keep responses in chat.**
Do not create text files, markdown files, or any other files to hold your analysis, explanations, or answers. Discussion responses belong in the chat interface. Only create or modify files when the user explicitly requests an action using imperative phrasing.

**Agreement is not an action request.**
If the user says they like or agree with a suggestion (e.g., "I like that revision", "That looks good"), do not treat this as permission to change anything. Only act when the user uses clear imperative phrasing to request a change.

Examples of discuss-only prompts:
- "Why does this work?"
- "Can we change X?"
- "What if we tried Y?"
- "This looks wrong"
- "I like that plan"

**When in doubt:** Treat it as discussion-only, and ask before changing anything.
If it's unclear whether the user wants discussion or actual changes, assume the user only wants to talk through options and understanding, and ask:

- "Do you want me to make this change, or continue discussing?"

The user welcomes these clarifying questions.

## Investigation & Problem-Solving

When there's observable reality to inspect: **gather evidence before theorizing**. Check configuration, read code, trace execution, examine logs. If you catch yourself guessing ("probably", "should be", "it's likely"), stop and verify instead.

## Writing Implementation Plans

**Goals are durable, orders are brittle.** Trust and delegate over command and control.

When creating plans for other agents, convey intent, nuance, and how to verify—not prescriptive steps. Link to examples in the codebase rather than writing implementations. Target ~200-300 lines.

## Tooling Preferences

Prefer these tools over their "standard" counterparts when available:

- Search: `rg`, `git grep`, `ast-grep` / `sg` (over `find`, `grep`, `sed`, `awk`)
- File listing: `fd` or `python3` with `pathlib.Path.glob` (over `find`)
- Structured data: `jq`, `yq`
- Scope: root every search at a named subtree — never `/`, `$HOME`, or `~`, even with a depth limit. If a bounded search comes up empty, ask the toolchain where the file lives (`which`, `python3 -c 'import x; print(x.__file__)'`) rather than widening to the root; ask the user if that fails.

For scripting and non-trivial processing, prefer a small `python3` or `bun` script over complex shell pipelines. When in doubt, write a short script instead of an opaque one-liner.

## Git

When asked to suggest a commit message, honor any commit-format requirement in the project's guidance; otherwise determine the author's style by running `git log --author="$(git config user.name)" --format="%s" -20` and match their actual patterns.

Each commit represents one logical change. When asked to commit, only stage the files that are part of that change. Use `git add <specific-files>` for the files you actually changed yourself. Never use `git add -A`.

When using `git rebase`, be careful not to invoke the user's editor.

Never execute `git push` under any circumstances.

## Test Output Handling

When running test suites (pytest, mvn test, npm test, cargo test, etc.), always pipe output to a log file rather than directly to head/tail/grep/other filtering tools. If you need to filter output while running, use tee to both save the full output and filter it. This ensures that when tests fail, the complete output is available for analysis without needing to re-run tests.

Examples:
- `mvn test > /tmp/test.log 2>&1`
- `pytest > /tmp/test.log 2>&1`
- `mvn test 2>&1 | tee /tmp/test.log | grep -A 5 'FAILED'` (to save AND filter)

---

@~/.agents/AGENTS.md
