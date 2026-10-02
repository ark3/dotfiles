/**
 * Auto-read AGENTS.md References Extension
 *
 * Automatically follows @file.md references found in AGENTS.md files
 * and includes their content in the system prompt.
 *
 * When AGENTS.md contains lines like:
 *   See @../saint-agents/AGENTS.md for SAINT system overview...
 *   See @OVERVIEW.md for more information...
 *
 * This extension will:
 * 1. Detect those @ references
 * 2. Resolve the paths relative to the AGENTS.md location
 * 3. Read the referenced files
 * 4. Inject their content into the system prompt
 */

import * as fs from "node:fs";
import * as path from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

interface ReferencedFile {
        sourcePath: string; // The AGENTS.md file that referenced it
        targetPath: string; // The actual file path to read
        reference: string; // The original @reference text
}

/**
 * Find all AGENTS.md (or CLAUDE.md) files by walking up from cwd
 */
function findAgentFiles(cwd: string): string[] {
        const files: string[] = [];
        let currentDir = cwd;

        // Walk up from cwd
        while (true) {
                const agentsPath = path.join(currentDir, "AGENTS.md");
                const claudePath = path.join(currentDir, "CLAUDE.md");

                if (fs.existsSync(agentsPath)) {
                        files.push(agentsPath);
                } else if (fs.existsSync(claudePath)) {
                        files.push(claudePath);
                }

                const parentDir = path.dirname(currentDir);
                if (parentDir === currentDir) break; // Reached root
                currentDir = parentDir;
        }

        // Also check global location
        const homeDir = process.env.HOME || process.env.USERPROFILE;
        if (homeDir) {
                const globalAgents = path.join(homeDir, ".pi", "agent", "AGENTS.md");
                const globalClaude = path.join(homeDir, ".pi", "agent", "CLAUDE.md");
                if (fs.existsSync(globalAgents)) {
                        files.push(globalAgents);
                } else if (fs.existsSync(globalClaude)) {
                        files.push(globalClaude);
                }
        }

        return files;
}

/**
 * Extract @ references from a file's content
 * Matches patterns like: @../path/to/file.md or @OVERVIEW.md
 */
function extractReferences(content: string, sourcePath: string): ReferencedFile[] {
        const references: ReferencedFile[] = [];
        const sourceDir = path.dirname(sourcePath);

        // Match @path references (letters, numbers, dots, slashes, dashes, underscores, tilde)
        const pattern = /@([\w\-.~/]+\.md)/gi;
        let match;

        while ((match = pattern.exec(content)) !== null) {
                const originalReference = match[1];
                let reference = originalReference;

                // Expand tilde to home directory
                if (reference.startsWith('~')) {
                        const homeDir = process.env.HOME || process.env.USERPROFILE || '';
                        reference = reference.replace(/^~/, homeDir);
                }

                // Resolve relative to the source file's directory
                const targetPath = path.resolve(sourceDir, reference);

                references.push({
                        sourcePath,
                        targetPath,
                        reference: `@${originalReference}`,
                });
        }

        return references;
}

/**
 * Read referenced files and return their content with context
 */
function readReferencedFiles(refs: ReferencedFile[]): Map<string, string> {
        const contents = new Map<string, string>();

        for (const ref of refs) {
                // Skip if already read (deduplication)
                if (contents.has(ref.targetPath)) {
                        continue;
                }

                try {
                        if (fs.existsSync(ref.targetPath)) {
                                const content = fs.readFileSync(ref.targetPath, "utf-8");
                                contents.set(ref.targetPath, content);
                        }
                } catch (error: any) {
                        // Silently skip files we can't read
                        console.warn(`[auto-read-agents-refs] Could not read ${ref.targetPath}: ${error?.message || String(error)}`);
                }
        }

        return contents;
}

export default function autoReadAgentsRefsExtension(pi: ExtensionAPI) {
        let referencedContent: Map<string, string> = new Map();
        let discoveredRefs: ReferencedFile[] = [];

        // Scan for references on session start
        pi.on("session_start", async (_event, ctx) => {
                const agentFiles = findAgentFiles(ctx.cwd);

                if (agentFiles.length === 0) {
                        return;
                }

                const allRefs: ReferencedFile[] = [];
                const processedPaths = new Set<string>();  // Track what we've scanned
                const toProcess = [...agentFiles];         // Stack of files to scan
                let totalBytes = 0;
                const MAX_BYTES = 1 * 1024 * 1024;  // 1MB limit

                // Iteratively process files until no new references found (depth-first)
                while (toProcess.length > 0) {
                        const filePath = toProcess.pop()!;

                        // Skip if already processed (avoid cycles)
                        if (processedPaths.has(filePath)) continue;
                        processedPaths.add(filePath);

                        try {
                                const content = fs.readFileSync(filePath, "utf-8");
                                const contentBytes = Buffer.byteLength(content, "utf-8");
                                totalBytes += contentBytes;

                                // Check size limit
                                if (totalBytes > MAX_BYTES) {
                                        console.warn(
                                                `[auto-read-agents-refs] Size limit reached (${MAX_BYTES} bytes), ` +
                                                `skipping remaining references after ${filePath}`
                                        );
                                        break;  // Stop processing more files
                                }

                                const refs = extractReferences(content, filePath);

                                // Add new references to our list
                                for (const ref of refs) {
                                        allRefs.push(ref);
                                        // Queue the target file for scanning too
                                        if (!processedPaths.has(ref.targetPath)) {
                                                toProcess.push(ref.targetPath);
                                        }
                                }
                        } catch (error: any) {
                                console.warn(`[auto-read-agents-refs] Could not read ${filePath}: ${error?.message || String(error)}`);
                        }
                }

                discoveredRefs = allRefs;

                // Read all referenced files
                referencedContent = readReferencedFiles(allRefs);

                // Count how many discovered refs are missing
                const uniqueMissingCount = new Set(
                        allRefs
                                .filter(ref => !referencedContent.has(ref.targetPath))
                                .map(ref => ref.targetPath)
                ).size;

                if (referencedContent.size > 0) {
                        let message = `Auto-loaded ${referencedContent.size} file(s) from @references in AGENTS.md`;
                        if (uniqueMissingCount > 0) {
                                message += ` (${uniqueMissingCount} missing)`;
                        }
                        ctx.ui.notify(message, "info");
                }
        });

        // Inject referenced content into system prompt
        pi.on("before_agent_start", async (event) => {
                if (referencedContent.size === 0) {
                        return;
                }

                // Build additional context from referenced files
                const sections: string[] = [];

                for (const [filePath, content] of referencedContent.entries()) {
                        const relativePath = path.relative(process.cwd(), filePath);
                        sections.push(`## Auto-loaded from ${relativePath}\n\n${content}`);
                }

                const additionalContext = sections.join("\n\n---\n\n");

                return {
                        systemPrompt: event.systemPrompt + "\n\n" + additionalContext,
                };
        });

        // Add a command to list discovered references
        pi.registerCommand("refs", {
                description: "Show auto-discovered @references from AGENTS.md",
                handler: async (_args, ctx) => {
                        if (discoveredRefs.length === 0) {
                                ctx.ui.notify("No @references found in AGENTS.md files", "info");
                                return;
                        }

                        const lines = ["Discovered @references:", ""];
                        for (const ref of discoveredRefs) {
                                const status = referencedContent.has(ref.targetPath) ? "✓" : "✗";
                                const relSource = path.relative(process.cwd(), ref.sourcePath);
                                const relTarget = path.relative(process.cwd(), ref.targetPath);
                                lines.push(`${status} ${ref.reference} (${relSource} → ${relTarget})`);
                        }

                        ctx.ui.notify(lines.join("\n"), "info");
                },
        });
}
