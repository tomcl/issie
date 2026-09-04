// Fail a release only for a security advisory in something users actually run.
//
// WHY NOT PLAIN `npm audit`. Most of this project's dependency tree is build tooling - webpack,
// the dev server, electron-builder - and none of it reaches a user: package.json's build.files
// excludes node_modules apart from `usb`, and webpack bundles the rest into build/. An advisory in
// the dev server is worth fixing on a developer's machine and is not a reason to block a release,
// which is what it did: v6.3.0 failed to publish over four moderate and one high advisory, all of
// them in packages that never ship.
//
// WHY NOT PLAIN `npm audit --omit=dev`. That is the shipped set almost exactly, and misses the one
// thing it can least afford to: `electron` is a devDependency, because electron-builder requires it
// to be, and it is the runtime every user runs. `npm ls electron --omit=dev` reports nothing, so an
// Electron advisory would pass a --omit=dev gate silently.
//
// So: the production tree, plus electron.

const { execSync } = require("child_process");

/// npm audit exits non-zero when it finds something, so the output is what matters rather than the
/// status; a genuine failure to run (no network, bad JSON) is told apart by there being no report.
///
/// execSync rather than execFileSync: npm is npm.cmd on Windows, and since Node 18.20 a .cmd cannot
/// be spawned without a shell - execFileSync throws EINVAL and hands back no output at all.
function audit(extraArgs) {
    let stdout;
    try {
        stdout = execSync(`npm audit --json ${extraArgs.join(" ")}`.trim(), {
            encoding: "utf8",
            maxBuffer: 64 * 1024 * 1024,
            stdio: ["ignore", "pipe", "pipe"],
        });
    } catch (err) {
        stdout = err.stdout;
    }
    if (!stdout) throw new Error(`npm audit ${extraArgs.join(" ")} produced no output`);
    const report = JSON.parse(stdout);
    if (report.error) {
        throw new Error(`npm audit failed: ${report.error.summary || report.error.code}`);
    }
    return report.vulnerabilities || {};
}

function describe(name, v) {
    return `  ${v.severity.padEnd(8)} ${name} ${v.range || ""}`.trimEnd();
}

const shipped = audit(["--omit=dev"]);
// electron is not in the production tree, so it is asked for separately
const everything = audit([]);
const electron = everything.electron ? { electron: everything.electron } : {};

const failing = { ...shipped, ...electron };
const names = Object.keys(failing);

if (names.length === 0) {
    const ignored = Object.keys(everything).length;
    console.log(
        `No security advisories in what ships` +
            (ignored ? ` (${ignored} in build-only dependencies, not release-blocking)` : "")
    );
    process.exit(0);
}

console.error("Security advisories in dependencies that reach users:\n");
for (const name of names) console.error(describe(name, failing[name]));
console.error("\nThese ship - in the webpack bundle, as `usb`, or as electron itself - so they block a release.");
console.error("Run `npm audit` to see the whole tree, build tooling included.");
process.exit(1);
