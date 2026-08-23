#!/usr/bin/env node
/*
    drive.js

    Drive a running Issie from the terminal, through the window.issieDev harness.

        node scripts/drive.js state                  what the app currently is, as JSON
        node scripts/drive.js refs                   what is holding a simulation, and heap use
        node scripts/drive.js waves                  what the waveform viewer shows, holds and awaits
        node scripts/drive.js wait <expr> [secs]     block until a condition of the app holds
        node scripts/drive.js commands               the commands send accepts
        node scripts/drive.js send <name> [arg]      send one, and wait for the render it causes
        node scripts/drive.js script <file.txt>      a command per line, each awaiting its render

    A script file is one `<name> [arg]` per line; blank lines and # comments are ignored. Each line
    waits for the app to finish rendering before the next is sent, so a sequence needs no sleeps -
    which is the whole point, since guessing how long a render takes is both slow and unreliable.

    A line may also be `wait <expr>`, which blocks until the expression is true of the app. Use it
    for anything a render does not settle: a build that continues after the message that started it,
    a fetch answered by another process, a sidecar that is still starting up. `expr` is JavaScript
    with `state`, `waves`, `refs` and `sidecar` in scope - the four reporters above - so

        wait sidecar.connected
        wait waves.state === 'Success'
        wait waves.missing === 0 && !waves.fetchInProgress

    are all conditions rather than durations. A wait that times out prints what the app actually
    was, which is the thing worth knowing when a sequence goes wrong.

        openSheet main5
        rightTab Simulation
        simSubTab StepSim
        startSimulation

    Issie must be running a debug build (npm run dev / npm run app after a dev compile), since the
    harness is published only when JSHelpers.debugLevel > 0. Set ISSIE_DEBUG_PORT if 9222 is wrong.

    Companion to inspect-canvas.js, which reads the schematic; this one drives the application.
*/

const DEFAULT_PORT = process.env.ISSIE_DEBUG_PORT || '9222';

async function pageTarget(port) {
    let targets;
    try {
        targets = await (await fetch(`http://127.0.0.1:${port}/json/list`)).json();
    } catch (e) {
        throw new Error(
            `nothing is listening on port ${port}.\n` +
            `Start Issie with 'npm run dev', or set ISSIE_DEBUG_PORT if the port was changed.`);
    }
    const page = targets.find(t => t.type === 'page' && t.webSocketDebuggerUrl);
    if (!page) throw new Error(`no renderer window on port ${port}`);
    return page;
}

function connect(url) {
    return new Promise((resolve, reject) => {
        const ws = new WebSocket(url);
        const pending = new Map();
        let nextId = 1;
        ws.onopen = () => resolve({
            send(method, params) {
                const id = nextId++;
                return new Promise((res, rej) => {
                    pending.set(id, { res, rej });
                    ws.send(JSON.stringify({ id, method, params: params || {} }));
                });
            },
            close: () => ws.close()
        });
        ws.onerror = e => reject(new Error('websocket error: ' + (e.message || e.type)));
        ws.onmessage = ev => {
            const msg = JSON.parse(ev.data);
            const entry = msg.id && pending.get(msg.id);
            if (!entry) return;
            pending.delete(msg.id);
            msg.error ? entry.rej(new Error(JSON.stringify(msg.error))) : entry.res(msg.result);
        };
    });
}

/// Evaluate an expression in the page, awaiting a promise if it produces one.
async function evaluate(cdp, expression) {
    const r = await cdp.send('Runtime.evaluate', {
        expression, awaitPromise: true, returnByValue: true
    });
    if (r.exceptionDetails) {
        throw new Error('page threw: ' + (r.exceptionDetails.exception?.description
            || r.exceptionDetails.text));
    }
    return r.result.value;
}

const HARNESS_MISSING =
    'window.issieDev is not published.\n' +
    'It exists only in a debug build - run a dev compile (node scripts/dev.js --once) and restart Issie.';

async function requireHarness(cdp) {
    if (!(await evaluate(cdp, 'typeof window.issieDev === "object"'))) throw new Error(HARNESS_MISSING);
}

/// Send one command and resolve once the render it caused has happened. A command that changes
/// nothing renders nothing, so the wait is bounded rather than indefinite.
function sendExpr(name, arg, timeoutMs = 20000) {
    const n = JSON.stringify(name), a = JSON.stringify(arg || '');
    return `(async () => {
        const rendered = new Promise(res => {
            window.issieDev.onNextRender(res);
            setTimeout(() => res('timeout'), ${timeoutMs});
        });
        const reply = window.issieDev.send(${n}, ${a});
        const how = await rendered;
        return JSON.stringify({ reply, rendered: how !== 'timeout' });
    })()`;
}

/// Evaluate a condition over the four reporters, in the page.
function waitExpr(expr) {
    return `(() => {
        const state = window.issieDev.state();
        const waves = window.issieDev.waveState();
        const refs = window.issieDev.simRefs();
        const sidecar = window.issieDev.sidecar();
        try { return !!(${expr}); } catch (e) { return false; }
    })()`;
}

/// Block until `expr` is true of the app, or the timeout runs out.
///
/// Polled rather than pushed: what is being waited for is usually not a render - a build that
/// continues after the message that started it, a promise answered by another process - so there is
/// no event to hang it on. The poll is cheap (four reporters, all O(1) or O(selected waves)) and the
/// alternative is a sleep long enough to be safe, which is a sleep long enough to be slow.
async function waitFor(cdp, expr, seconds) {
    const deadline = Date.now() + seconds * 1000;
    const started = Date.now();
    for (;;) {
        if (await evaluate(cdp, waitExpr(expr))) return Math.round((Date.now() - started) / 100) / 10;
        if (Date.now() > deadline) {
            const state = await evaluate(cdp, 'window.issieDev.state()');
            const waves = await evaluate(cdp, 'window.issieDev.waveState()');
            const sidecar = await evaluate(cdp, 'window.issieDev.sidecar()');
            throw new Error([
                `waited ${seconds}s for: ${expr}`,
                `state: ${JSON.stringify(state)}`,
                `waves: ${JSON.stringify(waves)}`,
                `sidecar: ${JSON.stringify(sidecar)}`,
            ].join('\n'));
        }
        await new Promise(r => setTimeout(r, 100));
    }
}

function parseScript(text) {
    return text.split(/\r?\n/)
        .map(l => l.replace(/#.*$/, '').trim())
        .filter(l => l)
        .map(l => {
            const i = l.indexOf(' ');
            return i < 0 ? [l, ''] : [l.slice(0, i), l.slice(i + 1).trim()];
        });
}

(async () => {
    const [cmd, ...rest] = process.argv.slice(2);
    if (!cmd) {
        console.error(require('fs').readFileSync(__filename, 'utf8').split('*/')[0].split('/*')[1]);
        process.exit(1);
    }
    const page = await pageTarget(DEFAULT_PORT);
    const cdp = await connect(page.webSocketDebuggerUrl);
    try {
        await requireHarness(cdp);
        switch (cmd) {
            case 'state':
                console.log(JSON.stringify(await evaluate(cdp, 'window.issieDev.state()'), null, 2));
                break;
            case 'refs':
                console.log(JSON.stringify(await evaluate(cdp, 'window.issieDev.simRefs()'), null, 2));
                break;
            case 'waves':
                console.log(JSON.stringify(await evaluate(cdp, 'window.issieDev.waveState()'), null, 2));
                break;
            case 'wait': {
                const seconds = /^\d+$/.test(rest[rest.length - 1]) ? +rest.pop() : 60;
                const expr = rest.join(' ');
                if (!expr) throw new Error("wait needs a condition, e.g. wait \"waves.missing === 0\"");
                console.log(`waited ${await waitFor(cdp, expr, seconds)}s for: ${expr}`);
                break;
            }
            case 'commands':
                console.log((await evaluate(cdp, 'window.issieDev.commands()')).join('\n'));
                break;
            case 'send': {
                const [name, ...argWords] = rest;
                if (!name) throw new Error('send needs a command name; try: drive.js commands');
                const out = JSON.parse(await evaluate(cdp, sendExpr(name, argWords.join(' '))));
                console.log(out.reply + (out.rendered ? '' : '   (no render within the timeout)'));
                break;
            }
            case 'script': {
                const file = rest[0];
                if (!file) throw new Error('script needs a file of commands');
                for (const [name, arg] of parseScript(require('fs').readFileSync(file, 'utf8'))) {
                    if (name === 'wait') {
                        console.log(`wait ${arg}: ${await waitFor(cdp, arg, 60)}s`);
                        continue;
                    }
                    const out = JSON.parse(await evaluate(cdp, sendExpr(name, arg)));
                    console.log(`${name}${arg ? ' ' + arg : ''}: ${out.reply}` +
                        (out.rendered ? '' : '   (no render within the timeout)'));
                }
                break;
            }
            default:
                throw new Error(`unknown command '${cmd}': state | refs | waves | wait | commands | send | script`);
        }
    } finally {
        cdp.close();
    }
})().catch(e => { console.error(e.message); process.exit(1); });
