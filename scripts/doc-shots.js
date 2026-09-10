#!/usr/bin/env node
/*
    doc-shots.js

    Regenerate the screenshots the documentation uses, from a running Issie.

        node scripts/doc-shots.js                list the shots
        node scripts/doc-shots.js all            take all of them
        node scripts/doc-shots.js custom         take one, by name

    Issie must be running a debug build with the remote debugging port open:

        npm run app -- -d

    and the projects the shots are taken from must exist. They are written as data rather than
    drawn by hand - see docs/dev/sheetDescriptionDsl.md and scripts/doc-shot-projects.fsx.

    Why this exists rather than a person pressing PrtScn:

    - **Resolution.** A screenshot is displayed in the documentation about 800 CSS px wide. Taken
      from a maximised window at 1700-1900 CSS px, Issie's 11px UI text lands at under 5px there
      and cannot be read - which is what was wrong with the tutorial's pictures. The window is
      therefore driven to a narrow viewport, so the UI occupies proportionally more of the frame,
      at twice the device density so the result is still sharp. That has to be a viewport
      override rather than a resize of the real window: Electron implements neither the CDP
      Browser domain nor window.resizeTo.
    - **Sequencing.** The override lasts only as long as the connection that sets it, so the
      resize, the driving and the capture must all happen on one connection - which is why this
      is a script and not three calls to drive.js and inspect-canvas.js.
    - **Fitting.** `zoomToFit` fits the circuit to the window as it is at that moment, so it has
      to be sent after the viewport is set, not before.

    Not every picture is here. select2 - the Select Waves dialog filtered to one sheet by its
    breadcrumb - is not: the breadcrumb is neither a button nor anything the harness can name, and
    clicking it by its text hit something else, which produced a picture identical to select1.

    The truth-table ones are not either, and were tried: the right-hand pane
    is fixed at 650 CSS px - only its divider changes that - and at that width a truth table's
    column headers are drawn over their own sort arrows, so the result is worse than the picture
    it would replace. Widening the pane means dragging the divider, which nothing here can do yet.

    No dependencies: Node's global fetch and WebSocket are all the DevTools Protocol needs.
*/

const fs = require('fs');
const path = require('path');

const PORT = process.env.ISSIE_DEBUG_PORT || '9222';
const REPO = path.resolve(__dirname, '..');
const PROJECTS = path.join(REPO, 'tmp', 'docShots');
const OUT = path.join(REPO, 'docs', 'img', 'userGuide');
const HOME = path.join(REPO, 'docs', 'img', 'homePage');

/// For a dialog, which covers the whole window: a wider window lays it out better. 1.5x rather
/// than 2x because these are dense text UIs and 2x put a single picture over a third of a
/// megabyte - the documentation is displayed at about 800px, so 1.5x is still more than enough.
const WIDE = { width: 1700, height: 1000, scale: 1.5 };

/// For the right-hand pane. Its width is fixed at 650px whatever the window does - it is what is
/// left after the canvas, and only the divider changes it - so widening the window is pointless
/// here and only the density is turned down.
const PANE = { width: 1280, height: 880, scale: 1.5 };

// Issie says of itself that its UI degrades below 1150 CSS px and is best above 1250, so the
// viewport is not made small enough to fix the legibility problem on its own. What fixes it is
// clipping each shot to the part of the window it is about - the canvas, or one dialog - which
// is both narrower than the window and the only part worth showing. Twice the density, so the
// reduction to page width has pixels to work with.
const VIEW = { width: 1280, height: 880, scale: 2 };

// ---------------------------------------------------------------- CDP plumbing

async function pageTarget() {
    let targets;
    try {
        targets = await (await fetch(`http://127.0.0.1:${PORT}/json/list`)).json();
    } catch (e) {
        throw new Error(`nothing is listening on port ${PORT}.\n`
            + `Start Issie with 'npm run app -- -d'.`);
    }
    const page = targets.find(t => t.type === 'page' && t.webSocketDebuggerUrl);
    if (!page) throw new Error(`no renderer window on port ${PORT}`);
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
            if (entry) {
                pending.delete(msg.id);
                msg.error ? entry.rej(new Error(JSON.stringify(msg.error))) : entry.res(msg.result);
            }
        };
    });
}

// ---------------------------------------------------------------- the app, as an object

function app(cdp) {
    const evaluate = async (expression) => {
        const r = await cdp.send('Runtime.evaluate',
                                 { expression, returnByValue: true, awaitPromise: true });
        if (r.exceptionDetails) {
            const ex = r.exceptionDetails.exception || r.exceptionDetails;
            throw new Error('page threw: ' + (ex.description || ex.value || JSON.stringify(ex)));
        }
        return r.result.value;
    };

    const settle = () =>
        evaluate('new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r)))');

    return {
        evaluate,
        settle,

        /// One harness command, waiting for the render it causes - the same contract drive.js has.
        async send(name, arg) {
            const a = arg === undefined ? 'undefined' : JSON.stringify(String(arg));
            return evaluate(`(async () => {
                const rendered = new Promise(res => window.issieDev.onNextRender(res));
                const reply = window.issieDev.send(${JSON.stringify(name)}, ${a});
                await Promise.race([rendered, new Promise(r => setTimeout(r, 4000))]);
                return reply;
            })()`);
        },

        /// Block until a condition of the app holds, as drive.js wait does.
        async wait(expr, secs = 30) {
            const deadline = Date.now() + secs * 1000;
            for (;;) {
                const ok = await evaluate(`(() => {
                    const state = window.issieDev.state();
                    const waves = window.issieDev.waveState();
                    const refs = window.issieDev.simRefs();
                    const sidecar = window.issieDev.sidecar();
                    try { return !!(${expr}); } catch (e) { return false; }
                })()`);
                if (ok) return;
                if (Date.now() > deadline) {
                    const state = await evaluate('JSON.stringify(window.issieDev.state())');
                    throw new Error(`waited ${secs}s for: ${expr}\nstate: ${state}`);
                }
                await new Promise(r => setTimeout(r, 200));
            }
        },

        /// Click something the harness has no command for. Used sparingly, and always on a
        /// button found by its own text rather than by position.
        async clickText(text, tag = 'button') {
            const found = await evaluate(`(() => {
                const wanted = ${JSON.stringify(text)}.toLowerCase();
                const els = [...document.querySelectorAll(${JSON.stringify(tag)})];
                const el = els.find(e => (e.textContent || '').trim().toLowerCase() === wanted)
                        || els.find(e => (e.textContent || '').trim().toLowerCase().includes(wanted));
                if (!el) return false;
                el.click();
                return true;
            })()`);
            if (!found) throw new Error(`no ${tag} whose text is "${text}"`);
            await settle();
        },

        /// Capture the window, or just the element `clip` names. A clipped shot is what makes the
        /// UI readable once the picture is scaled to page width: the canvas alone is about two
        /// thirds of the window, so its text arrives about half as reduced.
        async capture(file, clip, maxHeight, view) {
            const scale = (view || VIEW).scale;
            const params = { format: 'png' };
            if (clip) {
                // Every box is clamped to the canvas pane and to the window: #Canvas is a scroll
                // container far larger than the window, and the drawn circuit can extend past
                // what is on screen.
                const box = await evaluate(`(() => {
                    const pane = document.getElementById('Canvas');
                    const margin = 24;
                    let r;
                    if (${JSON.stringify(clip)} === 'drawn') {
                        const top = document.getElementById('DrawBlockSVGTop');
                        if (!top || !pane) return null;
                        // Descend through the single-child <g>s the zoom transform sits on until
                        // the node that actually branches - one child per symbol or wire.
                        let items = top.querySelector(':scope > g') || top;
                        while (items.children.length === 1 && items.children[0].tagName === 'g') {
                            items = items.children[0];
                        }
                        const b = items.getBoundingClientRect();
                        r = { left: b.left - margin, top: b.top - margin,
                              right: b.right + margin, bottom: b.bottom + margin };
                    } else {
                        const el = document.querySelector(${JSON.stringify(clip)});
                        if (!el) return null;
                        r = el.getBoundingClientRect();
                    }
                    // The drawn circuit is clamped to the canvas pane as well as to the window,
                    // since it scrolls under a pane narrower than both. Anything named by a
                    // selector is clamped only to the window: a dialog covers the whole of it,
                    // and intersecting that with the canvas pane leaves nothing.
                    const limit =
                        ${JSON.stringify(clip)} === 'drawn' && pane
                            ? pane.getBoundingClientRect()
                            : { left: 0, top: 0, right: window.innerWidth, bottom: window.innerHeight };
                    const x = Math.max(r.left, limit.left, 0);
                    const y = Math.max(r.top, limit.top, 0);
                    const right = Math.min(r.right, limit.right, window.innerWidth);
                    const bottom = Math.min(r.bottom, limit.bottom, window.innerHeight);
                    return { x, y, width: right - x, height: bottom - y };
                })()`);
                if (!box) throw new Error(`nothing matches ${clip} - cannot clip the shot`);
                if (box.width < 50 || box.height < 50) {
                    throw new Error(`${clip} is not visible - clipped to ${box.width}x${box.height}`);
                }
                // A pane can be much taller than the part of it worth showing - the wave viewer
                // fills the window height whatever is in it - so a shot may cap its own height.
                if (maxHeight) box.height = Math.min(box.height, maxHeight);
                // scale 1, NOT the view's: a clip's scale multiplies on top of the
                // device scale factor the viewport is already rendering at, so passing
                // the density here again captured everything at its square - 4x for a
                // schematic, which is where the oversized files came from.
                params.clip = { ...box, scale: 1 };
            }
            const { data } = await cdp.send('Page.captureScreenshot', params);
            fs.mkdirSync(path.dirname(file), { recursive: true });
            fs.writeFileSync(file, Buffer.from(data, 'base64'));
            const size = clip
                ? `${Math.round(params.clip.width * scale)}x${Math.round(params.clip.height * scale)}`
                : `${(view || VIEW).width * scale}x${(view || VIEW).height * scale}`;
            return `${path.relative(REPO, file)}  ${size}`;
        }
    };
}

// ---------------------------------------------------------------- the shots

/// Open a project and wait for it. MenuHelpers.openProjectFromPath is wrapped in warnAppWidth, so
/// below about 1600 CSS px it raises the window size warning FIRST and only loads the project when
/// that is dismissed - every time, not just once. The viewport used here is narrower than that, so
/// the warning is part of opening a project rather than a one-off at startup.
const openProject = async (a, project) => {
    const dir = path.join(PROJECTS, project);
    await a.send('openProject', dir);
    await a.clickText('Continue').catch(() => {});
    // By the folder name rather than the whole path, which would mean matching Windows
    // separators through two layers of escaping. "tutorial" does not match "tutorialClocked".
    await a.wait(`state.project.toLowerCase().endsWith(${JSON.stringify(project.toLowerCase())})`, 30);
};

/// Open a sheet and fit it to the window. Loading a sheet takes more renders than the one `send`
/// waits for, so the sheet has to be waited for by name: without that, zoomToFit and the capture
/// both act on the sheet that was open before.
const openSheet = async (a, sheet) => {
    // Back to the Catalogue first: with the simulation pane open the canvas is much narrower, so
    // a schematic shot taken after a wave-simulation shot came out at half the size.
    await a.send('rightTab', 'Catalogue');
    await a.send('openSheet', sheet);
    await a.wait(`state.openSheet === ${JSON.stringify(sheet)}`, 30);
    await a.settle();
    await a.send('zoomToFit');
    await a.settle();
};

const openMain = (project) => async (a) => {
    await openProject(a, project);
    await openSheet(a, 'main');
};

/// Open a project's top sheet and get its waveform simulation running with the three signals the
/// tutorial asks for showing. Waves are chosen by the label of the component they come from, so
/// this says what it means rather than indexing into a list whose order falls out of the design.
const waveSim = async (a, project) => {
    await openMain(project)(a);
    await a.send('rightTab', 'Simulation');
    await a.send('simSubTab', 'WaveSim');
    await a.send('startWaveSim');
    // A design that will not simulate leaves the viewer with nothing to draw, and the shot would
    // silently be of an error popup. Fail here instead, with the reason.
    await a.wait(`waves.state === 'Success'`, 60).catch(async () => {
        const w = await a.evaluate('JSON.stringify(window.issieDev.waveState())');
        throw new Error(`the wave simulation of ${project} did not start: ` + w);
    });
    await a.send('waveSelect', 'CNT1,AROM1,DECODER1');
    // Ten cycles rather than the four that fit by default, so the ROM's contents can be seen
    // changing, and the cursor a little way in so the value column is showing a real reading.
    await a.send('waveView', '0 10');
    await a.send('waveCursor', '3');
    await a.wait(`waves.missing === 0 && !waves.fetchInProgress`, 90);
    await a.settle();
};

/// The truth table of the decoder, which is what both truth-table pictures are of: the tutorial
/// selects the DECODER1 instance and asks for a table of the selected logic.
const truthTableOfDecoder = async (a) => {
    await openMain('tutorial')(a);
    await a.send('rightTab', 'Simulation');
    await a.send('simSubTab', 'TruthTable');
    await a.send('select', 'DECODER1');
    await a.send('truthTable');
    await a.wait(`state.truthTableOpen`, 30);
    await a.settle();
};

/// The right-hand pane: the wave viewer, the truth table, whichever tab is showing.
const RIGHT_PANE = '#RightSection';

/// One dialog. Bulma's modal card, scoped to the ACTIVE modal: the wave simulator renders its
/// three dialogs all the time and shows one, so an unscoped .modal-card finds a hidden one and
/// the shot clips to nothing.
const MODAL = '.modal.is-active .modal-card';

/// Clip to what is actually drawn on the sheet rather than to the canvas pane. A circuit is
/// usually much wider than it is tall, so fitting it to a tall pane leaves most of the picture
/// empty - and the empty part is what pushes the interesting part down to an unreadable size once
/// the image is scaled to page width.
const DRAWN = 'drawn';

const SHOTS = [
    {
        name: 'features1',
        file: 'features1.png',
        clip: DRAWN,
        what: 'the decoder logic on its own sheet, before it becomes a custom component',
        run: async (a) => {
            await openProject(a, 'tutorial');
            await openSheet(a, 'decoder');
        }
    },
    {
        name: 'custom',
        file: 'custom.png',
        clip: DRAWN,
        what: 'the main sheet: ROM, split chain and the decoder as a custom component',
        run: openMain('tutorial')
    },
    {
        name: 'waveform',
        file: 'waveform.png',
        clip: DRAWN,
        what: 'the same design made clocked, with a counter driving the ROM address',
        run: openMain('tutorialClocked')
    },
    {
        name: 'select1',
        view: WIDE,
        clipHeight: 780,
        file: 'select1.png',
        clip: MODAL,
        what: 'the Select Waves dialog, showing the design hierarchy',
        run: async (a) => {
            await waveSim(a, 'tutorialClocked');
            await a.send('waveModal', 'on');
            await a.settle();
        }
    },
    {
        name: 'waveform1',
        view: PANE,
        file: 'waveform1.png',
        clip: RIGHT_PANE,
        clipHeight: 540,
        what: 'the waveform viewer showing the counter, the ROM address and the decoder output',
        run: async (a) => {
            await waveSim(a, 'tutorialClocked');
        }
    },
    {
        name: 'waveform2',
        view: PANE,
        file: 'waveform2.png',
        clip: RIGHT_PANE,
        clipHeight: 540,
        what: 'the same waveforms after a register is added between the counter and the ROM',
        run: async (a) => {
            await waveSim(a, 'tutorialClockedReg');
        }
    },
    // ---- pictures on the features page, which live in a different folder ----
    {
        name: 'catalogue',
        file: 'catalogue.png',
        dir: 'homePage',
        what: 'the Catalogue pane beside a schematic',
        run: async (a) => {
            await openProject(a, 'showcase');
            await openSheet(a, 'sequencer');
        }
    },
];

// ---------------------------------------------------------------- main

(async () => {
    const wanted = process.argv.slice(2);
    if (wanted.length === 0) {
        console.log('shots (pass a name, or "all"):\n');
        for (const s of SHOTS) console.log(`  ${s.name.padEnd(12)} ${s.file.padEnd(16)} ${s.what}`);
        console.log(`\nprojects are read from ${path.relative(REPO, PROJECTS)}`);
        return;
    }
    const chosen = wanted.includes('all') ? SHOTS : SHOTS.filter(s => wanted.includes(s.name));
    const unknown = wanted.filter(w => w !== 'all' && !SHOTS.some(s => s.name === w));
    if (unknown.length) throw new Error('no such shot: ' + unknown.join(', '));

    if (!fs.existsSync(PROJECTS)) {
        throw new Error(`no projects at ${PROJECTS}\n`
            + `Generate them first: dotnet fsi scripts/doc-shot-projects.fsx`);
    }

    const target = await pageTarget();
    const cdp = await connect(target.webSocketDebuggerUrl);
    const a = app(cdp);
    try {
        if (!(await a.evaluate('typeof window.issieDev === "object"'))) {
            throw new Error('window.issieDev is not published - Issie must be a debug build,'
                            + ' started with: npm run app -- -d');
        }
        const setView = async (view) => {
            await cdp.send('Emulation.setDeviceMetricsOverride', {
                width: view.width, height: view.height,
                deviceScaleFactor: view.scale, mobile: false
            });
            await a.settle();
        };
        await setView(VIEW);
        // Issie warns about its window size on startup, and the warning sits over everything.
        await a.clickText('Continue').catch(() => {});
        for (const shot of chosen) {
            // Every shot runs against the same application, so each starts from a known state:
            // without this, a shot that opens a dialog left it open over the next one's picture.
            // A shot may ask for a wider window: the right-hand pane is what is left after the
            // canvas, so at the default 1280 it is 650px - below the 1150 Issie says its own UI
            // degrades at, and a truth table's column headers collide there.
            await setView(shot.view || VIEW);
            await a.send('waveModal', 'off');
            await a.send('ramModal', 'off');
            await a.send('select', '');
            await a.settle();
            await shot.run(a);
            await a.settle();
            const dir = shot.dir === 'homePage' ? HOME : OUT;
            console.log('wrote ' + await a.capture(path.join(dir, shot.file), shot.clip, shot.clipHeight, shot.view));
        }
    } finally {
        // An override left in place would affect whatever is done in this window next.
        await cdp.send('Emulation.clearDeviceMetricsOverride').catch(() => {});
        cdp.close();
    }
})().catch(e => { console.error(e.message); process.exit(1); });
