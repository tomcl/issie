#!/usr/bin/env node
/*
    inspect-canvas.js

    Read what Issie's draw block is showing, from outside the app.

        node scripts/inspect-canvas.js model            what the MODEL says is drawn
        node scripts/inspect-canvas.js geometry         what the SVG actually contains
        node scripts/inspect-canvas.js shot out.png     screenshot the renderer window
        node scripts/inspect-canvas.js eval expr.js     evaluate a file's contents in the page

    Issie must be running under `npm run dev` (or `npm run debug`), which starts Electron with
    --remote-debugging-port. Set ISSIE_DEBUG_PORT to match if it was changed from 9222.

    `model` is the authoritative answer and needs a debug build, since MainView only publishes
    window.issie when JSHelpers.debugLevel > 0. `geometry` reads the DOM instead, so it works
    against any build but describes the rendered output rather than the state behind it -
    useful precisely when the two are suspected of disagreeing.

    No dependencies: Node 22 has a global fetch and WebSocket, which is all the Chrome DevTools
    Protocol needs.
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
    if (!page) throw new Error(`no renderer window on port ${port} (targets: ${targets.map(t => t.type).join(', ') || 'none'})`);
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

async function evaluate(cdp, expression) {
    const r = await cdp.send('Runtime.evaluate', { expression, returnByValue: true, awaitPromise: true });
    if (r.exceptionDetails) {
        const ex = r.exceptionDetails.exception || r.exceptionDetails;
        throw new Error('page threw: ' + (ex.description || ex.value || JSON.stringify(ex)));
    }
    return r.result.value;
}

// What the model says. See ModelHelpers.canvasInspection for the shape of the result.
const MODEL = `(() => {
    if (!window.issie || !window.issie.canvas)
        return { error: 'window.issie is not published - is this a debug build? (JSHelpers.debugLevel > 0)' };
    return window.issie.canvas();
})()`;

// What the SVG contains, in diagram coordinates.
//
// The zoom transform sits on the one <g> wrapping everything, so the drawn items beneath it are
// already in the units the model uses - but do NOT read the bounding box of #DrawBlockSVGTop
// itself, which is outside that transform and so comes back multiplied by the zoom.
//
// How deep the wrapping goes is not fixed (today: svg > g[scale] > g > items), so rather than
// assume a depth this descends through single-child <g>s until it reaches the node that actually
// branches. Its children are one symbol or one wire each.
const GEOMETRY = `(() => {
    const top = document.getElementById('DrawBlockSVGTop');
    if (!top) return { error: 'no #DrawBlockSVGTop - is a sheet open?' };
    const round = n => Math.round(n * 100) / 100;
    const box = el => {
        let b; try { b = el.getBBox(); } catch (e) { return null; }
        return { x: round(b.x), y: round(b.y), w: round(b.width), h: round(b.height) };
    };
    const zoomG = top.querySelector(':scope > g');
    let items = zoomG || top;
    while (items.children.length === 1 && items.children[0].tagName === 'g') items = items.children[0];
    const describe = el => {
        const o = { tag: el.tagName, bbox: box(el) };
        if (el.id) o.id = el.id;
        const cls = el.getAttribute('class'); if (cls) o.class = cls;
        for (const a of ['x','y','width','height','cx','cy','r','x1','y1','x2','y2',
                         'points','d','transform','fill','stroke','stroke-width','text-anchor']) {
            const v = el.getAttribute(a); if (v !== null) o[a] = v;
        }
        if (el.tagName === 'text') o.text = el.textContent;
        return o;
    };
    const canvas = document.getElementById('Canvas');
    return {
        canvasScroll: canvas
            ? { left: canvas.scrollLeft, top: canvas.scrollTop, cw: canvas.clientWidth, ch: canvas.clientHeight }
            : null,
        zoomTransform: zoomG ? getComputedStyle(zoomG).transform : 'none',
        // bounding box of everything drawn, in diagram units
        drawnBBox: box(items),
        groups: [...items.children].map(g => ({
            ...describe(g),
            children: [...g.querySelectorAll('*')].slice(0, 60).map(describe)
        }))
    };
})()`;

(async () => {
    const [, , cmd, arg] = process.argv;
    if (!cmd || ['-h', '--help', 'help'].includes(cmd)) {
        console.log('usage: inspect-canvas.js model | geometry | shot <out.png> | eval <expr.js>');
        return;
    }
    const target = await pageTarget(DEFAULT_PORT);
    const cdp = await connect(target.webSocketDebuggerUrl);
    try {
        switch (cmd) {
            case 'model':
                console.log(JSON.stringify(await evaluate(cdp, MODEL), null, 1));
                break;
            case 'geometry':
                console.log(JSON.stringify(await evaluate(cdp, GEOMETRY), null, 1));
                break;
            case 'shot': {
                if (!arg) throw new Error('shot needs an output file: inspect-canvas.js shot out.png');
                const { data } = await cdp.send('Page.captureScreenshot', { format: 'png' });
                require('fs').writeFileSync(arg, Buffer.from(data, 'base64'));
                console.log('wrote ' + arg);
                break;
            }
            case 'eval': {
                if (!arg) throw new Error('eval needs a file: inspect-canvas.js eval expr.js');
                const src = require('fs').readFileSync(arg, 'utf8');
                console.log(JSON.stringify(await evaluate(cdp, src), null, 1));
                break;
            }
            default:
                throw new Error(`unknown command '${cmd}'`);
        }
    } finally {
        cdp.close();
    }
})().catch(e => { console.error(e.message); process.exit(1); });
