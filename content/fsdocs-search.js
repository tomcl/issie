import Fuse from "https://esm.sh/fuse.js@7.0.0";

const searchBtn = document.querySelector("#search-btn");

function hideSearchBtn() {
    // Hide search icon if we can't search in the first place.
    searchBtn.style.display = 'none';
}

function debounce(mainFunction, delay) {
    // Declare a variable called 'timer' to store the timer ID
    let timer;

    // Return an anonymous function that takes in any number of arguments
    return function (...args) {
        // Clear the previous timer to prevent the execution of 'mainFunction'
        clearTimeout(timer);

        // Set a new timer that will execute 'mainFunction' after the specified delay
        timer = setTimeout(() => {
            mainFunction(...args);
        }, delay);
    };
}

// How many results to put in the DOM. Fuse returns every match, and a common word
// matches thousands of API entries.
const maxResults = 25;

// Fuse is used to find candidates, not to order them. With ignoreLocation and
// ignoreFieldNorm set, any substring hit anywhere in a page scores a perfect 0, so
// nearly every result ties and the order that comes back is arbitrary - which is
// why searching "waveform" used to return twenty F# symbols and no documentation.
// The candidates are therefore re-ranked below on where the term appears and how
// much of the page it accounts for.

// Weights for where a term is found.
const titleWeight = 10;
const headingWeight = 4;
const bodyWeight = 3;
// A phrase, when more than one word was typed, is worth more than its words.
const phraseTitleWeight = 12;
const phraseHeadingWeight = 6;
const phraseBodyWeight = 6;
// Body hits saturate (the 5th mention means much less than the 2nd) and are damped
// by page length, so that the longest page does not win every query.
const saturation = 2;
const lengthScale = 4000;

/// Documentation outranks the API reference, which is 8313 of the 8347 index
/// entries. Developer notes are findable but not promoted; blog posts sit between.
function kindWeight(item) {
    if (item.type !== "content") return 1;
    if (item.uri.includes("/dev/")) return 1;
    if (item.uri.includes("/updates/")) return 1.2;
    return 3;
}

function escapeForRegex(s) {
    return s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

/// Split camel case so that "waveform" finds `initialWaveformColWidth`. Only titles
/// are treated this way: it is where an API entry carries its meaning, and doing it
/// to every page body on every keystroke would not be free.
function splitCamelCase(s) {
    return s.replace(/([a-z0-9])([A-Z])/g, '$1 $2').toLowerCase();
}

function relevance(item, terms, phrase) {
    const title = splitCamelCase(item.title || '');
    const headings = (item.headings || []).join('   ').toLowerCase();
    const body = (item.content || '').toLowerCase();
    // Damp long pages, so a mention in a short focused page counts for more than
    // one in the middle of the longest page on the site.
    const lengthNorm = Math.sqrt(lengthScale / (lengthScale + body.length));

    let score = 0;
    for (const term of terms) {
        if (term.test.test(title)) score += titleWeight;
        if (term.test.test(headings)) score += headingWeight;
        const matches = body.match(term.all);
        const n = matches ? Math.min(matches.length, 32) : 0;
        if (n) score += bodyWeight * (n / (n + saturation)) * lengthNorm;
    }
    if (phrase) {
        if (phrase.test(title)) score += phraseTitleWeight;
        if (phrase.test(headings)) score += phraseHeadingWeight;
        if (phrase.test(body)) score += phraseBodyWeight * lengthNorm;
    }
    return score * kindWeight(item);
}

/// Terms match on a word boundary, so that "ram" does not match "pa*ram*eter".
function compileQuery(query) {
    const words = [...new Set(query.toLowerCase().split(/\s+/).filter(w => w.length >= 2))];
    const terms = words.map(w => ({
        test: new RegExp('\\b' + escapeForRegex(w)),
        all: new RegExp('\\b' + escapeForRegex(w), 'g')
    }));
    const phrase = words.length > 1
        ? new RegExp('\\b' + escapeForRegex(query.toLowerCase().trim()))
        : null;
    return { terms, phrase };
}

const root = document.documentElement.getAttribute("data-root");

if (root && searchBtn) {
    const searchIndexUrl = `${root.replace(/\/$/, '')}/index.json`;

    // The index is several hundred KB gzipped. Fetching it when the module loaded
    // made every page on the site pay for it, whether or not anyone searched.
    // Fetch it the first time the search dialog is opened instead.
    let indexPromise = null;

    function ensureIndex() {
        if (!indexPromise) {
            indexPromise = fetch(searchIndexUrl, {})
                .then(response => response.json())
                .then(index => new Fuse(index, {
                    includeScore: true,
                    keys: ['uri', 'title', 'content', 'headings'],
                    includeMatches: true,
                    ignoreLocation: true,
                    threshold: 0.6,
                    minMatchCharLength: 2,
                    ignoreFieldNorm: true,
                    shouldSort: true
                }))
                .catch(() => {
                    hideSearchBtn();
                    return null;
                });
        }
        return indexPromise;
    }

    const searchDialog = document.querySelector("dialog");
    const empty = document.querySelector("dialog .empty");
    const resultsElement = document.querySelector("dialog ul");
    const searchBox = document.querySelector("dialog input[type=search]");

    function openSearch() {
        ensureIndex();
        searchDialog.showModal();
    }

    searchBtn.addEventListener("click", openSearch)

    searchDialog.addEventListener("click", ev => {
        if (ev.target.tagName === "DIALOG") {
            searchBox.value = '';
            searchDialog.close()
        }
    })

    function rank(results, query) {
        const { terms, phrase } = compileQuery(query);
        const seen = new Set();
        const scored = [];
        for (const result of results) {
            // The same symbol can appear under two namespaces.
            if (seen.has(result.item.uri)) continue;
            seen.add(result.item.uri);
            scored.push({
                item: result.item,
                score: relevance(result.item, terms, phrase),
                fuseScore: result.score
            });
        }
        // Fuse's own order is the tie-break, which is what orders a query that
        // matches no word boundary anywhere - a partial symbol name, say.
        return scored
            .sort((a, b) => b.score - a.score || a.fuseScore - b.fuseScore)
            .slice(0, maxResults);
    }

    async function searchAux(searchTerm) {
        const fuse = await ensureIndex();
        if (!fuse) return;

        // The box may have been cleared or retyped while the index was loading.
        if (searchBox.value !== searchTerm) return;

        const results = rank(fuse.search(searchTerm), searchTerm);
        if (results.length === 0) {
            clearResults();
            empty.textContent = "No results were found";
        } else {
            if (location.hostname === 'localhost') {
                console.table(results);
            }

            empty.style.display = 'none';
            const newResultNodes =
                results
                    .map(result => {
                        const item = result.item;
                        const li = document.createElement("li");
                        const a = document.createElement("a");
                        a.setAttribute("href", item.uri);
                        const icon = document.createElement("iconify-icon");
                        icon.setAttribute("width", "24");
                        icon.setAttribute("height", "24");
                        icon.setAttribute("icon", item.type === "content" ? "iconoir:page" : "bxs:file-doc")
                        // A page title comes from its source heading and can carry
                        // that heading's trailing newline.
                        a.append(icon, item.title.trim());
                        li.appendChild(a);
                        return li;
                    });
            resultsElement.replaceChildren(...newResultNodes);
        }
    }

    const search = debounce(searchAux, 250);

    function clearResults() {
        empty.style.display = 'block';
        resultsElement.replaceChildren();
    }

    function onSearchInput(searchTerm) {
        if (!searchTerm) {
            empty.textContent = "Type something to start searching.";
            clearResults();
        } else {
            search(searchTerm);
        }
    }

    // 'input' rather than 'keyup' alone, so that pasting into the box searches too.
    searchBox.addEventListener('input', ev => {
        ev.stopPropagation();
        onSearchInput(ev.target.value);
    });

    searchBox.addEventListener('keyup', ev => {
        ev.stopPropagation();
    });

    window.addEventListener('keyup', ev => {
        if (ev.key === 'Escape' && searchDialog.open) {
            searchDialog.close();
        }

        if (ev.key === '/' && !searchDialog.open) {
            openSearch();
        }
    })
} else {
    hideSearchBtn();
}
