// Runs before the page paints, so that a reader who wants the dark theme never
// sees a white flash first.
//
// The media query passed to matchMedia is the query alone: an "@media" prefix
// makes it unparseable, matches is then always false, and the system preference
// is silently ignored - which is what used to happen here.
const prefersDark = window.matchMedia("(prefers-color-scheme: dark)").matches;
let currentTheme = localStorage.getItem('theme') ?? (prefersDark ? 'dark' : 'light');
if (currentTheme === 'dark') {
    window.document.documentElement.setAttribute("data-theme", 'dark');
}
