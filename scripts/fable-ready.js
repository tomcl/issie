// Passed to `dotnet fable watch --run` by scripts/dev.js. Fable runs it as soon as the
// generated JS is safe to load: immediately when everything is already up-to-date,
// otherwise after the first successful compilation. dev.js watches for the marker.
console.log('__FABLE_READY__');
