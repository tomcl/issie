document.addEventListener("DOMNodeInserted", function (event) {
    if (!!window && !(!!window.$)) {
        window.$ = window.jQuery = require('jQuery');
    }
});
