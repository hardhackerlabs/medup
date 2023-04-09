document.addEventListener("DOMContentLoaded", function () {
    var codeBlocks = document.querySelectorAll("pre code");

    for (var i = 0; i < codeBlocks.length; i++) {
        hljs.highlightBlock(codeBlocks[i]);
    }
});
