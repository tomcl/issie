var path = require("path");
var HtmlWebpackPlugin = require('html-webpack-plugin');

function resolve(filePath) {
    return path.join(__dirname, filePath)
}



module.exports = {
    entry: [resolve("src/Renderer/scss/main.css"),resolve("src/Renderer/Renderer.fs.js")],
    resolve: {
        modules: ['node_modules']
    },
    output: {
        filename: "renderer.js"
    },
    plugins: [

    ],
    module: {
        rules: [
            {
                test: /\.js$/,
                enforce: "pre",
                use: ["source-map-loader"],
            }

        ]
    }
}
