var path = require("path");

function resolve(filePath) {
    return path.join(__dirname, filePath)
}

module.exports = {
    entry: resolve("src/Main/Main.fsproj"),
    externals: {
        bufferutil: 'commonjs bufferutil',
        'utf-8-validate': 'commonjs utf-8-validate',
    },
    module: {
        rules: [
            {
                test: /\.fs(x|proj)?$/,
                use: {
                    loader: "fable-loader"
                }
            }
        ]
    }
}