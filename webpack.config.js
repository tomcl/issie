var path = require("path");
var webpack = require("webpack");
var fableUtils = require("fable-utils");

function resolve(filePath) {
  return path.join(__dirname, filePath)
}

var babelOptions = fableUtils.resolveBabelOptions({
  presets: [ [ "es2015", { "modules": false } ] ],
  plugins: ["transform-runtime"]
});

var isProduction = process.argv.indexOf("-p") >= 0;
console.log("Bundling for " + (isProduction ? "production" : "development") + "...");

var basicConfig = {
  mode: isProduction ? 'production' : 'development',
  devtool: "source-map",
  resolve: {
    modules: [resolve("./node_modules/")]
  },
  node: {
    __dirname: false,
    __filename: false
  },
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: {
            loader: "fable-loader",
            options: {
                define: isProduction ? [] : ["DEBUG"]
            }
        }
      },
      {
          test: /\.js$/,
          exclude: /node_modules/,
          use: {
              loader: 'babel-loader',
              options: babelOptions
          },
      },
      {
          test: /\.(sass|scss|css)$/,
          use: [
              'style-loader',
              'css-loader',
              'sass-loader',
          ],
      },
      {
          test: /\.css$/,
          use: ['style-loader', 'css-loader']
      },
      {
          test: /\.(png|jpg|jpeg|gif|svg|woff|woff2|ttf|eot)(\?.*$|$)/,
          use: ["file-loader"]
      }
    ]
  }
};

// At the moment draw2d and jquery are NOT npm/yarn dependencies. The source
// code is copied in the app/public/lib/ folder. This should be fine but if you
// want to update them via the package manager, you need the copy-webpack-plugin
// to copy the source files to the app/public/lib/ folder while packaging.

var mainConfig = Object.assign({
  target: "electron-main",
  entry: resolve("src/Main/Main.fsproj"),
  output: {
    path: resolve("app"),
    filename: "main.js"
  },
}, basicConfig);

var rendererConfig = Object.assign({
  target: "electron-renderer",
  entry: [ resolve("src/Renderer/Renderer.fsproj") ],
  output: {
    path: resolve("app"),
    filename: "renderer.js"
  },
}, basicConfig);

module.exports = [ mainConfig, rendererConfig ]