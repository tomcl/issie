const path = require('path');
const webpack = require('webpack');

const mode = process.env.NODE_ENV || "development";
const staticPath =
  mode === "production" ?
    "`${path.join(process.resourcesPath, 'static')}`" :
    "'static'";

module.exports = {
  mode,
  target: 'electron-main',
  devtool: 'source-map',
  entry: './src/Main/Main.fs.js',
  output: {
    globalObject: 'this',
    filename: 'index.js',
    path: path.resolve(__dirname, 'build'),
    publicPath: ''
    // No `clean: true` here. Both configs write into build/, and clean empties the whole
    // directory - so building main after the renderer deletes renderer-index.js, index.html and
    // css/index.css, and electron-builder packages an app with no renderer in it. scripts/build.js
    // removes build/ once, before either compiler runs, which is where that belongs.
  },
  optimization: {
    minimize: false,
    },
  module: {
      rules: [
          {
              test: /\.js$/,
              enforce: "pre",
              use: ["source-map-loader"],
          }
    ]
  },
  plugins: [
    new webpack.DefinePlugin({ '__static': staticPath }),
  ],
  resolve: {
    extensions: ['.ts', '.js']
  }
};