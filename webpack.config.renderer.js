const path = require('path');
const singleFableLibrary = require('./webpack.fable-library');
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const HtmlWebpackPlugin = require('html-webpack-plugin');
const webpack = require('webpack');

const mode = process.env.NODE_ENV || "development";
const staticPath =
  mode === "production" ?
    "`${path.join(process.resourcesPath, 'static')}`" :
    "'static'";

module.exports = {
  mode,
  // 'web', not 'electron-renderer'. The renderer is an ordinary web page now: it has no node and no
  // electron module. The two targets differ in exactly the way that matters here - electron-renderer
  // silently turns an import of a node builtin into require("..."), which then throws at runtime in
  // a page that has no require, while web makes it a build error naming the file that asked. That is
  // the difference between finding this at compile time and finding it as a blank window.
  target: 'web',
  devtool: 'cheap-module-source-map',
  entry: './src/Renderer/Renderer.fs.js',
  output: {
    globalObject: 'this',
    filename: 'renderer-index.js',
    path: path.resolve(__dirname, 'build'),
    publicPath: ''
  },
  optimization: {
    minimize: false,
  },
  module: {
      rules: [
          {
              test: /\.fs\.js$/,
              enforce: "pre",
              use: ["source-map-loader"],
          },
      {
        test: /\.(sa|sc|c)ss$/,
        use: [
          MiniCssExtractPlugin.loader,
          { loader: "css-loader", options: { sourceMap: true } },
        ],
      },

      {
        test: /\.(png|jpe?g|gif|svg|eot|ttf|woff|woff2)$/,
        use: ['file-loader'],
      }
    ]
  },
  plugins: [
    new webpack.DefinePlugin({ '__static': staticPath }),
    new MiniCssExtractPlugin({
        filename: 'css/index.css'
    }),
    new HtmlWebpackPlugin({
      template: 'public/index.html',
    }),
    // The renderer only ever had one copy, but it resolves to the same one main uses now, so the
    // two processes agree about what an F# Map is. See webpack.fable-library.js.
    singleFableLibrary(),
  ],
  resolve: {
    extensions: ['.ts', '.tsx', '.js']
  },
  // No externals. `usb` used to be one, for IS-uart.js - which now lives in the main process, where
  // a native module can actually be loaded. Nothing the renderer imports needs node any more.
};