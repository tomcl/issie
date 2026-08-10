const path = require('path');
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
  target: 'electron-renderer',
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
  ],
  resolve: {
    extensions: ['.ts', '.tsx', '.js']
  },
  // No externals. `usb` used to be one, for IS-uart.js - which now lives in the main process, where
  // a native module can actually be loaded. Nothing the renderer imports needs node any more.
};