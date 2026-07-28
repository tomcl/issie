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
    publicPath: '',
    clean: true
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