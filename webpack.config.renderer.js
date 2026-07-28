const path = require('path');
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const HtmlWebpackPlugin = require('html-webpack-plugin');
const webpack = require('webpack');
const TerserPlugin = require('terser-webpack-plugin');

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
    minimize: true,
    minimizer: [
      new TerserPlugin({
        // extractComments would emit a separate LICENSE.txt that electron-builder
        // then packages; keep everything in the one bundle instead.
        extractComments: false,
        terserOptions: {
          // Property names are deliberately NOT mangled. Fable.SimpleJson
          // serialises .dgm files using type information that reaches the output
          // as object keys and string literals; renaming those would silently
          // break loading every existing saved project.
          mangle: true,
          format: { comments: false },
        },
      }),
    ],
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
  externals: {
    usb: "commonjs2 usb", // Ref: https://copyprogramming.com/howto/electron-and-serial-ports
  }
};