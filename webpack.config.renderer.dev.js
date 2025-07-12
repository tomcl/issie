const path = require('path');
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const HtmlWebpackPlugin = require('html-webpack-plugin');
const webpack = require('webpack');

module.exports = {
  mode: 'development',
  target: 'electron-renderer',
  devtool: 'eval-cheap-module-source-map', // Faster source maps for development
  entry: './src/Renderer/Renderer.fs.js',
  output: {
    globalObject: 'this',
    filename: 'renderer-index.js',
    path: path.resolve(__dirname, 'build'),
    publicPath: ''
  },
  optimization: {
    removeAvailableModules: false,
    removeEmptyChunks: false,
    splitChunks: false,
    runtimeChunk: false,
    // Development-specific optimizations for faster rebuilds
    moduleIds: 'named',
    chunkIds: 'named'
  },
  cache: {
    type: 'filesystem', // Enable persistent caching
    buildDependencies: {
      config: [__filename]
    }
  },
  module: {
    rules: [
      {
        test: /\.fs\.js$/,
        enforce: "pre",
        use: ["source-map-loader"],
      },
      {
        test: /\.(js|json|ts|tsx)$/,
        exclude: /(node_modules|bower_components)/,
        use: {
          loader: 'babel-loader',
          options: {
            cacheDirectory: true // Enable babel caching
          }
        }
      },
      {
        test: /\.(sa|sc|c)ss$/,
        use: [
          'style-loader', // Use style-loader in dev for HMR
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
    new webpack.DefinePlugin({ '__static': "'static'" }),
    new HtmlWebpackPlugin({
      template: 'public/index.html',
      cache: true
    }),
    // Enable HMR
    new webpack.HotModuleReplacementPlugin()
  ],
  resolve: {
    extensions: ['.ts', '.tsx', '.js'],
    // Cache module resolutions
    cache: true
  },
  externals: {
    usb: "commonjs2 usb",
  },
  watchOptions: {
    ignored: /node_modules/,
    aggregateTimeout: 300, // Delay rebuild after the first change
    poll: false
  }
};