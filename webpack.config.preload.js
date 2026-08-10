const path = require('path');

// The preload runs in its own context before the renderer, so it is a third bundle rather than part
// of either existing one.
//
// Output lands beside index.js in build/, which is what lets Bridge.preloadPath find it with a plain
// path.join(__dirname, 'preload.js'): in development the main bundle is written to build/ and run
// from there, and in production both files sit together inside the asar. No dev/production branch
// is needed, unlike splashPagePath which has to reach outside the bundle to static/.
module.exports = {
  mode: process.env.NODE_ENV || 'development',
  target: 'electron-preload',
  devtool: 'source-map',
  entry: './src/Preload/preload.js',
  output: {
    filename: 'preload.js',
    path: path.resolve(__dirname, 'build'),
    publicPath: ''
    // No `clean: true`, for the same reason webpack.config.main.js does not set it: all three
    // configs write into build/, and cleaning here would delete whichever bundle ran first.
  },
  optimization: {
    minimize: false,
  },
};
