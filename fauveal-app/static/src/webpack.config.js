const path = require("path");
const webpack = require("webpack");
module.exports = {
  entry: {
    bundle: "./js/entry.js",
    "bundle.min": "./js/entry.js",
  },
  devtool: "source-map",
  resolve: {
    fallback: {
      buffer: require.resolve("buffer/"),
      assert: require.resolve("assert/"),
      stream: require.resolve("stream-browserify"),
    },
  },
  output: {
    path: path.resolve(__dirname, "dist"),
    library: { name: "bundle", type: "umd" },
  },
  plugins: [
    new webpack.ProvidePlugin({
      Buffer: ["buffer", "Buffer"],
    }),
    // fix "process is not defined" error; TODO may  be redundant with the "browser" tag now in package.json
    new webpack.ProvidePlugin({
      process: "process/browser",
    }),
  ],
  mode: "production",
};
