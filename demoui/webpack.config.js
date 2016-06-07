var webpack = require('webpack');
var path = require('path');
var cssnext = require('postcss-cssnext');
var loaders = require('./webpack.loaders');

module.exports = {
	entry: [
		'webpack-dev-server/client?http://0.0.0.0:8090', // WebpackDevServer host and port
		'webpack/hot/only-dev-server',
		'./index.js' // Your appʼs entry point
	],
	devtool: process.env.WEBPACK_DEVTOOL || 'source-map',
	output: {
		path: path.join(__dirname, 'public'),
		filename: 'bundle.js'
	},
	resolve: {
		extensions: ['', '.js']
	},
	module: {
		loaders: loaders
	},
  postcss: function() {
    return [cssnext];
  },
	devServer: {
		contentBase: "./public",
			noInfo: true, //  --no-info option
			hot: true,
	  inline: true,
      port: 8090
		},
	plugins: [
		new webpack.NoErrorsPlugin()
	]
};
