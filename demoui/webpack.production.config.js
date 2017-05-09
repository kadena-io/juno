var webpack = require('webpack');
var path = require('path');
var cssnext = require('postcss-cssnext');
var loaders = require('./webpack.loaders');

module.exports = {
	entry: [
		'./index.js' // Your appʼs entry point
	],
	output: {
		path: path.join(__dirname, 'public'),
		filename: 'bundle.js'
	},
	resolve: {
		extensions: ['', '.js', '.jsx']
	},
	module: {
		loaders: loaders
	}
};
