const webpack = require('webpack');
const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const ExtractTextPlugin = require('extract-text-webpack-plugin');

module.exports = {
    entry: {
        app: './src/index.js',
    },
    output: {
        filename: '[name].[chunkhash].bundle.js',
        path: path.resolve(__dirname, 'dist'),
    },
    devtool: 'source-map',
    devServer: {
        contentBase: './dist',
        compress: true,
        port: 9000,
        proxy: {
            '/api': {
                target: 'http://localhost:8080/rain-predictor',
                pathRewrite: {
                    '^/api': '',
                },
            },
        },
        stats: { colors: true },
    },
    module: {
        rules: [{
                test: /\.elm$/,
                exclude: [path.resolve('node_modules'), path.resolve('elm-stuff')],
                loader: 'elm-webpack-loader',
                options: {
                    verbose: true,
                    warn: true
                }

            },
            {
                test: /\.css$/,
                loader: ExtractTextPlugin.extract({
                    fallback: 'style-loader',
                    use: ['css-loader'],
                }),
            }
        ],
        noParse: /\.elm$/
    },
    plugins: [
        new ExtractTextPlugin('style.css'),
        new HtmlWebpackPlugin({
            template: 'index.html',
            indject: 'body',
        })
    ]
};