const path = require('path');
const webpack = require('webpack');
const child_process = require('child_process');

const distDir = child_process.execSync("stack --stack-yaml stack-ghcjs.yaml path --dist-dir", { encoding: 'utf8' });
const allJs = path.join(distDir.trim(), "/build/test-app/test-app.jsexe/all")

module.exports = function(env) {

  return {
    entry: path.resolve(allJs),
    output: {
      filename: '[name].js',
      path: path.resolve('js'),
      publicPath: '/static/'
    },
    resolve: {
      extensions: ['.js' ],
      alias: {
        'bootstrapStyles': 'bootstrap/dist/css/bootstrap.min.css'
      },
    },
    module: {
      loaders: [
        { test: /\.css$/, exclude: /\.useable\.css$/, use: [
            { loader: "style-loader" },
            { loader: "css-loader" }
          ]},
        { test: /test-app\.jsexe\/all.js$/, loader: 'imports-loader?bootstrapStyles'},
        { test: /\.(woff|woff2)(\?v=\d+\.\d+\.\d+)?$/, loader: 'file-loader'},
        { test: /\.ttf(\?v=\d+\.\d+\.\d+)?$/, loader: 'file-loader'},
        { test: /\.eot(\?v=\d+\.\d+\.\d+)?$/, loader: 'file-loader'},
        { test: /fonts\/.+\.svg(\?v=\d+\.\d+\.\d+)?$/, loader: 'file-loader'},
      ],
    },
    devtool: "#cheap-module-source-map",
    devServer: {
      port: 9000,
      historyApiFallback: true,
      hot: true
    },
  }
}
