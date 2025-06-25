import type { Configuration } from 'webpack';
import { rules } from './webpack.rules';
import { plugins } from './webpack.plugins';
import path from 'path';

rules.push({
  test: /\.css$/,
  use: [{ loader: 'style-loader' }, { loader: 'css-loader' }],
});

export const rendererConfig: Configuration = {
  module: {
    rules,
  },
  plugins,
  resolve: {
    extensions: ['.js', '.ts', '.jsx', '.tsx', '.css'],
    alias: {
      '@nexpo/shared-components': path.resolve(__dirname, '../../packages/shared-components/src'),
      '@nexpo/shared-ui': path.resolve(__dirname, '../../packages/shared-ui/src'),
      '@nexpo/shared-utils': path.resolve(__dirname, '../../packages/shared-utils/src'),
      '@': path.resolve(__dirname, './src'),
    },
  },
};
