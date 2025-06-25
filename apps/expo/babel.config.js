module.exports = function(api) {
  api.cache(true);
  return {
    presets: ['babel-preset-expo'],
    plugins: [
      // Commented out until properly installed
      // 'react-native-reanimated/plugin',
      [
        'module-resolver',
        {
          root: ['./'],
          alias: {
            '@shared/components': '../../packages/shared-components/src',
            '@shared/pages': '../../packages/shared-pages/src',
            '@shared/ui': '../../packages/shared-ui/src',
            '@shared/utils': '../../packages/shared-utils/src',
          },
        },
      ],
    ],
  };
};
