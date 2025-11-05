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
            '@shared/components': '../../../packages/shared-nexpo/shared-components/src',
            '@shared/pages': '../../../packages/shared-nexpo/shared-pages/src',
            '@shared/ui': '../../../packages/shared-nexpo/shared-ui/src',
            '@shared/utils': '../../../packages/shared-nexpo/shared-utils/src',
            '@shared/provider': '../../../packages/shared-nexpo/shared-provider/src',
          },
        },
      ],
    ],
  };
};
