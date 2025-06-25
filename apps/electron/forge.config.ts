import type { ForgeConfig } from '@electron-forge/shared-types';
import { MakerSquirrel } from '@electron-forge/maker-squirrel';
import { MakerZIP } from '@electron-forge/maker-zip';
import { MakerDeb } from '@electron-forge/maker-deb';
import { MakerRpm } from '@electron-forge/maker-rpm';
import { MakerDMG } from '@electron-forge/maker-dmg';
import { AutoUnpackNativesPlugin } from '@electron-forge/plugin-auto-unpack-natives';
import { WebpackPlugin } from '@electron-forge/plugin-webpack';
import { PublisherGitHubConfig } from '@electron-forge/publisher-github';
import { mainConfig } from './webpack.main.config';
import { rendererConfig } from './webpack.renderer.config';

const config: ForgeConfig = {
  packagerConfig: {
    name: 'Nexpo',
    asar: true,
    icon: './src/assets/icon',
    appBundleId: 'com.nexpo.app',
    appCategoryType: 'public.app-category.productivity',
    win32metadata: {
      CompanyName: 'Nexpo',
      FileDescription: 'Nexpo Desktop Application',
      OriginalFilename: 'Nexpo.exe',
      ProductName: 'Nexpo',
    },
  },
  rebuildConfig: {},
  makers: [
    new MakerSquirrel({
      authors: 'Nexpo Team',
      description: 'Nexpo Desktop Application',
      setupIcon: './src/assets/icon.ico',
      iconUrl: 'https://raw.githubusercontent.com/nexpo/nexpo/main/icon.ico',
    }),
    new MakerZIP({}, ['darwin']),
    new MakerDMG({
      format: 'ULFO',
      icon: './src/assets/icon.icns',
      name: 'Nexpo',
    }),
    new MakerRpm({
      options: {
        homepage: 'https://nexpo.io',
        icon: './src/assets/icon.png',
      },
    }),
    new MakerDeb({
      options: {
        maintainer: 'Nexpo Team',
        homepage: 'https://nexpo.io',
        icon: './src/assets/icon.png',
      },
    }),
  ],
  publishers: [
    {
      name: '@electron-forge/publisher-github',
      config: {
        repository: {
          owner: 'nexpo',
          name: 'nexpo',
        },
        prerelease: false,
        draft: true,
      } as PublisherGitHubConfig,
    },
  ],
  plugins: [
    new AutoUnpackNativesPlugin({}),
    new WebpackPlugin({
      mainConfig,
      renderer: {
        config: rendererConfig,
        entryPoints: [
          {
            html: './src/renderer/index.html',
            js: './src/renderer/index.tsx',
            name: 'main_window',
            preload: {
              js: './src/preload/index.ts',
            },
          },
        ],
      },
    }),
  ],
};
