/**
 * @shared/fonts - Comprehensive font library for Nexpo monorepo
 * 
 * A curated collection of high-quality fonts inspired by DaFont.com,
 * with professional standards from Google Docs, Microsoft Word, and modern web design.
 * 
 * Supports Next.js, Expo, and Tauri platforms with optimized loading strategies.
 */

// Export all types
export * from './types';

// Export font catalog
export { FONT_CATALOG } from './fonts/catalog';

// Export collections and presets
export {
  PROFESSIONAL_COLLECTION,
  MODERN_WEB_COLLECTION,
  CREATIVE_COLLECTION,
  EDITORIAL_COLLECTION,
  FONT_COLLECTIONS,
  TYPOGRAPHY_SYSTEMS,
} from './collections/presets';

// Export platform-specific providers
export { 
  WebFontProvider, 
  useWebFonts,
  createNextFontCSS,
  generateFontPreloadLinks,
} from './providers/web';

export { 
  MobileFontProvider, 
  useMobileFonts,
  getFontFamily as getMobileFontFamily,
} from './providers/mobile';

export { 
  TauriFontProvider, 
  default as TauriFontProviderDefault,
} from './providers/tauri';

// Export utilities
export { fontUtils } from './utils';

// Re-export commonly used utilities at top level
export {
  getFontById,
  getFontsByCategory,
  getFontsByUseCase,
  searchFonts,
  getFontPairings,
  weightToNumber,
  numberToWeight,
  generateFontFamily,
  generateTypographyCSS,
  responsiveFontSize,
  recommendFontWeight,
} from './utils';

/**
 * Quick start guide:
 * 
 * 1. Web/Next.js:
 *    ```tsx
 *    import { WebFontProvider, useWebFonts, FONT_CATALOG } from '@shared/fonts';
 *    
 *    function App() {
 *      return (
 *        <WebFontProvider preloadFonts={[
 *          { family: 'Inter', weight: '400' },
 *          { family: 'Poppins', weight: '600' }
 *        ]}>
 *          <YourApp />
 *        </WebFontProvider>
 *      );
 *    }
 *    ```
 * 
 * 2. Mobile/Expo:
 *    ```tsx
 *    import { MobileFontProvider, useMobileFonts, getMobileFontFamily } from '@shared/fonts';
 *    
 *    function App() {
 *      return (
 *        <MobileFontProvider preloadFonts={[
 *          { family: 'Inter', weight: '400' },
 *          { family: 'Poppins', weight: '600' }
 *        ]}>
 *          <Text style={{ fontFamily: getMobileFontFamily('Inter', '400') }}>
 *            Hello World
 *          </Text>
 *        </MobileFontProvider>
 *      );
 *    }
 *    ```
 * 
 * 3. Desktop/Tauri:
 *    ```tsx
 *    import { TauriFontProvider, useTauriFonts } from '@shared/fonts';
 *    
 *    function App() {
 *      return (
 *        <TauriFontProvider includeSystemFonts={true}>
 *          <YourApp />
 *        </TauriFontProvider>
 *      );
 *    }
 *    ```
 */
