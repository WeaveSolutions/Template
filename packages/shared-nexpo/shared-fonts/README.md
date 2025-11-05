# @shared/fonts

A comprehensive, cross-platform font library for the Nexpo monorepo. This package provides a curated collection of high-quality fonts inspired by the best of DaFont.com, with professional standards suitable for Google Docs, Microsoft Word, and modern web design.

## Features

- 🎨 **20+ Premium Fonts**: Carefully selected from Google Fonts and Fontsource
- 🚀 **Cross-Platform Support**: Optimized for Next.js, Expo, and Tauri
- 📦 **Tree-Shakeable**: Import only the fonts you need
- 🎯 **TypeScript First**: Full type safety with comprehensive interfaces
- 🎭 **Font Collections**: Pre-designed collections for different use cases
- ⚡ **Optimized Loading**: Platform-specific loading strategies
- 🔧 **Utility Functions**: Helpers for font management and typography

## Installation

```bash
# From the monorepo root
pnpm add @shared/fonts --filter <your-app>
```

## Quick Start

### Web (Next.js)

```tsx
import { WebFontProvider, useWebFonts, FONT_CATALOG } from '@shared/fonts';

function MyApp({ Component, pageProps }) {
  return (
    <WebFontProvider preloadFonts={[
      { family: 'Inter', weight: '400' },
      { family: 'Inter', weight: '600' },
      { family: 'Poppins', weight: '700' }
    ]}>
      <Component {...pageProps} />
    </WebFontProvider>
  );
}

// In your components
function MyComponent() {
  const { isLoaded } = useWebFonts();
  
  return (
    <h1 style={{ fontFamily: 'Poppins, sans-serif', fontWeight: 700 }}>
      Beautiful Typography
    </h1>
  );
}
```

### Mobile (Expo)

```tsx
import { MobileFontProvider, useMobileFonts, getMobileFontFamily } from '@shared/fonts';
import { Text } from 'react-native';

export default function App() {
  return (
    <MobileFontProvider preloadFonts={[
      { family: 'Inter', weight: '400' },
      { family: 'Poppins', weight: '600' }
    ]}>
      <AppContent />
    </MobileFontProvider>
  );
}

function AppContent() {
  const { fontsLoaded } = useMobileFonts();
  
  if (!fontsLoaded) {
    return <Text>Loading fonts...</Text>;
  }
  
  return (
    <Text style={{ 
      fontFamily: getMobileFontFamily('Poppins', '600'),
      fontSize: 24 
    }}>
      Hello Expo!
    </Text>
  );
}
```

### Desktop (Tauri)

```tsx
import { TauriFontProvider, useTauriFonts } from '@shared/fonts';

function App() {
  return (
    <TauriFontProvider 
      includeSystemFonts={true}
      preloadFonts={[
        { family: 'Inter', weight: '400' },
        { family: 'Montserrat', weight: '700' }
      ]}
    >
      <MainWindow />
    </TauriFontProvider>
  );
}
```

## Available Fonts

### Sans-serif
- **Inter** - Modern, clean UI font (100-900 weights)
- **Poppins** - Geometric, friendly design (100-900 weights)
- **Roboto** - Android's system font (100, 300-900 weights)
- **Montserrat** - Urban, modern feel (100-900 weights)
- **Lato** - Humanist qualities (100, 300, 400, 700, 900 weights)
- **Open Sans** - Neutral, friendly (300-800 weights)
- **Raleway** - Elegant, thin options (100-900 weights)
- **Nunito** - Rounded, soft terminals (200-900 weights)
- **Quicksand** - Playful, rounded (300-700 weights)
- **Barlow** - Slightly rounded, modern (100-900 weights)

### Serif
- **Playfair Display** - Editorial elegance (400-900 weights)
- **Merriweather** - Reading comfort (300, 400, 700, 900 weights)

### Display
- **Bebas Neue** - Bold, impactful (400 weight)
- **Oswald** - Condensed, strong (200-700 weights)
- **Fredoka** - Fun, approachable (300-700 weights)

### Script/Handwriting
- **Dancing Script** - Casual script (400-700 weights)
- **Pacifico** - Retro script (400 weight)
- **Caveat** - Handwritten notes (400-700 weights)
- **Amatic SC** - Casual handwritten (400, 700 weights)

### Monospace
- **Space Mono** - Code and technical (400, 700 weights)

## Font Collections

Pre-designed collections for common use cases:

```tsx
import { FONT_COLLECTIONS, TYPOGRAPHY_SYSTEMS } from '@shared/fonts';

// Professional Document Suite
const professionalFonts = FONT_COLLECTIONS.professional;
// Includes: Inter, Roboto, Lato, Open Sans, Merriweather, Playfair Display

// Modern Web Design
const modernFonts = FONT_COLLECTIONS.modern;
// Includes: Inter, Poppins, Montserrat, Raleway, Bebas Neue, Quicksand

// Creative & Playful
const creativeFonts = FONT_COLLECTIONS.creative;
// Includes: Fredoka, Pacifico, Dancing Script, Caveat, Amatic SC, Quicksand

// Editorial & Publishing
const editorialFonts = FONT_COLLECTIONS.editorial;
// Includes: Playfair Display, Merriweather, Lato, Open Sans, Oswald
```

## Typography Systems

Complete typography systems with scale and styles:

```tsx
import { TYPOGRAPHY_SYSTEMS } from '@shared/fonts';

// Use a pre-defined system
const typography = TYPOGRAPHY_SYSTEMS.professional;

// Apply styles
<h1 style={typography.styles.h1}>Main Heading</h1>
<p style={typography.styles.body1}>Body text with perfect readability</p>
```

## Utility Functions

```tsx
import { fontUtils } from '@shared/fonts';

// Search fonts
const modernFonts = fontUtils.searchFonts('modern');
const sansSerifFonts = fontUtils.getFontsByCategory(FontCategory.SANS_SERIF);

// Get font pairings
const pairings = fontUtils.getFontPairings('inter');

// Generate responsive font sizes
const fontSize = fontUtils.responsiveFontSize(16, 14, 20);
// Returns: clamp(14px, calc(...), 20px)

// Typography CSS
const css = fontUtils.generateTypographyCSS({
  fontFamily: 'Inter',
  fontSize: 16,
  fontWeight: FontWeight.MEDIUM,
  lineHeight: 1.5
});
```

## Platform-Specific Features

### Next.js Font Optimization

```tsx
import { generateFontPreloadLinks } from '@shared/fonts';

// In _document.tsx
const preloadLinks = generateFontPreloadLinks([
  { family: 'Inter', weight: '400' },
  { family: 'Inter', weight: '600' }
]);
```

### Expo Font Status

```tsx
const { fontsLoaded, getLoadingStatus } = useMobileFonts();

const interStatus = getLoadingStatus('Inter');
if (interStatus.error) {
  console.error('Failed to load Inter:', interStatus.error);
}
```

### Tauri System Fonts

```tsx
const { systemFonts, isSystemFont } = useTauriFonts();

// Check if font is available on system
if (isSystemFont('Helvetica Neue')) {
  // Use system font
}
```

## Font Loading Strategies

1. **Preload Critical Fonts**: Load essential fonts before rendering
2. **Progressive Loading**: Load additional fonts as needed
3. **System Font Fallbacks**: Always provide system font fallbacks
4. **Font Display Swap**: Use `font-display: swap` for better UX

## Best Practices

1. **Limit Font Weights**: Only load the weights you actually use
2. **Use Font Collections**: Start with pre-designed collections
3. **Provide Fallbacks**: Always specify fallback fonts
4. **Test Cross-Platform**: Ensure fonts work on all target platforms
5. **Consider Performance**: Preload only critical fonts

## License

All fonts included in this package are licensed under open-source licenses:
- Most fonts: SIL Open Font License 1.1
- Some fonts: Apache License 2.0

Please check individual font licenses in the font catalog metadata.

## Contributing

To add a new font:
1. Add font packages to `package.json`
2. Update `FONT_CATALOG` in `src/fonts/catalog.ts`
3. Import font assets in platform providers
4. Update font map in mobile provider
5. Add to appropriate collections
6. Update documentation

## Support

For issues or questions, please file an issue in the Nexpo monorepo.
