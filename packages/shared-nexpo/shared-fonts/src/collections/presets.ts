import { FontCollection, TypographySystem, FontWeight, FontConfig } from '../types';
import { FONT_CATALOG } from '../fonts/catalog';

/**
 * Pre-designed font collections for common use cases
 * Inspired by professional document standards and modern web design
 */

// Google Docs / Microsoft Word style professional collection
export const PROFESSIONAL_COLLECTION: FontCollection = {
  id: 'professional',
  name: 'Professional Document Suite',
  description: 'Clean, readable fonts suitable for business documents, reports, and formal communication',
  theme: 'professional',
  fonts: [
    FONT_CATALOG['inter'],
    FONT_CATALOG['roboto'],
    FONT_CATALOG['lato'],
    FONT_CATALOG['open-sans'],
    FONT_CATALOG['merriweather'],
    FONT_CATALOG['playfair-display'],
    FONT_CATALOG['space-mono'],
  ],
  recommendedPairings: [
    {
      primary: 'inter',
      secondary: 'merriweather',
      use: 'Modern documents with classic serif for emphasis',
    },
    {
      primary: 'lato',
      secondary: 'playfair-display',
      use: 'Corporate presentations with elegant headlines',
    },
    {
      primary: 'roboto',
      secondary: 'roboto',
      use: 'Technical documentation with consistent voice',
    },
  ],
};

// Modern web design collection
export const MODERN_WEB_COLLECTION: FontCollection = {
  id: 'modern-web',
  name: 'Modern Web Design',
  description: 'Contemporary fonts optimized for digital interfaces and modern aesthetics',
  theme: 'modern',
  fonts: [
    FONT_CATALOG['inter'],
    FONT_CATALOG['poppins'],
    FONT_CATALOG['montserrat'],
    FONT_CATALOG['raleway'],
    FONT_CATALOG['bebas-neue'],
    FONT_CATALOG['quicksand'],
    FONT_CATALOG['space-mono'],
  ],
  recommendedPairings: [
    {
      primary: 'poppins',
      secondary: 'inter',
      use: 'Bold headlines with clean body text',
    },
    {
      primary: 'montserrat',
      secondary: 'lato',
      use: 'Urban modern design with readable content',
    },
    {
      primary: 'bebas-neue',
      secondary: 'roboto',
      use: 'Impact headlines with neutral body',
    },
  ],
};

// Creative & playful collection
export const CREATIVE_COLLECTION: FontCollection = {
  id: 'creative',
  name: 'Creative & Playful',
  description: 'Expressive fonts for brands, games, and creative projects',
  theme: 'playful',
  fonts: [
    FONT_CATALOG['fredoka'],
    FONT_CATALOG['pacifico'],
    FONT_CATALOG['dancing-script'],
    FONT_CATALOG['caveat'],
    FONT_CATALOG['amatic-sc'],
    FONT_CATALOG['quicksand'],
    FONT_CATALOG['nunito'],
  ],
  recommendedPairings: [
    {
      primary: 'fredoka',
      secondary: 'nunito',
      use: 'Fun, approachable design for kids content',
    },
    {
      primary: 'pacifico',
      secondary: 'quicksand',
      use: 'Retro branding with modern readability',
    },
    {
      primary: 'dancing-script',
      secondary: 'lato',
      use: 'Elegant script accents with clean body',
    },
  ],
};

// Classic editorial collection
export const EDITORIAL_COLLECTION: FontCollection = {
  id: 'editorial',
  name: 'Editorial & Publishing',
  description: 'Timeless fonts for magazines, blogs, and long-form content',
  theme: 'classic',
  fonts: [
    FONT_CATALOG['playfair-display'],
    FONT_CATALOG['merriweather'],
    FONT_CATALOG['lato'],
    FONT_CATALOG['open-sans'],
    FONT_CATALOG['oswald'],
    FONT_CATALOG['dancing-script'],
  ],
  recommendedPairings: [
    {
      primary: 'playfair-display',
      secondary: 'lato',
      use: 'Magazine-style headlines with readable body',
    },
    {
      primary: 'merriweather',
      secondary: 'open-sans',
      use: 'Traditional reading experience with modern UI',
    },
    {
      primary: 'oswald',
      secondary: 'merriweather',
      use: 'Bold section headers with classic body text',
    },
  ],
};

// Default typography systems for different use cases
export const TYPOGRAPHY_SYSTEMS: Record<string, TypographySystem> = {
  // Professional system (like Google Docs)
  professional: {
    scale: {
      xs: 10,
      sm: 12,
      base: 14,
      lg: 16,
      xl: 18,
      '2xl': 20,
      '3xl': 24,
      '4xl': 28,
      '5xl': 32,
      '6xl': 36,
      '7xl': 42,
      '8xl': 48,
      '9xl': 56,
    },
    fonts: {
      primary: {
        family: 'Inter',
        fallback: ['system-ui', 'sans-serif'],
      },
      secondary: {
        family: 'Merriweather',
        fallback: ['Georgia', 'serif'],
      },
      display: {
        family: 'Playfair Display',
        fallback: ['Georgia', 'serif'],
      },
      mono: {
        family: 'Space Mono',
        fallback: ['Consolas', 'monospace'],
      },
    },
    styles: {
      h1: {
        fontFamily: 'Playfair Display',
        fontSize: 36,
        fontWeight: FontWeight.BOLD,
        lineHeight: 1.2,
        letterSpacing: -0.02,
      },
      h2: {
        fontFamily: 'Playfair Display',
        fontSize: 28,
        fontWeight: FontWeight.SEMI_BOLD,
        lineHeight: 1.3,
        letterSpacing: -0.01,
      },
      h3: {
        fontFamily: 'Inter',
        fontSize: 24,
        fontWeight: FontWeight.SEMI_BOLD,
        lineHeight: 1.4,
      },
      h4: {
        fontFamily: 'Inter',
        fontSize: 20,
        fontWeight: FontWeight.MEDIUM,
        lineHeight: 1.4,
      },
      h5: {
        fontFamily: 'Inter',
        fontSize: 18,
        fontWeight: FontWeight.MEDIUM,
        lineHeight: 1.5,
      },
      h6: {
        fontFamily: 'Inter',
        fontSize: 16,
        fontWeight: FontWeight.MEDIUM,
        lineHeight: 1.5,
      },
      body1: {
        fontFamily: 'Inter',
        fontSize: 16,
        fontWeight: FontWeight.REGULAR,
        lineHeight: 1.6,
      },
      body2: {
        fontFamily: 'Inter',
        fontSize: 14,
        fontWeight: FontWeight.REGULAR,
        lineHeight: 1.6,
      },
      caption: {
        fontFamily: 'Inter',
        fontSize: 12,
        fontWeight: FontWeight.REGULAR,
        lineHeight: 1.5,
        letterSpacing: 0.02,
      },
      overline: {
        fontFamily: 'Inter',
        fontSize: 12,
        fontWeight: FontWeight.MEDIUM,
        lineHeight: 1.5,
        letterSpacing: 0.08,
        textTransform: 'uppercase',
      },
      button: {
        fontFamily: 'Inter',
        fontSize: 14,
        fontWeight: FontWeight.MEDIUM,
        lineHeight: 1.5,
        letterSpacing: 0.02,
        textTransform: 'uppercase',
      },
      code: {
        fontFamily: 'Space Mono',
        fontSize: 14,
        fontWeight: FontWeight.REGULAR,
        lineHeight: 1.5,
      },
    },
  },

  // Modern web system
  modern: {
    scale: {
      xs: 12,
      sm: 14,
      base: 16,
      lg: 18,
      xl: 20,
      '2xl': 24,
      '3xl': 30,
      '4xl': 36,
      '5xl': 48,
      '6xl': 60,
      '7xl': 72,
      '8xl': 96,
      '9xl': 128,
    },
    fonts: {
      primary: {
        family: 'Poppins',
        fallback: ['system-ui', 'sans-serif'],
      },
      secondary: {
        family: 'Inter',
        fallback: ['system-ui', 'sans-serif'],
      },
      display: {
        family: 'Bebas Neue',
        fallback: ['Impact', 'sans-serif'],
      },
      mono: {
        family: 'Space Mono',
        fallback: ['Consolas', 'monospace'],
      },
    },
    styles: {
      h1: {
        fontFamily: 'Bebas Neue',
        fontSize: 72,
        fontWeight: FontWeight.REGULAR,
        lineHeight: 1.1,
        letterSpacing: 0.02,
      },
      h2: {
        fontFamily: 'Poppins',
        fontSize: 48,
        fontWeight: FontWeight.BOLD,
        lineHeight: 1.2,
        letterSpacing: -0.02,
      },
      h3: {
        fontFamily: 'Poppins',
        fontSize: 36,
        fontWeight: FontWeight.SEMI_BOLD,
        lineHeight: 1.3,
        letterSpacing: -0.01,
      },
      h4: {
        fontFamily: 'Poppins',
        fontSize: 30,
        fontWeight: FontWeight.SEMI_BOLD,
        lineHeight: 1.3,
      },
      h5: {
        fontFamily: 'Poppins',
        fontSize: 24,
        fontWeight: FontWeight.MEDIUM,
        lineHeight: 1.4,
      },
      h6: {
        fontFamily: 'Poppins',
        fontSize: 20,
        fontWeight: FontWeight.MEDIUM,
        lineHeight: 1.4,
      },
      body1: {
        fontFamily: 'Inter',
        fontSize: 16,
        fontWeight: FontWeight.REGULAR,
        lineHeight: 1.7,
      },
      body2: {
        fontFamily: 'Inter',
        fontSize: 14,
        fontWeight: FontWeight.REGULAR,
        lineHeight: 1.7,
      },
      caption: {
        fontFamily: 'Inter',
        fontSize: 12,
        fontWeight: FontWeight.REGULAR,
        lineHeight: 1.5,
        letterSpacing: 0.03,
      },
      overline: {
        fontFamily: 'Poppins',
        fontSize: 12,
        fontWeight: FontWeight.SEMI_BOLD,
        lineHeight: 1.5,
        letterSpacing: 0.1,
        textTransform: 'uppercase',
      },
      button: {
        fontFamily: 'Poppins',
        fontSize: 16,
        fontWeight: FontWeight.SEMI_BOLD,
        lineHeight: 1.5,
        letterSpacing: 0.02,
      },
      code: {
        fontFamily: 'Space Mono',
        fontSize: 14,
        fontWeight: FontWeight.REGULAR,
        lineHeight: 1.6,
      },
    },
  },

  // Creative system
  creative: {
    scale: {
      xs: 14,
      sm: 16,
      base: 18,
      lg: 20,
      xl: 24,
      '2xl': 30,
      '3xl': 36,
      '4xl': 48,
      '5xl': 60,
      '6xl': 72,
      '7xl': 96,
      '8xl': 120,
      '9xl': 144,
    },
    fonts: {
      primary: {
        family: 'Fredoka',
        fallback: ['Comic Sans MS', 'sans-serif'],
      },
      secondary: {
        family: 'Quicksand',
        fallback: ['system-ui', 'sans-serif'],
      },
      display: {
        family: 'Pacifico',
        fallback: ['cursive'],
      },
      mono: {
        family: 'Space Mono',
        fallback: ['Consolas', 'monospace'],
      },
      handwriting: {
        family: 'Caveat',
        fallback: ['cursive'],
      },
    },
    styles: {
      h1: {
        fontFamily: 'Pacifico',
        fontSize: 60,
        fontWeight: FontWeight.REGULAR,
        lineHeight: 1.2,
      },
      h2: {
        fontFamily: 'Fredoka',
        fontSize: 48,
        fontWeight: FontWeight.BOLD,
        lineHeight: 1.3,
      },
      h3: {
        fontFamily: 'Fredoka',
        fontSize: 36,
        fontWeight: FontWeight.SEMI_BOLD,
        lineHeight: 1.3,
      },
      h4: {
        fontFamily: 'Fredoka',
        fontSize: 30,
        fontWeight: FontWeight.MEDIUM,
        lineHeight: 1.4,
      },
      h5: {
        fontFamily: 'Quicksand',
        fontSize: 24,
        fontWeight: FontWeight.SEMI_BOLD,
        lineHeight: 1.4,
      },
      h6: {
        fontFamily: 'Quicksand',
        fontSize: 20,
        fontWeight: FontWeight.SEMI_BOLD,
        lineHeight: 1.5,
      },
      body1: {
        fontFamily: 'Quicksand',
        fontSize: 18,
        fontWeight: FontWeight.REGULAR,
        lineHeight: 1.6,
      },
      body2: {
        fontFamily: 'Quicksand',
        fontSize: 16,
        fontWeight: FontWeight.REGULAR,
        lineHeight: 1.6,
      },
      caption: {
        fontFamily: 'Quicksand',
        fontSize: 14,
        fontWeight: FontWeight.MEDIUM,
        lineHeight: 1.5,
      },
      overline: {
        fontFamily: 'Fredoka',
        fontSize: 14,
        fontWeight: FontWeight.SEMI_BOLD,
        lineHeight: 1.5,
        letterSpacing: 0.08,
        textTransform: 'uppercase',
      },
      button: {
        fontFamily: 'Fredoka',
        fontSize: 18,
        fontWeight: FontWeight.SEMI_BOLD,
        lineHeight: 1.5,
      },
      code: {
        fontFamily: 'Space Mono',
        fontSize: 16,
        fontWeight: FontWeight.REGULAR,
        lineHeight: 1.5,
      },
    },
  },
};

// Export all collections
export const FONT_COLLECTIONS = {
  professional: PROFESSIONAL_COLLECTION,
  modern: MODERN_WEB_COLLECTION,
  creative: CREATIVE_COLLECTION,
  editorial: EDITORIAL_COLLECTION,
};
