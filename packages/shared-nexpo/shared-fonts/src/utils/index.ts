import { FontMetadata, FontWeight, FontStyle, FontCategory, TypographyStyle } from '../types';
import { FONT_CATALOG } from '../fonts/catalog';

/**
 * Font utility functions for common operations
 */

/**
 * Get font by ID from catalog
 */
export const getFontById = (fontId: string): FontMetadata | undefined => {
  return FONT_CATALOG[fontId];
};

/**
 * Get fonts by category
 */
export const getFontsByCategory = (category: FontCategory): FontMetadata[] => {
  return Object.values(FONT_CATALOG).filter(font => font.category === category);
};

/**
 * Get fonts by use case
 */
export const getFontsByUseCase = (useCase: string): FontMetadata[] => {
  const normalizedUseCase = useCase.toLowerCase();
  return Object.values(FONT_CATALOG).filter(font => 
    font.useCases.some(uc => uc.toLowerCase().includes(normalizedUseCase))
  );
};

/**
 * Search fonts by name or designer
 */
export const searchFonts = (query: string): FontMetadata[] => {
  const normalizedQuery = query.toLowerCase();
  return Object.values(FONT_CATALOG).filter(font => 
    font.displayName.toLowerCase().includes(normalizedQuery) ||
    font.designer?.toLowerCase().includes(normalizedQuery) ||
    font.family.toLowerCase().includes(normalizedQuery)
  );
};

/**
 * Get recommended font pairings for a given font
 */
export const getFontPairings = (fontId: string): FontMetadata[] => {
  const font = FONT_CATALOG[fontId];
  if (!font || !font.pairings) {
    return [];
  }
  
  return font.pairings
    .map(pairingId => FONT_CATALOG[pairingId])
    .filter(Boolean);
};

/**
 * Convert font weight number to CSS weight value
 */
export const weightToNumber = (weight: FontWeight | string): number => {
  const weightMap: Record<string, number> = {
    [FontWeight.THIN]: 100,
    [FontWeight.EXTRA_LIGHT]: 200,
    [FontWeight.LIGHT]: 300,
    [FontWeight.REGULAR]: 400,
    [FontWeight.MEDIUM]: 500,
    [FontWeight.SEMI_BOLD]: 600,
    [FontWeight.BOLD]: 700,
    [FontWeight.EXTRA_BOLD]: 800,
    [FontWeight.BLACK]: 900,
  };
  
  return weightMap[weight] || parseInt(weight, 10) || 400;
};

/**
 * Convert CSS weight number to FontWeight enum
 */
export const numberToWeight = (weight: number): FontWeight => {
  const weightMap: Record<number, FontWeight> = {
    100: FontWeight.THIN,
    200: FontWeight.EXTRA_LIGHT,
    300: FontWeight.LIGHT,
    400: FontWeight.REGULAR,
    500: FontWeight.MEDIUM,
    600: FontWeight.SEMI_BOLD,
    700: FontWeight.BOLD,
    800: FontWeight.EXTRA_BOLD,
    900: FontWeight.BLACK,
  };
  
  return weightMap[weight] || FontWeight.REGULAR;
};

/**
 * Generate CSS font-family string with fallbacks
 */
export const generateFontFamily = (
  family: string, 
  fallbacks: string[] = []
): string => {
  const families = [family, ...fallbacks];
  return families
    .map(f => (f.includes(' ') ? `"${f}"` : f))
    .join(', ');
};

/**
 * Generate CSS for typography style
 */
export const generateTypographyCSS = (style: TypographyStyle): string => {
  const css: string[] = [];
  
  if (style.fontFamily) {
    css.push(`font-family: ${style.fontFamily};`);
  }
  if (style.fontSize !== undefined) {
    css.push(`font-size: ${style.fontSize}px;`);
  }
  if (style.fontWeight) {
    css.push(`font-weight: ${weightToNumber(style.fontWeight)};`);
  }
  if (style.fontStyle) {
    css.push(`font-style: ${style.fontStyle};`);
  }
  if (style.lineHeight !== undefined) {
    css.push(`line-height: ${style.lineHeight};`);
  }
  if (style.letterSpacing !== undefined) {
    css.push(`letter-spacing: ${style.letterSpacing}em;`);
  }
  if (style.textTransform) {
    css.push(`text-transform: ${style.textTransform};`);
  }
  if (style.textDecoration) {
    css.push(`text-decoration: ${style.textDecoration};`);
  }
  
  return css.join('\n  ');
};

/**
 * Calculate responsive font size based on viewport
 */
export const responsiveFontSize = (
  baseSize: number,
  minSize: number = 14,
  maxSize: number = 24,
  minViewport: number = 320,
  maxViewport: number = 1200
): string => {
  const slope = (maxSize - minSize) / (maxViewport - minViewport);
  const yIntercept = minSize - slope * minViewport;
  
  return `clamp(${minSize}px, ${yIntercept}px + ${slope * 100}vw, ${maxSize}px)`;
};

/**
 * Get contrast ratio between text and background colors
 * Useful for ensuring font readability
 */
export const getContrastRatio = (
  textColor: string, 
  backgroundColor: string
): number => {
  // Simple implementation - would need color parsing in production
  // This is a placeholder that returns a mock ratio
  return 4.5; // Minimum WCAG AA ratio
};

/**
 * Recommend font weight based on font size and use case
 */
export const recommendFontWeight = (
  fontSize: number,
  useCase: 'heading' | 'body' | 'caption' = 'body'
): FontWeight => {
  if (useCase === 'heading') {
    if (fontSize >= 48) return FontWeight.BOLD;
    if (fontSize >= 32) return FontWeight.SEMI_BOLD;
    return FontWeight.MEDIUM;
  } else if (useCase === 'caption') {
    return fontSize <= 12 ? FontWeight.MEDIUM : FontWeight.REGULAR;
  } else {
    return fontSize <= 14 ? FontWeight.MEDIUM : FontWeight.REGULAR;
  }
};

/**
 * Font loading optimization utilities
 */
export const fontOptimization = {
  /**
   * Generate preload link tags for critical fonts
   */
  generatePreloadTags: (fonts: string[]): string[] => {
    return fonts.map(fontId => {
      const font = FONT_CATALOG[fontId];
      if (!font) return '';
      
      const familySlug = font.family.toLowerCase().replace(/\s+/g, '-');
      return `<link rel="preload" href="/fonts/${familySlug}/${familySlug}-regular.woff2" as="font" type="font/woff2" crossorigin>`;
    }).filter(Boolean);
  },

  /**
   * Get critical fonts for above-the-fold content
   */
  getCriticalFonts: (fontIds: string[]): string[] => {
    // Return up to 3 most important fonts for initial load
    return fontIds.slice(0, 3);
  },

  /**
   * Generate font-display CSS based on loading strategy
   */
  getFontDisplay: (strategy: 'swap' | 'fallback' | 'optional' | 'block' = 'swap'): string => {
    return `font-display: ${strategy};`;
  },
};

/**
 * Platform-specific font utilities
 */
export const platformFonts = {
  /**
   * Get system font stack for current platform
   */
  getSystemFontStack: (): string[] => {
    // Check if we're in a browser environment
    if (typeof window === 'undefined') {
      return ['system-ui', 'sans-serif'];
    }

    const userAgent = window.navigator.userAgent;
    
    if (userAgent.includes('Mac')) {
      return ['-apple-system', 'BlinkMacSystemFont', 'Helvetica Neue', 'sans-serif'];
    } else if (userAgent.includes('Windows')) {
      return ['Segoe UI', 'Tahoma', 'sans-serif'];
    } else if (userAgent.includes('Android')) {
      return ['Roboto', 'Droid Sans', 'sans-serif'];
    } else if (userAgent.includes('Linux')) {
      return ['Ubuntu', 'Cantarell', 'DejaVu Sans', 'sans-serif'];
    }
    
    return ['system-ui', 'sans-serif'];
  },

  /**
   * Check if font is available on system
   */
  isFontAvailable: async (fontFamily: string): Promise<boolean> => {
    if (typeof document === 'undefined' || !('fonts' in document)) {
      return false;
    }

    try {
      return await document.fonts.check(`16px "${fontFamily}"`);
    } catch {
      return false;
    }
  },
};

/**
 * Typography scale utilities
 */
export const typographyScale = {
  /**
   * Generate modular scale
   */
  modularScale: (
    base: number = 16,
    ratio: number = 1.25,
    steps: number = 10
  ): number[] => {
    const scale: number[] = [];
    for (let i = -2; i < steps - 2; i++) {
      scale.push(Math.round(base * Math.pow(ratio, i)));
    }
    return scale;
  },

  /**
   * Get type scale preset
   */
  getPresetScale: (preset: 'minor-second' | 'major-second' | 'minor-third' | 'major-third' | 'perfect-fourth' | 'augmented-fourth' | 'perfect-fifth' | 'golden-ratio') => {
    const ratios = {
      'minor-second': 1.067,
      'major-second': 1.125,
      'minor-third': 1.2,
      'major-third': 1.25,
      'perfect-fourth': 1.333,
      'augmented-fourth': 1.414,
      'perfect-fifth': 1.5,
      'golden-ratio': 1.618,
    };
    
    return typographyScale.modularScale(16, ratios[preset]);
  },
};

/**
 * Export all utilities
 */
export const fontUtils = {
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
  getContrastRatio,
  recommendFontWeight,
  ...fontOptimization,
  platform: platformFonts,
  scale: typographyScale,
};
