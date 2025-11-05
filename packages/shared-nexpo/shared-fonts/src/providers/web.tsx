import React, { createContext, useContext, useEffect, useState } from 'react';
import { FontLoader, FontConfig, FontLoadingStatus } from '../types';

// Import all web fonts (Fontsource)
import '@fontsource/inter/100.css';
import '@fontsource/inter/200.css';
import '@fontsource/inter/300.css';
import '@fontsource/inter/400.css';
import '@fontsource/inter/500.css';
import '@fontsource/inter/600.css';
import '@fontsource/inter/700.css';
import '@fontsource/inter/800.css';
import '@fontsource/inter/900.css';

import '@fontsource/poppins/100.css';
import '@fontsource/poppins/200.css';
import '@fontsource/poppins/300.css';
import '@fontsource/poppins/400.css';
import '@fontsource/poppins/500.css';
import '@fontsource/poppins/600.css';
import '@fontsource/poppins/700.css';
import '@fontsource/poppins/800.css';
import '@fontsource/poppins/900.css';

import '@fontsource/roboto/100.css';
import '@fontsource/roboto/300.css';
import '@fontsource/roboto/400.css';
import '@fontsource/roboto/500.css';
import '@fontsource/roboto/700.css';
import '@fontsource/roboto/900.css';

import '@fontsource/montserrat/100.css';
import '@fontsource/montserrat/200.css';
import '@fontsource/montserrat/300.css';
import '@fontsource/montserrat/400.css';
import '@fontsource/montserrat/500.css';
import '@fontsource/montserrat/600.css';
import '@fontsource/montserrat/700.css';
import '@fontsource/montserrat/800.css';
import '@fontsource/montserrat/900.css';

import '@fontsource/playfair-display/400.css';
import '@fontsource/playfair-display/500.css';
import '@fontsource/playfair-display/600.css';
import '@fontsource/playfair-display/700.css';
import '@fontsource/playfair-display/800.css';
import '@fontsource/playfair-display/900.css';

import '@fontsource/merriweather/300.css';
import '@fontsource/merriweather/400.css';
import '@fontsource/merriweather/700.css';
import '@fontsource/merriweather/900.css';

import '@fontsource/bebas-neue/400.css';

import '@fontsource/oswald/200.css';
import '@fontsource/oswald/300.css';
import '@fontsource/oswald/400.css';
import '@fontsource/oswald/500.css';
import '@fontsource/oswald/600.css';
import '@fontsource/oswald/700.css';

import '@fontsource/fredoka/300.css';
import '@fontsource/fredoka/400.css';
import '@fontsource/fredoka/500.css';
import '@fontsource/fredoka/600.css';
import '@fontsource/fredoka/700.css';

import '@fontsource/dancing-script/400.css';
import '@fontsource/dancing-script/500.css';
import '@fontsource/dancing-script/600.css';
import '@fontsource/dancing-script/700.css';

import '@fontsource/pacifico/400.css';

import '@fontsource/caveat/400.css';
import '@fontsource/caveat/500.css';
import '@fontsource/caveat/600.css';
import '@fontsource/caveat/700.css';

import '@fontsource/amatic-sc/400.css';
import '@fontsource/amatic-sc/700.css';

import '@fontsource/space-mono/400.css';
import '@fontsource/space-mono/700.css';

import '@fontsource/lato/100.css';
import '@fontsource/lato/300.css';
import '@fontsource/lato/400.css';
import '@fontsource/lato/700.css';
import '@fontsource/lato/900.css';

import '@fontsource/open-sans/300.css';
import '@fontsource/open-sans/400.css';
import '@fontsource/open-sans/500.css';
import '@fontsource/open-sans/600.css';
import '@fontsource/open-sans/700.css';
import '@fontsource/open-sans/800.css';

import '@fontsource/raleway/100.css';
import '@fontsource/raleway/200.css';
import '@fontsource/raleway/300.css';
import '@fontsource/raleway/400.css';
import '@fontsource/raleway/500.css';
import '@fontsource/raleway/600.css';
import '@fontsource/raleway/700.css';
import '@fontsource/raleway/800.css';
import '@fontsource/raleway/900.css';

import '@fontsource/nunito/200.css';
import '@fontsource/nunito/300.css';
import '@fontsource/nunito/400.css';
import '@fontsource/nunito/500.css';
import '@fontsource/nunito/600.css';
import '@fontsource/nunito/700.css';
import '@fontsource/nunito/800.css';
import '@fontsource/nunito/900.css';

import '@fontsource/quicksand/300.css';
import '@fontsource/quicksand/400.css';
import '@fontsource/quicksand/500.css';
import '@fontsource/quicksand/600.css';
import '@fontsource/quicksand/700.css';

import '@fontsource/barlow/100.css';
import '@fontsource/barlow/200.css';
import '@fontsource/barlow/300.css';
import '@fontsource/barlow/400.css';
import '@fontsource/barlow/500.css';
import '@fontsource/barlow/600.css';
import '@fontsource/barlow/700.css';
import '@fontsource/barlow/800.css';
import '@fontsource/barlow/900.css';

/**
 * Web Font Loader for Next.js and browser environments
 * Uses CSS Font Loading API for optimal performance
 */
class WebFontLoader implements FontLoader {
  private loadingStatus: Map<string, FontLoadingStatus> = new Map();

  constructor() {
    // Initialize all fonts as loaded since we're importing them statically
    const fontFamilies = [
      'Inter', 'Poppins', 'Roboto', 'Montserrat', 'Playfair Display',
      'Merriweather', 'Bebas Neue', 'Oswald', 'Fredoka', 'Dancing Script',
      'Pacifico', 'Caveat', 'Amatic SC', 'Space Mono', 'Lato', 'Open Sans',
      'Raleway', 'Nunito', 'Quicksand', 'Barlow'
    ];

    fontFamilies.forEach(family => {
      this.loadingStatus.set(family.toLowerCase(), {
        isLoaded: true,
        isLoading: false,
      });
    });
  }

  async loadFont(font: FontConfig): Promise<void> {
    const { family, weight = '400', style = 'normal' } = font;
    const familyKey = family.toLowerCase();

    if (this.isLoaded(family)) {
      return;
    }

    this.loadingStatus.set(familyKey, {
      isLoaded: false,
      isLoading: true,
    });

    try {
      // Check if Font Loading API is available
      if ('fonts' in document) {
        await document.fonts.load(`${style} ${weight} 16px "${family}"`);
      }

      this.loadingStatus.set(familyKey, {
        isLoaded: true,
        isLoading: false,
      });
    } catch (error) {
      this.loadingStatus.set(familyKey, {
        isLoaded: false,
        isLoading: false,
        error: error as Error,
      });
      throw error;
    }
  }

  async loadFonts(fonts: FontConfig[]): Promise<void> {
    await Promise.all(fonts.map(font => this.loadFont(font)));
  }

  isLoaded(family: string): boolean {
    const status = this.loadingStatus.get(family.toLowerCase());
    return status?.isLoaded || false;
  }

  getLoadingStatus(family: string): FontLoadingStatus {
    return this.loadingStatus.get(family.toLowerCase()) || {
      isLoaded: false,
      isLoading: false,
    };
  }
}

// Font Context
interface FontContextValue {
  loader: FontLoader;
  loadFont: (font: FontConfig) => Promise<void>;
  loadFonts: (fonts: FontConfig[]) => Promise<void>;
  isLoaded: (family: string) => boolean;
  getLoadingStatus: (family: string) => FontLoadingStatus;
}

const FontContext = createContext<FontContextValue | null>(null);

// Font Provider Component
interface FontProviderProps {
  children: React.ReactNode;
  preloadFonts?: FontConfig[];
}

export const WebFontProvider: React.FC<FontProviderProps> = ({ 
  children, 
  preloadFonts = [] 
}) => {
  const [loader] = useState(() => new WebFontLoader());

  useEffect(() => {
    if (preloadFonts.length > 0) {
      loader.loadFonts(preloadFonts);
    }
  }, [loader, preloadFonts]);

  const value: FontContextValue = {
    loader,
    loadFont: (font) => loader.loadFont(font),
    loadFonts: (fonts) => loader.loadFonts(fonts),
    isLoaded: (family) => loader.isLoaded(family),
    getLoadingStatus: (family) => loader.getLoadingStatus(family),
  };

  return (
    <FontContext.Provider value={value}>
      {children}
    </FontContext.Provider>
  );
};

// Hook to use fonts
export const useWebFonts = () => {
  const context = useContext(FontContext);
  if (!context) {
    throw new Error('useWebFonts must be used within a WebFontProvider');
  }
  return context;
};

// Next.js specific font optimization
export const createNextFontCSS = (fontConfig: FontConfig): string => {
  const { family, weight = '400', style = 'normal', fallback = [] } = fontConfig;
  const fontFamily = [family, ...fallback].map(f => 
    f.includes(' ') ? `"${f}"` : f
  ).join(', ');

  return `
    font-family: ${fontFamily};
    font-weight: ${weight};
    font-style: ${style};
  `.trim();
};

// Font preloading utility for Next.js
export const generateFontPreloadLinks = (fonts: FontConfig[]): string[] => {
  const links: string[] = [];
  
  fonts.forEach(({ family, weight = '400' }) => {
    const familySlug = family.toLowerCase().replace(/\s+/g, '-');
    const fontPath = `/fonts/${familySlug}/${familySlug}-${weight}.woff2`;
    
    links.push(`<link rel="preload" href="${fontPath}" as="font" type="font/woff2" crossorigin>`);
  });

  return links;
};
