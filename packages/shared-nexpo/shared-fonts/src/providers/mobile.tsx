import React, { createContext, useContext, useEffect, useState } from 'react';
import * as Font from 'expo-font';
import { FontLoader, FontConfig, FontLoadingStatus } from '../types';

// Import Expo Google Fonts
import {
  Inter_100Thin,
  Inter_200ExtraLight,
  Inter_300Light,
  Inter_400Regular,
  Inter_500Medium,
  Inter_600SemiBold,
  Inter_700Bold,
  Inter_800ExtraBold,
  Inter_900Black,
} from '@expo-google-fonts/inter';

import {
  Poppins_100Thin,
  Poppins_200ExtraLight,
  Poppins_300Light,
  Poppins_400Regular,
  Poppins_500Medium,
  Poppins_600SemiBold,
  Poppins_700Bold,
  Poppins_800ExtraBold,
  Poppins_900Black,
} from '@expo-google-fonts/poppins';

import {
  Roboto_100Thin,
  Roboto_300Light,
  Roboto_400Regular,
  Roboto_500Medium,
  Roboto_700Bold,
  Roboto_900Black,
} from '@expo-google-fonts/roboto';

import {
  Montserrat_100Thin,
  Montserrat_200ExtraLight,
  Montserrat_300Light,
  Montserrat_400Regular,
  Montserrat_500Medium,
  Montserrat_600SemiBold,
  Montserrat_700Bold,
  Montserrat_800ExtraBold,
  Montserrat_900Black,
} from '@expo-google-fonts/montserrat';

import {
  PlayfairDisplay_400Regular,
  PlayfairDisplay_500Medium,
  PlayfairDisplay_600SemiBold,
  PlayfairDisplay_700Bold,
  PlayfairDisplay_800ExtraBold,
  PlayfairDisplay_900Black,
} from '@expo-google-fonts/playfair-display';

import {
  Merriweather_300Light,
  Merriweather_400Regular,
  Merriweather_700Bold,
  Merriweather_900Black,
} from '@expo-google-fonts/merriweather';

import { BebasNeue_400Regular } from '@expo-google-fonts/bebas-neue';

import {
  Oswald_200ExtraLight,
  Oswald_300Light,
  Oswald_400Regular,
  Oswald_500Medium,
  Oswald_600SemiBold,
  Oswald_700Bold,
} from '@expo-google-fonts/oswald';

import {
  Fredoka_300Light,
  Fredoka_400Regular,
  Fredoka_500Medium,
  Fredoka_600SemiBold,
  Fredoka_700Bold,
} from '@expo-google-fonts/fredoka';

import {
  DancingScript_400Regular,
  DancingScript_500Medium,
  DancingScript_600SemiBold,
  DancingScript_700Bold,
} from '@expo-google-fonts/dancing-script';

import { Pacifico_400Regular } from '@expo-google-fonts/pacifico';

import {
  Caveat_400Regular,
  Caveat_500Medium,
  Caveat_600SemiBold,
  Caveat_700Bold,
} from '@expo-google-fonts/caveat';

import {
  AmaticSC_400Regular,
  AmaticSC_700Bold,
} from '@expo-google-fonts/amatic-sc';

import {
  SpaceMono_400Regular,
  SpaceMono_700Bold,
} from '@expo-google-fonts/space-mono';

import {
  Lato_100Thin,
  Lato_300Light,
  Lato_400Regular,
  Lato_700Bold,
  Lato_900Black,
} from '@expo-google-fonts/lato';

import {
  OpenSans_300Light,
  OpenSans_400Regular,
  OpenSans_500Medium,
  OpenSans_600SemiBold,
  OpenSans_700Bold,
  OpenSans_800ExtraBold,
} from '@expo-google-fonts/open-sans';

import {
  Raleway_100Thin,
  Raleway_200ExtraLight,
  Raleway_300Light,
  Raleway_400Regular,
  Raleway_500Medium,
  Raleway_600SemiBold,
  Raleway_700Bold,
  Raleway_800ExtraBold,
  Raleway_900Black,
} from '@expo-google-fonts/raleway';

import {
  Nunito_200ExtraLight,
  Nunito_300Light,
  Nunito_400Regular,
  Nunito_500Medium,
  Nunito_600SemiBold,
  Nunito_700Bold,
  Nunito_800ExtraBold,
  Nunito_900Black,
} from '@expo-google-fonts/nunito';

import {
  Quicksand_300Light,
  Quicksand_400Regular,
  Quicksand_500Medium,
  Quicksand_600SemiBold,
  Quicksand_700Bold,
} from '@expo-google-fonts/quicksand';

import {
  Barlow_100Thin,
  Barlow_200ExtraLight,
  Barlow_300Light,
  Barlow_400Regular,
  Barlow_500Medium,
  Barlow_600SemiBold,
  Barlow_700Bold,
  Barlow_800ExtraBold,
  Barlow_900Black,
} from '@expo-google-fonts/barlow';

// Font map for Expo
const FONT_MAP = {
  'Inter-100': Inter_100Thin,
  'Inter-200': Inter_200ExtraLight,
  'Inter-300': Inter_300Light,
  'Inter-400': Inter_400Regular,
  'Inter-500': Inter_500Medium,
  'Inter-600': Inter_600SemiBold,
  'Inter-700': Inter_700Bold,
  'Inter-800': Inter_800ExtraBold,
  'Inter-900': Inter_900Black,
  
  'Poppins-100': Poppins_100Thin,
  'Poppins-200': Poppins_200ExtraLight,
  'Poppins-300': Poppins_300Light,
  'Poppins-400': Poppins_400Regular,
  'Poppins-500': Poppins_500Medium,
  'Poppins-600': Poppins_600SemiBold,
  'Poppins-700': Poppins_700Bold,
  'Poppins-800': Poppins_800ExtraBold,
  'Poppins-900': Poppins_900Black,
  
  'Roboto-100': Roboto_100Thin,
  'Roboto-300': Roboto_300Light,
  'Roboto-400': Roboto_400Regular,
  'Roboto-500': Roboto_500Medium,
  'Roboto-700': Roboto_700Bold,
  'Roboto-900': Roboto_900Black,
  
  'Montserrat-100': Montserrat_100Thin,
  'Montserrat-200': Montserrat_200ExtraLight,
  'Montserrat-300': Montserrat_300Light,
  'Montserrat-400': Montserrat_400Regular,
  'Montserrat-500': Montserrat_500Medium,
  'Montserrat-600': Montserrat_600SemiBold,
  'Montserrat-700': Montserrat_700Bold,
  'Montserrat-800': Montserrat_800ExtraBold,
  'Montserrat-900': Montserrat_900Black,
  
  'PlayfairDisplay-400': PlayfairDisplay_400Regular,
  'PlayfairDisplay-500': PlayfairDisplay_500Medium,
  'PlayfairDisplay-600': PlayfairDisplay_600SemiBold,
  'PlayfairDisplay-700': PlayfairDisplay_700Bold,
  'PlayfairDisplay-800': PlayfairDisplay_800ExtraBold,
  'PlayfairDisplay-900': PlayfairDisplay_900Black,
  
  'Merriweather-300': Merriweather_300Light,
  'Merriweather-400': Merriweather_400Regular,
  'Merriweather-700': Merriweather_700Bold,
  'Merriweather-900': Merriweather_900Black,
  
  'BebasNeue-400': BebasNeue_400Regular,
  
  'Oswald-200': Oswald_200ExtraLight,
  'Oswald-300': Oswald_300Light,
  'Oswald-400': Oswald_400Regular,
  'Oswald-500': Oswald_500Medium,
  'Oswald-600': Oswald_600SemiBold,
  'Oswald-700': Oswald_700Bold,
  
  'Fredoka-300': Fredoka_300Light,
  'Fredoka-400': Fredoka_400Regular,
  'Fredoka-500': Fredoka_500Medium,
  'Fredoka-600': Fredoka_600SemiBold,
  'Fredoka-700': Fredoka_700Bold,
  
  'DancingScript-400': DancingScript_400Regular,
  'DancingScript-500': DancingScript_500Medium,
  'DancingScript-600': DancingScript_600SemiBold,
  'DancingScript-700': DancingScript_700Bold,
  
  'Pacifico-400': Pacifico_400Regular,
  
  'Caveat-400': Caveat_400Regular,
  'Caveat-500': Caveat_500Medium,
  'Caveat-600': Caveat_600SemiBold,
  'Caveat-700': Caveat_700Bold,
  
  'AmaticSC-400': AmaticSC_400Regular,
  'AmaticSC-700': AmaticSC_700Bold,
  
  'SpaceMono-400': SpaceMono_400Regular,
  'SpaceMono-700': SpaceMono_700Bold,
  
  'Lato-100': Lato_100Thin,
  'Lato-300': Lato_300Light,
  'Lato-400': Lato_400Regular,
  'Lato-700': Lato_700Bold,
  'Lato-900': Lato_900Black,
  
  'OpenSans-300': OpenSans_300Light,
  'OpenSans-400': OpenSans_400Regular,
  'OpenSans-500': OpenSans_500Medium,
  'OpenSans-600': OpenSans_600SemiBold,
  'OpenSans-700': OpenSans_700Bold,
  'OpenSans-800': OpenSans_800ExtraBold,
  
  'Raleway-100': Raleway_100Thin,
  'Raleway-200': Raleway_200ExtraLight,
  'Raleway-300': Raleway_300Light,
  'Raleway-400': Raleway_400Regular,
  'Raleway-500': Raleway_500Medium,
  'Raleway-600': Raleway_600SemiBold,
  'Raleway-700': Raleway_700Bold,
  'Raleway-800': Raleway_800ExtraBold,
  'Raleway-900': Raleway_900Black,
  
  'Nunito-200': Nunito_200ExtraLight,
  'Nunito-300': Nunito_300Light,
  'Nunito-400': Nunito_400Regular,
  'Nunito-500': Nunito_500Medium,
  'Nunito-600': Nunito_600SemiBold,
  'Nunito-700': Nunito_700Bold,
  'Nunito-800': Nunito_800ExtraBold,
  'Nunito-900': Nunito_900Black,
  
  'Quicksand-300': Quicksand_300Light,
  'Quicksand-400': Quicksand_400Regular,
  'Quicksand-500': Quicksand_500Medium,
  'Quicksand-600': Quicksand_600SemiBold,
  'Quicksand-700': Quicksand_700Bold,
  
  'Barlow-100': Barlow_100Thin,
  'Barlow-200': Barlow_200ExtraLight,
  'Barlow-300': Barlow_300Light,
  'Barlow-400': Barlow_400Regular,
  'Barlow-500': Barlow_500Medium,
  'Barlow-600': Barlow_600SemiBold,
  'Barlow-700': Barlow_700Bold,
  'Barlow-800': Barlow_800ExtraBold,
  'Barlow-900': Barlow_900Black,
};

/**
 * Mobile Font Loader for Expo/React Native
 */
class MobileFontLoader implements FontLoader {
  private loadingStatus: Map<string, FontLoadingStatus> = new Map();
  private loadedFonts: Set<string> = new Set();

  async loadFont(font: FontConfig): Promise<void> {
    const { family, weight = '400' } = font;
    const fontKey = `${family.replace(/\s+/g, '')}-${weight}`;
    
    if (this.isLoaded(fontKey)) {
      return;
    }

    this.loadingStatus.set(fontKey, {
      isLoaded: false,
      isLoading: true,
    });

    try {
      const fontAsset = FONT_MAP[fontKey as keyof typeof FONT_MAP];
      
      if (!fontAsset) {
        throw new Error(`Font ${fontKey} not found in font map`);
      }

      await Font.loadAsync({
        [fontKey]: fontAsset,
      });

      this.loadedFonts.add(fontKey);
      this.loadingStatus.set(fontKey, {
        isLoaded: true,
        isLoading: false,
      });
    } catch (error) {
      this.loadingStatus.set(fontKey, {
        isLoaded: false,
        isLoading: false,
        error: error as Error,
      });
      throw error;
    }
  }

  async loadFonts(fonts: FontConfig[]): Promise<void> {
    const fontMap: { [key: string]: any } = {};
    
    fonts.forEach(({ family, weight = '400' }) => {
      const fontKey = `${family.replace(/\s+/g, '')}-${weight}`;
      const fontAsset = FONT_MAP[fontKey as keyof typeof FONT_MAP];
      
      if (fontAsset && !this.isLoaded(fontKey)) {
        fontMap[fontKey] = fontAsset;
      }
    });

    if (Object.keys(fontMap).length === 0) {
      return;
    }

    try {
      await Font.loadAsync(fontMap);
      
      Object.keys(fontMap).forEach(fontKey => {
        this.loadedFonts.add(fontKey);
        this.loadingStatus.set(fontKey, {
          isLoaded: true,
          isLoading: false,
        });
      });
    } catch (error) {
      Object.keys(fontMap).forEach(fontKey => {
        this.loadingStatus.set(fontKey, {
          isLoaded: false,
          isLoading: false,
          error: error as Error,
        });
      });
      throw error;
    }
  }

  isLoaded(family: string): boolean {
    // Check both with and without weight
    if (this.loadedFonts.has(family)) {
      return true;
    }
    
    // Check if it's a family name without weight
    const familyBase = family.replace(/\s+/g, '');
    return Array.from(this.loadedFonts).some(font => 
      font.startsWith(`${familyBase}-`)
    );
  }

  getLoadingStatus(family: string): FontLoadingStatus {
    const status = this.loadingStatus.get(family);
    if (status) {
      return status;
    }

    // Check if it's loaded with a different weight
    const familyBase = family.replace(/\s+/g, '');
    const loadedFont = Array.from(this.loadedFonts).find(font => 
      font.startsWith(`${familyBase}-`)
    );

    if (loadedFont) {
      return { isLoaded: true, isLoading: false };
    }

    return { isLoaded: false, isLoading: false };
  }
}

// Font Context
interface FontContextValue {
  loader: FontLoader;
  loadFont: (font: FontConfig) => Promise<void>;
  loadFonts: (fonts: FontConfig[]) => Promise<void>;
  isLoaded: (family: string) => boolean;
  getLoadingStatus: (family: string) => FontLoadingStatus;
  fontsLoaded: boolean;
}

const FontContext = createContext<FontContextValue | null>(null);

// Font Provider Component
interface MobileFontProviderProps {
  children: React.ReactNode;
  preloadFonts?: FontConfig[];
}

export const MobileFontProvider: React.FC<MobileFontProviderProps> = ({ 
  children, 
  preloadFonts = [] 
}) => {
  const [loader] = useState(() => new MobileFontLoader());
  const [fontsLoaded, setFontsLoaded] = useState(false);

  useEffect(() => {
    const loadInitialFonts = async () => {
      if (preloadFonts.length > 0) {
        try {
          await loader.loadFonts(preloadFonts);
          setFontsLoaded(true);
        } catch (error) {
          console.error('Error loading fonts:', error);
          setFontsLoaded(true); // Continue anyway
        }
      } else {
        setFontsLoaded(true);
      }
    };

    loadInitialFonts();
  }, [loader, preloadFonts]);

  const value: FontContextValue = {
    loader,
    loadFont: (font) => loader.loadFont(font),
    loadFonts: (fonts) => loader.loadFonts(fonts),
    isLoaded: (family) => loader.isLoaded(family),
    getLoadingStatus: (family) => loader.getLoadingStatus(family),
    fontsLoaded,
  };

  return (
    <FontContext.Provider value={value}>
      {children}
    </FontContext.Provider>
  );
};

// Hook to use fonts
export const useMobileFonts = () => {
  const context = useContext(FontContext);
  if (!context) {
    throw new Error('useMobileFonts must be used within a MobileFontProvider');
  }
  return context;
};

// Utility to get font family name for React Native
export const getFontFamily = (family: string, weight: string = '400'): string => {
  return `${family.replace(/\s+/g, '')}-${weight}`;
};
