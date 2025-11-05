import { View, Text, TouchableOpacity } from 'react-native';
import React from 'react';

declare module '@nexpo/shared-components' {
  export const baseStyles: any;  // Add declaration for baseStyles object
  export const WebHeader: React.ComponentType;  // Add declaration for WebHeader component
  export const MobileNavBar: React.ComponentType<{navigation: any}>;  // Add declaration for MobileNavBar component
  export const View: typeof View;
  export const Text: typeof Text;
  export const TouchableOpacity: typeof TouchableOpacity;
  
  // Theme exports
  export function useTheme(): {
    theme: 'light' | 'dark';
    themeMode: 'light' | 'dark' | 'system';
    setThemeMode: (mode: 'light' | 'dark' | 'system') => void;
    colors: {
      background: string;
      surface: string;
      text: string;
      textSecondary: string;
      primary: string;
      primaryGradient: string;
      border: string;
      cardBackground: string;
      cardBorder: string;
    };
  };
  export const ThemeProvider: React.FC<{ children: React.ReactNode }>;
}
