import { useState, useEffect } from 'react';

export interface ThemeConfig {
  mode: 'light' | 'dark';
  colors: {
    background: string;
    text: string;
    primary: string;
    secondary: string;
  };
}

export const useTheme = () => {
  const [theme, setTheme] = useState<ThemeConfig>({
    mode: 'light',
    colors: {
      background: '#ffffff',
      text: '#000000',
      primary: '#3b82f6',
      secondary: '#10b981',
    }
  });

  const toggleTheme = () => {
    setTheme(prev => ({
      mode: prev.mode === 'light' ? 'dark' : 'light',
      colors: prev.mode === 'light' 
        ? {
            background: '#1f2937',
            text: '#ffffff',
            primary: '#60a5fa',
            secondary: '#34d399',
          }
        : {
            background: '#ffffff',
            text: '#000000',
            primary: '#3b82f6',
            secondary: '#10b981',
          }
    }));
  };

  useEffect(() => {
    // Check for system preference
    const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
    if (prefersDark) {
      setTheme({
        mode: 'dark',
        colors: {
          background: '#1f2937',
          text: '#ffffff',
          primary: '#60a5fa',
          secondary: '#34d399',
        }
      });
    }
  }, []);

  return { theme, toggleTheme };
};
