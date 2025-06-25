import React, { createContext, useContext, useEffect, useState } from 'react';
import { useColorScheme, ViewProps } from 'react-native';

interface ThemeContextType {
  theme: 'light' | 'dark';
  toggleTheme: () => void;
  setTheme: (theme: 'light' | 'dark') => void;
  isDark: boolean;
}

const ThemeContext = createContext(undefined as any);

export const ThemeProvider = ({ children }: { children: any }) => {
  const systemColorScheme = useColorScheme();
  const [theme, setTheme] = useState(systemColorScheme);

  useEffect(() => {
    // Optionally sync with system theme on change
    setTheme(systemColorScheme);
  }, [systemColorScheme]);

  const toggleTheme = () => {
    setTheme((prevTheme: 'light' | 'dark') => (prevTheme === 'light' ? 'dark' : 'light'));
  };

  return (
    <ThemeContext.Provider
      value={{
        theme,
        toggleTheme,
        setTheme,
        isDark: theme === 'dark',
      }}
    >
      {children}
    </ThemeContext.Provider>
  );
};

export const useTheme = () => {
  const context = useContext(ThemeContext);
  if (context === undefined) {
    throw new Error('useTheme must be used within a ThemeProvider');
  }
  return context;
};

// Utility to apply theme-based styles
export const getThemeClassName = (theme: 'light' | 'dark', lightClass: string, darkClass: string) => {
  return theme === 'light' ? lightClass : darkClass;
};
