import React, { createContext, useContext, useState, ReactNode, useEffect, useMemo, useReducer, useCallback } from 'react';

// Define types for theme colors and the theme object
interface Colors {
  background: string;
  text: string;
  textSecondary: string;
  primary: string;
  surface: string;
  border: string;
  cardBackground: string;
  cardBorder: string;
  // Add other color properties as needed
}

const lightColors: Colors = {
  background: '#ffffff',
  text: '#000000',
  textSecondary: '#666666',
  primary: '#007bff',
  surface: '#f8f9fa',
  border: '#e0e0e0',
  cardBackground: 'rgba(255, 255, 255, 0.8)',
  cardBorder: 'rgba(0, 0, 0, 0.1)',
};

const darkColors: Colors = {
  background: '#121212',
  text: '#ffffff',
  textSecondary: '#b0b0b0',
  primary: '#007bff', // Or a different primary for dark mode
  surface: '#1e1e1e',
  border: '#333333',
  cardBackground: 'rgba(30, 30, 30, 0.8)',
  cardBorder: 'rgba(255, 255, 255, 0.1)',
};

export interface Theme {
  mode: 'light' | 'dark'; // Actual current mode reflecting system or user choice
  colors: Colors;
  isDark: boolean;
}

export type ThemeMode = 'light' | 'dark' | 'system'; // User's preference

export interface ThemeContextProps {
  theme: Theme;
  themeMode: ThemeMode; // User's selected preference ('light', 'dark', or 'system')
  setThemeMode: (mode: ThemeMode) => void;
  toggleTheme: () => void;
}

// Default context value for SSR and before client-side hydration
const defaultSsrThemeContextValue: ThemeContextProps = {
  theme: {
    mode: 'light', // Sensible default for SSR (e.g., light theme)
    colors: lightColors,
    isDark: false,
  },
  themeMode: 'system', // Default preference
  setThemeMode: () => console.warn('setThemeMode called on SSR or before client mount'),
  toggleTheme: () => console.warn('toggleTheme called on SSR or before client mount'),
};

const ThemeContext = createContext<ThemeContextProps>(defaultSsrThemeContextValue);

export const useTheme = () => {
  const context = useContext(ThemeContext);
  if (context === undefined) {
    throw new Error('useTheme must be used within a ThemeProvider');
  }
  return context;
};

interface ThemeProviderProps {
  children: ReactNode;
}

// This is the internal component that will contain all hook-based logic
const ClientThemeProvider = ({ children }: ThemeProviderProps) => {
  const [hasMounted, setHasMounted] = useState(false);
  // Initialize with SSR defaults, will be updated client-side
  const [themeMode, setThemeModeState] = useState<ThemeMode>(defaultSsrThemeContextValue.themeMode);
  const [theme, setTheme] = useState<Theme>(defaultSsrThemeContextValue.theme);

  useEffect(() => {
    setHasMounted(true);
    // Load saved theme preference from localStorage after component has mounted
    const savedThemePreference = localStorage.getItem('themeMode') as ThemeMode | null;
    if (savedThemePreference && ['light', 'dark', 'system'].includes(savedThemePreference)) {
      setThemeModeState(savedThemePreference);
    }
    // If no saved preference, it defaults to 'system' (from initialState)
  }, []); // Runs once on mount

  // Effect to update the actual theme object based on themeMode (preference) and system settings
  useEffect(() => {
    if (!hasMounted) return; // Only run client-side after mount

    let currentActualMode: 'light' | 'dark';
    if (themeMode === 'system') {
      currentActualMode = window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
    } else {
      currentActualMode = themeMode; // 'light' or 'dark'
    }

    setTheme({
      mode: currentActualMode,
      colors: currentActualMode === 'dark' ? darkColors : lightColors,
      isDark: currentActualMode === 'dark',
    });
  }, [themeMode, hasMounted]); // Re-run if themeMode preference changes or after mount

  // Effect to listen for system theme changes if themeMode is 'system'
  useEffect(() => {
    if (!hasMounted || themeMode !== 'system') return;

    const mediaQuery = window.matchMedia('(prefers-color-scheme: dark)');
    const handleChange = () => {
      // When system preference changes and current mode is 'system',
      // we re-evaluate the theme. Setting themeMode to 'system' (even if it's already system)
      // will trigger the previous useEffect to update the theme object.
      setThemeModeState('system');
    };

    mediaQuery.addEventListener('change', handleChange);
    return () => mediaQuery.removeEventListener('change', handleChange);
  }, [themeMode, hasMounted]);

  const handleSetThemeMode = (newMode: ThemeMode) => {
    if (!hasMounted) return;
    localStorage.setItem('themeMode', newMode);
    setThemeModeState(newMode);
  };

  const handleToggleTheme = () => {
    if (!hasMounted) return;
    // When toggling, we explicitly set to 'light' or 'dark', not 'system'
    const newActualMode = theme.isDark ? 'light' : 'dark';
    handleSetThemeMode(newActualMode); // This will set themeMode to 'light' or 'dark'
  };

  // Provide fully functional handlers only after mount
  const contextValue: ThemeContextProps = {
    theme,
    themeMode,
    setThemeMode: hasMounted ? handleSetThemeMode : defaultSsrThemeContextValue.setThemeMode,
    toggleTheme: hasMounted ? handleToggleTheme : defaultSsrThemeContextValue.toggleTheme,
  };

  return (
    <ThemeContext.Provider value={contextValue}>
      {children}
    </ThemeContext.Provider>
  );
};

// This is the exported ThemeProvider. It is hook-free.
export const ThemeProvider = ({ children }: ThemeProviderProps) => {
  const isClientSide = typeof window !== 'undefined';

  if (!isClientSide) {
    // SSR: Provide a default, non-hook-based context.
    return (
      <ThemeContext.Provider value={defaultSsrThemeContextValue}>
        {children}
      </ThemeContext.Provider>
    );
  }

  // Client-side: Render the component that will use hooks.
  return <ClientThemeProvider>{children}</ClientThemeProvider>;
};

export default ThemeProvider;
