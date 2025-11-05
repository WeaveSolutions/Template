export * from './components/ui/button';
export * from './lib/utils';
export * from './components/SEO';

// Export theme functionality with explicit exports to avoid conflicts
export { ThemeProvider, getThemeClassName } from './context/ThemeContext';
export { useTheme as useThemeContext } from './context/ThemeContext';

// Export standalone theme hook
export { useTheme, type ThemeConfig } from './hooks/useTheme';

// Define a default Theme object for usage in applications
export const Theme = {
  light: {
    background: '#ffffff',
    text: '#000000',
    primary: '#3b82f6',
    secondary: '#10b981',
  },
  dark: {
    background: '#1f2937',
    text: '#ffffff',
    primary: '#60a5fa',
    secondary: '#34d399',
  }
};
