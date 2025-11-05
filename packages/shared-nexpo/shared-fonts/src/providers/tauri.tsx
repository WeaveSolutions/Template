import React, { useEffect, useState } from 'react';
import { isDesktopApp } from '@nexpo/shared-utils/tauri';

interface TauriFontProviderProps {
  children: React.ReactNode;
}

/**
 * Font provider for Tauri desktop applications
 * Handles font loading and fallbacks for desktop environments
 */
export const TauriFontProvider: React.FC<TauriFontProviderProps> = ({ children }) => {
  const [fontsLoaded, setFontsLoaded] = useState(false);

  useEffect(() => {
    if (isDesktopApp()) {
      // In Tauri, fonts are typically bundled with the app
      // Check if fonts are available and loaded
      const checkFonts = async () => {
        try {
          // Add any font loading logic here if needed
          // For now, we'll assume fonts are immediately available
          setFontsLoaded(true);
        } catch (error) {
          console.warn('Font loading failed:', error);
          setFontsLoaded(true); // Fallback to system fonts
        }
      };

      checkFonts();
    } else {
      // Not in Tauri, fonts should be loaded by the web environment
      setFontsLoaded(true);
    }
  }, []);

  if (!fontsLoaded) {
    return (
      <div style={{ 
        display: 'flex', 
        justifyContent: 'center', 
        alignItems: 'center', 
        height: '100vh',
        fontFamily: 'system-ui, -apple-system, sans-serif'
      }}>
        Loading fonts...
      </div>
    );
  }

  return <>{children}</>;
};

export default TauriFontProvider;
