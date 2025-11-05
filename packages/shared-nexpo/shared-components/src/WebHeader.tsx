import React, { useState, useEffect } from 'react';
import { View, Text, TouchableOpacity, StyleSheet, Platform, Dimensions } from 'react-native';
import { useTheme } from './ThemeContext';
import { useAuth0 } from '@nexpo/shared-provider';

// Get screen width for precise calculations - declared outside component
const windowWidth = typeof window !== 'undefined' ? window.innerWidth : Dimensions.get('window').width;

// Web-specific header component
export const WebHeader = () => {
  // Only render on web
  if (Platform.OS !== 'web') {
    return null;
  }

  const { theme, themeMode, setThemeMode } = useTheme();
  const { user, logout } = useAuth0();

  const navigateTo = (path: string) => {
    if (typeof window !== 'undefined') {
      window.location.href = path;
    }
  };

  const handleSignOut = async () => {
    await logout();
    navigateTo('/');
  };

  const cycleThemeMode = () => {
    if (themeMode === 'system') {
      setThemeMode('light');
    } else if (themeMode === 'light') {
      setThemeMode('dark');
    } else {
      setThemeMode('system');
    }
  };

  const getThemeIcon = () => {
    if (themeMode === 'system') return '🌐';
    if (themeMode === 'light') return '☀️';
    return '🌙';
  };

  const styles = StyleSheet.create({
    headerContainer: {
      width: '100%',
      flexDirection: 'row',
      justifyContent: 'space-between',
      alignItems: 'center',
      backgroundColor: theme.colors.surface,
      paddingVertical: 12,
      paddingHorizontal: 24,
      ...Platform.select({
        web: {
          boxShadow: `0px 2px 4px ${theme.mode === 'dark' ? 'rgba(0, 0, 0, 0.5)' : 'rgba(0, 0, 0, 0.15)'}`,
        },
        default: {
          shadowColor: '#000',
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: theme.mode === 'dark' ? 0.5 : 0.15,
          shadowRadius: 4,
          elevation: 4,
        }
      }),
      borderBottomWidth: 1,
      borderBottomColor: theme.colors.border,
      ...(Platform.OS === 'web' ? {
        position: 'fixed' as any, // Web-specific fixed position
        top: 0,
        left: 0,
        right: 0,
        zIndex: 100,
      } : {}),
    },
    leftSection: {
      width: '30%',
      flexDirection: 'row',
      alignItems: 'center',
      justifyContent: 'flex-start',
    },
    centerSection: {
      width: '40%',
      flexDirection: 'row',
      justifyContent: 'center',
      alignItems: 'center',
      ...(Platform.OS === 'web' ? { gap: 16 } : { marginHorizontal: 8 }),
    },
    rightSection: {
      width: '30%',
      flexDirection: 'row',
      justifyContent: 'flex-end',
      alignItems: 'center',
      ...(Platform.OS === 'web' ? { gap: 12 } : {}),
    },
    logoEmoji: {
      fontSize: 28,
      marginRight: 12,
    },
    logoText: {
      fontSize: 22,
      fontWeight: 'bold',
      color: theme.colors.primary,
    },
    navButton: {
      flexDirection: 'row',
      alignItems: 'center',
      backgroundColor: 'transparent',
      paddingVertical: 10,
      paddingHorizontal: 16,
      borderRadius: 8,
      marginHorizontal: 8,
      borderWidth: 1,
      borderColor: theme.colors.border,
      ...(Platform.OS === 'web' ? { transition: 'all 0.3s' } : {}),
    },
    buttonEmoji: {
      fontSize: 18,
      marginRight: 8,
    },
    buttonText: {
      fontSize: 16,
      fontWeight: '500',
      color: theme.colors.primary,
    },
    themeButton: {
      flexDirection: 'row',
      alignItems: 'center',
      backgroundColor: theme.colors.cardBackground,
      paddingVertical: 8,
      paddingHorizontal: 12,
      borderRadius: 20,
      borderWidth: 1,
      borderColor: theme.colors.cardBorder,
      ...(Platform.OS === 'web' ? { 
        transition: 'all 0.3s',
        cursor: 'pointer',
      } : {}),
    },
    themeIcon: {
      fontSize: 16,
      marginRight: 6,
    },
    themeText: {
      fontSize: 14,
      fontWeight: '500',
      color: theme.colors.textSecondary,
      textTransform: 'capitalize',
    },
    profileButton: {
      flexDirection: 'row',
      alignItems: 'center',
      backgroundColor: 'transparent',
      paddingVertical: 10,
      paddingHorizontal: 16,
      borderRadius: 8,
      borderWidth: 1,
      borderColor: theme.colors.border,
      ...(Platform.OS === 'web' ? { transition: 'all 0.3s' } : {}),
    },
    profileText: {
      fontSize: 16,
      fontWeight: '500',
      color: theme.colors.text,
    },
  });

  return (
    <View style={styles.headerContainer}>
      {/* Left section with logo */}
      <View style={styles.leftSection}>
        <Text style={styles.logoEmoji}>🌍</Text>
        <Text style={styles.logoText}>CrossPlatform</Text>
      </View>
      
      {/* Navigation section */}
      <View style={styles.centerSection}>
        <TouchableOpacity 
          style={styles.navButton}
          onPress={() => navigateTo('/')}
        >
          <Text style={styles.buttonEmoji}>🏠</Text>
          <Text style={styles.buttonText}>Home</Text>
        </TouchableOpacity>
        <TouchableOpacity 
          style={styles.navButton}
          onPress={() => navigateTo('/web-exclusive')}
        >
          <Text style={styles.buttonEmoji}>🌐</Text>
          <Text style={styles.buttonText}>Web</Text>
        </TouchableOpacity>
        {user && (
          <TouchableOpacity 
            style={styles.navButton}
            onPress={() => navigateTo('/dashboard')}
          >
            <Text style={styles.buttonEmoji}>📊</Text>
            <Text style={styles.buttonText}>Dashboard</Text>
          </TouchableOpacity>
        )}
        {user && (
          <TouchableOpacity 
            style={styles.navButton}
            onPress={() => navigateTo('/mindsdb')}
          >
            <Text style={styles.buttonEmoji}>🧠</Text>
            <Text style={styles.buttonText}>MindsDB</Text>
          </TouchableOpacity>
        )}
      </View>
      
      {/* Right section with theme toggle and profile */}
      <View style={styles.rightSection}>
        <TouchableOpacity
          style={styles.themeButton}
          onPress={cycleThemeMode}
        >
          <Text style={styles.themeIcon}>{getThemeIcon()}</Text>
          <Text style={styles.themeText}>{themeMode}</Text>
        </TouchableOpacity>
        
        {user ? (
          <>
            <TouchableOpacity 
              style={styles.profileButton}
              onPress={() => navigateTo('/profile')}
            >
              <Text style={styles.buttonEmoji}>👤</Text>
              <Text style={styles.profileText}>{user.email?.split('@')[0]}</Text>
            </TouchableOpacity>
            <TouchableOpacity 
              style={[styles.profileButton, { borderColor: '#dc2626' }]}
              onPress={handleSignOut}
            >
              <Text style={[styles.profileText, { color: '#dc2626' }]}>Sign Out</Text>
            </TouchableOpacity>
          </>
        ) : (
          <TouchableOpacity 
            style={styles.profileButton}
            onPress={() => navigateTo('/login')}
          >
            <Text style={styles.buttonEmoji}>🔑</Text>
            <Text style={styles.profileText}>Sign In</Text>
          </TouchableOpacity>
        )}
      </View>
    </View>
  );
};

// Export a base styles object for backward compatibility
export const baseStyles = {
  container: "flex-1 items-center justify-center p-4",
  title: "text-2xl font-bold text-blue-600 mb-2",
  text: "text-base text-gray-800",
};
