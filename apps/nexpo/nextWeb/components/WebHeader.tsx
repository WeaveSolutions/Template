import React, { useState } from 'react';
import { View, Text, TouchableOpacity, Platform, StyleSheet } from 'react-native';
import { useRouter } from 'solito/router';
import { useTheme } from '@shared/components';

const WebHeader = () => {
  const router = useRouter();
  const { theme, toggleTheme, colors } = useTheme();
  const [menuOpen, setMenuOpen] = useState(false);

  const menuItems = [
    { label: 'Home', path: '/' },
    { label: 'Dashboard', path: '/dashboard' },
    { label: 'Profile', path: '/profile' },
    { label: 'Web Exclusive', path: '/web-exclusive' },
  ];

  return (
    <View style={[styles.header, { backgroundColor: colors.surface }]}>
      <View style={styles.headerContent}>
        <TouchableOpacity onPress={() => router.push('/')}>
          <Text style={[styles.logo, { color: colors.text }]}>Solito App</Text>
        </TouchableOpacity>

        {/* Desktop Navigation */}
        <View style={styles.desktopNav}>
          {menuItems.map((item) => (
            <TouchableOpacity
              key={item.path}
              onPress={() => router.push(item.path)}
              style={styles.navItem}
            >
              <Text style={[styles.navText, { color: colors.text }]}>{item.label}</Text>
            </TouchableOpacity>
          ))}
          <TouchableOpacity onPress={toggleTheme} style={styles.themeToggle}>
            <Text style={styles.themeIcon}>{theme === 'dark' ? '🌞' : '🌙'}</Text>
          </TouchableOpacity>
        </View>

        {/* Mobile Menu Button */}
        <TouchableOpacity
          style={styles.mobileMenuButton}
          onPress={() => setMenuOpen(!menuOpen)}
        >
          <Text style={[styles.menuIcon, { color: colors.text }]}>☰</Text>
        </TouchableOpacity>
      </View>

      {/* Mobile Menu */}
      {menuOpen && (
        <View style={[styles.mobileMenu, { backgroundColor: colors.surface }]}>
          {menuItems.map((item) => (
            <TouchableOpacity
              key={item.path}
              onPress={() => {
                router.push(item.path);
                setMenuOpen(false);
              }}
              style={styles.mobileMenuItem}
            >
              <Text style={[styles.mobileMenuText, { color: colors.text }]}>{item.label}</Text>
            </TouchableOpacity>
          ))}
          <TouchableOpacity onPress={toggleTheme} style={styles.mobileMenuItem}>
            <Text style={[styles.mobileMenuText, { color: colors.text }]}>
              {theme === 'dark' ? '🌞 Light Mode' : '🌙 Dark Mode'}
            </Text>
          </TouchableOpacity>
        </View>
      )}
    </View>
  );
};

const styles = StyleSheet.create({
  header: {
    ...Platform.select({
      web: {
        position: 'sticky' as any,
        top: 0,
        zIndex: 1000,
        boxShadow: '0 2px 4px rgba(0,0,0,0.1)',
      },
      default: {
        elevation: 4,
        shadowColor: '#000',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.1,
        shadowRadius: 4,
      },
    }),
  },
  headerContent: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
    padding: 16,
    maxWidth: 1200,
    width: '100%',
    marginHorizontal: 'auto' as any,
  },
  logo: {
    fontSize: 24,
    fontWeight: 'bold',
  },
  desktopNav: {
    flexDirection: 'row',
    alignItems: 'center',
    gap: 24,
    display: Platform.OS === 'web' ? 'flex' : 'none',
  },
  navItem: {
    padding: 8,
  },
  navText: {
    fontSize: 16,
    fontWeight: '500',
  },
  themeToggle: {
    padding: 8,
  },
  themeIcon: {
    fontSize: 20,
  },
  mobileMenuButton: {
    padding: 8,
    display: Platform.OS === 'web' ? 'none' : 'flex',
  },
  menuIcon: {
    fontSize: 24,
  },
  mobileMenu: {
    position: 'absolute' as any,
    top: '100%',
    left: 0,
    right: 0,
    borderTopWidth: 1,
    borderTopColor: 'rgba(0,0,0,0.1)',
  },
  mobileMenuItem: {
    padding: 16,
    borderBottomWidth: 1,
    borderBottomColor: 'rgba(0,0,0,0.05)',
  },
  mobileMenuText: {
    fontSize: 16,
  },
});

export default WebHeader;
