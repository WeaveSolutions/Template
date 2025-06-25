import React, { useState, useEffect } from 'react';
import { View, TouchableOpacity, StyleSheet, Platform, Text } from 'react-native';

// Conditionally import React Navigation only on native platforms
let useNavigationRN: any = () => null;
let useRoute: any = () => ({ name: 'Home' });

if (Platform.OS !== 'web') {
  const navigation = require('@react-navigation/native');
  useNavigationRN = navigation.useNavigation;
  useRoute = navigation.useRoute;
}

// Use a more generic typing to avoid type issues
const useNavigation = () => {
  const navigation = useNavigationRN();
  return navigation as any;
};

interface NavigationType {
  navigate: (screen: string) => void;
  getCurrentRoute?: () => { name: string };
}

interface MobileNavBarProps {
  navigation?: NavigationType;
}

const styles = StyleSheet.create({
  container: {
    flexDirection: 'row',
    justifyContent: 'space-evenly',
    alignItems: 'center',
    backgroundColor: '#fff',
    borderTopWidth: 1,
    borderTopColor: '#eaeaea',
    paddingVertical: 10,
    width: '100%',
    ...(Platform.OS === 'web' ? { position: 'absolute' as const, bottom: 0, left: 0, right: 0 } : {}),
  },
  navItem: {
    alignItems: 'center',
    justifyContent: 'center',
  },
  navIcon: {
    width: 24,
    height: 24,
    marginBottom: 4,
  },
  navText: {
    fontSize: 12,
  },
  activeItem: {
    color: '#007AFF',
  },
});

const MobileNavBar = ({ navigation: propNavigation }: MobileNavBarProps) => {
  // Use the passed navigation prop if available, otherwise use the hook
  const navigation = propNavigation || useNavigation();
  const route = useRoute();
  const [activeRoute, setActiveRoute] = useState(route?.name || 'Home');

  useEffect(() => {
    // Update active route when it changes
    if (route?.name) {
      setActiveRoute(route.name);
    }
  }, [route?.name]);

  // Function to navigate to a specific screen
  const navigateTo = (screen: string) => {
    setActiveRoute(screen);
    navigation.navigate(screen);
  };

  if (Platform.OS === 'web') {
    return (
      <div style={{
        display: 'flex',
        flexDirection: 'row',
        justifyContent: 'space-evenly',
        alignItems: 'center',
        backgroundColor: '#fff',
        borderTop: '1px solid #eaeaea',
        padding: '10px 0',
        width: '100%',
        position: 'absolute',
        bottom: '0',
        left: '0',
        right: '0'
      }}>
        <div
          style={{
            cursor: 'pointer',
            textAlign: 'center',
            color: activeRoute === 'Home' ? '#007AFF' : 'inherit'
          }}
          onClick={() => navigateTo('Home')}
        >
          <div style={{ width: '24px', height: '24px', margin: '0 auto 4px' }}>🏠</div>
          <span style={{ fontSize: '12px' }}>Home</span>
        </div>
        <div
          style={{
            cursor: 'pointer',
            textAlign: 'center',
            color: activeRoute === 'Exclusive' ? '#007AFF' : 'inherit'
          }}
          onClick={() => navigateTo('Exclusive')}
        >
          <div style={{ width: '24px', height: '24px', margin: '0 auto 4px' }}>🔒</div>
          <span style={{ fontSize: '12px' }}>Exclusive</span>
        </div>
      </div>
    );
  }

  return (
    <View style={styles.container}>
      <TouchableOpacity
        style={styles.navItem}
        onPress={() => navigateTo('Home')}
      >
        <View style={styles.navIcon}>
          <Text style={{ fontSize: 24 }}>🏠</Text>
        </View>
        <Text style={[styles.navText, activeRoute === 'Home' && styles.activeItem]}>Home</Text>
      </TouchableOpacity>

      <TouchableOpacity
        style={styles.navItem}
        onPress={() => navigateTo('Exclusive')}
      >
        <View style={styles.navIcon}>
          <Text style={{ fontSize: 24 }}>🔒</Text>
        </View>
        <Text style={[styles.navText, activeRoute === 'Exclusive' && styles.activeItem]}>Exclusive</Text>
      </TouchableOpacity>
    </View>
  );
};

export default MobileNavBar;
