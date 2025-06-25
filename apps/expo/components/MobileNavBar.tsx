import React from 'react';
import { View, TouchableOpacity, Text, StyleSheet } from 'react-native';
import { useNavigation, useRoute } from '@react-navigation/native';
import { useTheme } from '@shared/ui';

export default function MobileNavBar() {
  const navigation = useNavigation();
  const route = useRoute();
  const { theme } = useTheme();

  const tabs = [
    { name: 'Home', icon: '🏠' },
    { name: 'Dashboard', icon: '📊' },
    { name: 'Exclusive', icon: '⭐' },
    { name: 'Profile', icon: '👤' },
  ];

  return (
    <View style={[styles.container, { backgroundColor: theme.colors.background }]}>
      {tabs.map((tab) => {
        const isActive = route.name === tab.name;
        return (
          <TouchableOpacity
            key={tab.name}
            style={styles.tab}
            onPress={() => navigation.navigate(tab.name as any)}
          >
            <Text style={[styles.icon, isActive && styles.activeIcon]}>
              {tab.icon}
            </Text>
            <Text style={[
              styles.label,
              { color: theme.colors.text },
              isActive && { color: theme.colors.primary }
            ]}>
              {tab.name}
            </Text>
          </TouchableOpacity>
        );
      })}
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flexDirection: 'row',
    justifyContent: 'space-evenly',
    alignItems: 'center',
    paddingBottom: 20,
    paddingTop: 10,
    borderTopWidth: 1,
    borderTopColor: 'rgba(0,0,0,0.1)',
    elevation: 8,
    shadowColor: '#000',
    shadowOffset: { width: 0, height: -2 },
    shadowOpacity: 0.1,
    shadowRadius: 4,
  },
  tab: {
    alignItems: 'center',
    justifyContent: 'center',
    flex: 1,
  },
  icon: {
    fontSize: 24,
    marginBottom: 4,
  },
  activeIcon: {
    transform: [{ scale: 1.1 }],
  },
  label: {
    fontSize: 12,
  },
});
