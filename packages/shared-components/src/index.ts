import React from 'react';
import { 
  Text, 
  View, 
  TouchableOpacity, 
  StyleSheet, 
  Platform, 
  Image, 
  TextInput, 
  ScrollView, 
  FlatList, 
  SectionList, 
  Modal, 
  Alert, 
  Dimensions, 
  PixelRatio, 
  PanResponder, 
  Animated, 
  Easing 
} from 'react-native';

// Re-export React Native components for cross-platform use
export { 
  Text, 
  View, 
  TouchableOpacity, 
  StyleSheet, 
  Platform, 
  Image, 
  TextInput, 
  ScrollView, 
  FlatList, 
  SectionList, 
  Modal, 
  Alert, 
  Dimensions, 
  PixelRatio, 
  PanResponder, 
  Animated, 
  Easing 
};

// Export all components from shared-components package
export * from './ThemeContext';
export * from './WebHeader';
export * from './MobileNavBar';
export * from './Footer';

// Export auth components
export * from './auth';

// Export Button component
export * from './Button';

// Base styles for consistent UI across platforms
export const platformBaseStyles = StyleSheet.create({
  container: {
    flex: 1,
    padding: Platform.OS === 'web' ? 20 : 10,
  },
  text: {
    fontSize: Platform.OS === 'web' ? 16 : 14,
  },
});
