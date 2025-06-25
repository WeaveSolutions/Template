import React from 'react';
import { View, Text, ScrollView, StyleSheet, TouchableOpacity } from 'react-native';
import { useTheme } from '@shared/ui';
import MobileNavBar from '../components/MobileNavBar';
import { useNavigation } from '@react-navigation/native';
import { NativeStackNavigationProp } from '@react-navigation/native-stack';
import { RootStackParamList } from '../types/navigation';

type NavigationProp = NativeStackNavigationProp<RootStackParamList>;

export function ExclusiveScreen() {
  const { theme } = useTheme();
  const navigation = useNavigation<NavigationProp>();

  const styles = StyleSheet.create({
    container: {
      flex: 1,
      backgroundColor: theme.colors.background,
    },
    content: {
      flex: 1,
      padding: 20,
      paddingBottom: 80, // Space for nav bar
    },
    title: {
      fontSize: 32,
      fontWeight: 'bold',
      color: theme.colors.text,
      marginBottom: 16,
      textAlign: 'center',
    },
    subtitle: {
      fontSize: 18,
      color: theme.colors.secondary,
      marginBottom: 32,
      textAlign: 'center',
      lineHeight: 26,
    },
    featureCard: {
      backgroundColor: theme.colors.background,
      padding: 24,
      borderRadius: 16,
      marginBottom: 16,
      borderWidth: 1,
      borderColor: '#e5e7eb',
    },
    featureTitle: {
      fontSize: 20,
      fontWeight: 'bold',
      color: theme.colors.text,
      marginBottom: 8,
    },
    featureDescription: {
      fontSize: 16,
      color: theme.colors.secondary,
      lineHeight: 22,
    },
    icon: {
      fontSize: 40,
      marginBottom: 12,
      textAlign: 'center',
    },
  });

  const mobileFeatures = [
    { 
      icon: '📱', 
      title: 'Native Performance', 
      description: 'Leverages platform-specific APIs for optimal performance on iOS and Android.' 
    },
    { 
      icon: '🔔', 
      title: 'Push Notifications', 
      description: 'Native push notification support with rich media and deep linking.' 
    },
    { 
      icon: '📸', 
      title: 'Camera & Gallery', 
      description: 'Access device camera and photo library with native permissions handling.' 
    },
    { 
      icon: '📍', 
      title: 'Location Services', 
      description: 'GPS and location tracking with background location updates.' 
    },
    { 
      icon: '🎨', 
      title: 'Native Gestures', 
      description: 'Smooth, responsive touch interactions with platform-specific animations.' 
    },
    { 
      icon: '💾', 
      title: 'Offline Storage', 
      description: 'Secure local storage with SQLite and encrypted data persistence.' 
    },
  ];

  return (
    <View style={styles.container}>
      <ScrollView style={styles.content}>
        <Text style={styles.title}>Mobile Exclusive Features</Text>
        <Text style={styles.subtitle}>
          Experience native capabilities that make your app truly mobile
        </Text>

        {mobileFeatures.map((feature, index) => (
          <View key={index} style={styles.featureCard}>
            <Text style={styles.icon}>{feature.icon}</Text>
            <Text style={styles.featureTitle}>{feature.title}</Text>
            <Text style={styles.featureDescription}>{feature.description}</Text>
          </View>
        ))}
      </ScrollView>
      <MobileNavBar />
    </View>
  );
}

export default ExclusiveScreen;
