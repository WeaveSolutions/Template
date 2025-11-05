import React from 'react';
import { View, Text, TouchableOpacity, Image, StyleSheet } from 'react-native';
import { useAuth0 } from '@nexpo/shared-provider';

const styles = StyleSheet.create({
  container: {
    alignItems: 'center',
    padding: 20,
  },
  avatar: {
    width: 100,
    height: 100,
    borderRadius: 50,
    marginBottom: 16,
  },
  name: {
    fontSize: 24,
    fontWeight: 'bold',
    marginBottom: 8,
    textAlign: 'center',
  },
  email: {
    fontSize: 16,
    color: '#666',
    marginBottom: 16,
    textAlign: 'center',
  },
  loadingText: {
    fontSize: 16,
    color: '#666',
    textAlign: 'center',
  },
});

export function UserProfile() {
  const { user, isLoading } = useAuth0();

  if (isLoading) {
    return (
      <View style={styles.container}>
        <Text style={styles.loadingText}>Loading user profile...</Text>
      </View>
    );
  }

  if (!user) {
    return (
      <View style={styles.container}>
        <Text style={styles.loadingText}>Not logged in</Text>
      </View>
    );
  }

  return (
    <View style={styles.container}>
      {user.picture && (
        <Image
          source={{ uri: user.picture }}
          style={styles.avatar}
          accessibilityLabel="User profile picture"
        />
      )}
      <Text style={styles.name}>
        {user.name || 'Anonymous User'}
      </Text>
      {user.email && (
        <Text style={styles.email}>
          {user.email}
        </Text>
      )}
    </View>
  );
}
