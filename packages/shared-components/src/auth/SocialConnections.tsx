import React from 'react';
import { View, Text, TouchableOpacity, StyleSheet, Platform } from 'react-native';
import { useAuth } from '@shared/provider';

interface SocialConnectionsProps {
  onSuccess?: () => void;
  onError?: (error: Error) => void;
  showOnlyEnabled?: boolean;
}

// Social provider configuration
const SOCIAL_PROVIDERS = [
  { id: 'apple', name: 'Apple', icon: '🍎', color: '#000' },
  { id: 'facebook', name: 'Facebook', icon: 'f', color: '#1877F2' },
  { id: 'microsoft', name: 'Microsoft', icon: 'M', color: '#0078D4' },
  { id: 'github', name: 'GitHub', icon: '🐙', color: '#333' },
  { id: 'discord', name: 'Discord', icon: '💬', color: '#5865F2' },
  { id: 'linkedin', name: 'LinkedIn', icon: 'in', color: '#0A66C2' },
  { id: 'twitter', name: 'X (Twitter)', icon: 'X', color: '#000' },
  { id: 'spotify', name: 'Spotify', icon: '🎵', color: '#1DB954' },
  { id: 'slack', name: 'Slack', icon: 'S', color: '#4A154B' },
  { id: 'reddit', name: 'Reddit', icon: '🤖', color: '#FF4500' },
  { id: 'amazon', name: 'Amazon', icon: 'A', color: '#FF9900' },
  { id: 'coderabbit', name: 'CodeRabbit', icon: '🐰', color: '#6366F1' },
];

const styles = StyleSheet.create({
  container: {
    width: '100%',
  },
  title: {
    fontSize: 14,
    color: '#666',
    textAlign: 'center',
    marginBottom: 12,
  },
  providersGrid: {
    flexDirection: 'row',
    flexWrap: 'wrap',
    gap: 8,
    justifyContent: 'center',
  },
  providerButton: {
    width: 48,
    height: 48,
    borderRadius: 24,
    alignItems: 'center',
    justifyContent: 'center',
    borderWidth: 1,
    borderColor: '#e0e0e0',
    backgroundColor: '#fff',
    ...Platform.select({
      web: {
        cursor: 'pointer',
        transition: 'all 0.2s',
      },
    }),
  },
  providerIcon: {
    fontSize: 20,
    fontWeight: 'bold',
  },
  providerButtonDisabled: {
    opacity: 0.5,
  },
  comingSoon: {
    position: 'absolute',
    top: -8,
    right: -8,
    backgroundColor: '#ff6b6b',
    paddingHorizontal: 6,
    paddingVertical: 2,
    borderRadius: 10,
  },
  comingSoonText: {
    fontSize: 10,
    color: 'white',
    fontWeight: 'bold',
  },
});

export function SocialConnections({ 
  onSuccess, 
  onError, 
  showOnlyEnabled = false 
}: SocialConnectionsProps) {
  const { signInWithProvider, getEnabledProviders } = useAuth();
  const enabledProviders = getEnabledProviders();

  // Filter providers based on enabled status
  const displayProviders = showOnlyEnabled 
    ? SOCIAL_PROVIDERS.filter(p => enabledProviders.includes(p.id))
    : SOCIAL_PROVIDERS;

  const handleProviderAuth = async (providerId: string) => {
    try {
      await signInWithProvider(providerId, {
        onSuccess,
        onError,
      });
    } catch (err) {
      onError?.(err as Error);
    }
  };

  if (displayProviders.length === 0) {
    return null;
  }

  return (
    <View style={styles.container}>
      {!showOnlyEnabled && (
        <Text style={styles.title}>Continue with</Text>
      )}
      
      <View style={styles.providersGrid}>
        {displayProviders.map((provider) => {
          const isEnabled = enabledProviders.includes(provider.id);
          const isComingSoon = !isEnabled && !showOnlyEnabled;
          
          return (
            <View key={provider.id}>
              <TouchableOpacity
                style={[
                  styles.providerButton,
                  !isEnabled && styles.providerButtonDisabled,
                  { borderColor: isEnabled ? provider.color : '#e0e0e0' }
                ]}
                onPress={() => handleProviderAuth(provider.id)}
                disabled={!isEnabled}
              >
                <Text style={[
                  styles.providerIcon,
                  { color: isEnabled ? provider.color : '#999' }
                ]}>
                  {provider.icon}
                </Text>
              </TouchableOpacity>
              
              {isComingSoon && (
                <View style={styles.comingSoon}>
                  <Text style={styles.comingSoonText}>Soon</Text>
                </View>
              )}
            </View>
          );
        })}
      </View>
    </View>
  );
}
