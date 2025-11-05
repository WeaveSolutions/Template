import React from 'react';
import { View, TouchableOpacity, Text, ActivityIndicator, StyleSheet, Platform } from 'react-native';
import { useAuth } from '@nexpo/shared-provider';

interface PasskeyButtonProps {
  onSuccess?: () => void;
  onError?: (error: Error) => void;
}

const styles = StyleSheet.create({
  container: {
    width: '100%',
  },
  button: {
    width: '100%',
    height: 50,
    borderRadius: 8,
    flexDirection: 'row',
    alignItems: 'center',
    justifyContent: 'center',
    backgroundColor: '#f8f8f8',
    borderWidth: 1,
    borderColor: '#ddd',
    ...Platform.select({
      web: {
        cursor: 'pointer',
      },
    }),
  },
  buttonDisabled: {
    backgroundColor: '#f0f0f0',
    borderColor: '#e0e0e0',
  },
  icon: {
    fontSize: 20,
    marginRight: 8,
  },
  buttonText: {
    color: '#333',
    fontSize: 16,
    fontWeight: '500',
  },
  errorContainer: {
    marginTop: 12,
    padding: 12,
    backgroundColor: '#fee',
    borderRadius: 8,
    borderWidth: 1,
    borderColor: '#fcc',
  },
  errorText: {
    color: '#c00',
    fontSize: 14,
  },
  notSupportedText: {
    textAlign: 'center',
    color: '#666',
    fontSize: 14,
    fontStyle: 'italic',
    marginTop: 8,
  },
});

export function PasskeyButton({ onSuccess, onError }: PasskeyButtonProps) {
  const [isLoading, setIsLoading] = React.useState(false);
  const [error, setError] = React.useState<string | null>(null);
  const [isSupported, setIsSupported] = React.useState(true);
  
  const { signInWithPasskey, isPasskeySupported } = useAuth();

  React.useEffect(() => {
    // Check if passkeys are supported on this device/browser
    const checkSupport = async () => {
      const supported = await isPasskeySupported();
      setIsSupported(supported);
    };
    
    checkSupport();
  }, [isPasskeySupported]);

  const handlePasskeyAuth = async () => {
    setError(null);

    try {
      setIsLoading(true);

      const result = await signInWithPasskey();
      
      if (result.success) {
        onSuccess?.();
      } else {
        throw new Error(result.error || 'Passkey authentication failed');
      }
    } catch (err) {
      const error = err as Error;
      
      // Handle specific passkey errors
      if (error.message.includes('cancelled')) {
        setError('Authentication cancelled');
      } else if (error.message.includes('not found')) {
        setError('No passkey found for this device');
      } else {
        setError(error.message || 'Passkey authentication failed');
      }
      
      onError?.(error);
    } finally {
      setIsLoading(false);
    }
  };

  if (!isSupported) {
    return (
      <View style={styles.container}>
        <Text style={styles.notSupportedText}>
          Passkeys are not supported on this device
        </Text>
      </View>
    );
  }

  return (
    <View style={styles.container}>
      <TouchableOpacity
        style={[styles.button, isLoading && styles.buttonDisabled]}
        onPress={handlePasskeyAuth}
        disabled={isLoading}
      >
        {isLoading ? (
          <ActivityIndicator color="#333" />
        ) : (
          <>
            <Text style={styles.icon}>🔑</Text>
            <Text style={styles.buttonText}>Sign in with Passkey</Text>
          </>
        )}
      </TouchableOpacity>

      {error && (
        <View style={styles.errorContainer}>
          <Text style={styles.errorText}>{error}</Text>
        </View>
      )}
    </View>
  );
}
