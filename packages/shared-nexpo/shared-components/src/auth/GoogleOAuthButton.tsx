import React from 'react';
import { View, TouchableOpacity, Text, ActivityIndicator, StyleSheet, Platform, Image } from 'react-native';
import { useAuth } from '@nexpo/shared-provider';

interface GoogleOAuthButtonProps {
  mode: 'signin' | 'signup';
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
    backgroundColor: '#fff',
    borderWidth: 1,
    borderColor: '#ddd',
    ...Platform.select({
      web: {
        cursor: 'pointer',
        boxShadow: '0 2px 4px rgba(0,0,0,0.1)',
      },
      default: {
        elevation: 2,
        shadowColor: '#000',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.1,
        shadowRadius: 4,
      },
    }),
  },
  buttonDisabled: {
    backgroundColor: '#f5f5f5',
    borderColor: '#e0e0e0',
  },
  icon: {
    width: 20,
    height: 20,
    marginRight: 12,
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
  successText: {
    marginTop: 8,
    textAlign: 'center',
    color: '#4CAF50',
    fontSize: 14,
  },
});

// Google logo as base64 (simplified SVG)
const GOOGLE_LOGO = 'data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMTgiIGhlaWdodD0iMTgiIHZpZXdCb3g9IjAgMCAxOCAxOCIgZmlsbD0ibm9uZSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj4KPHBhdGggZmlsbC1ydWxlPSJldmVub2RkIiBjbGlwLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0xNy42NCA5LjIwNDU0QzE3LjY0IDguNTY2MzYgMTcuNTgyNyA3Ljk1MjczIDE3LjQ3NjQgNy4zNjM2NEg5VjEwLjg0NUgxMy44NDM2QzEzLjYzNSAxMS45NyAxMy4wMDA5IDEyLjkyMzIgMTIuMDQ3NyAxMy41NjE0VjE1LjgxOTVIMTQuOTU2NEMxNi42NTgyIDE0LjI1MjcgMTcuNjQgMTEuOTQ1NCAxNy42NCA5LjIwNDU0WiIgZmlsbD0iIzQyODVGNCIvPgo8cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGNsaXAtcnVsZT0iZXZlbm9kZCIgZD0iTTkgMThDMTEuNDMgMTggMTMuNDY3MyAxNy4xOTQxIDE0Ljk1NjQgMTUuODE5NUwxMi4wNDc3IDEzLjU2MTRDMTEuMjQxOCAxNC4xMDE0IDEwLjIxMDkgMTQuNDIwNSA5IDE0LjQyMDVDNi42NTU5MSAxNC40MjA1IDQuNjcxODIgMTIuODM3MyAzLjk2NDA5IDEwLjcxSDC45NzIyNzNWMTMuMDQxOEMyLjQ1ODE4IDE1Ljk4MzIgNS41MDM2NCAxOCA5IDE4WiIgZmlsbD0iIzM0QTg1MyIvPgo8cGF0aCBmaWxsLXJ1bGU9ImV2ZW5vZGQiIGNsaXAtcnVsZT0iZXZlbm9kZCIgZD0iTTMuOTY0MDkgMTAuNzFDMy43ODQwOSAxMC4xNyAzLjY4MTgyIDkuNTkzMTggMy42ODE4MiA5QzMuNjgxODIgOC40MDY4MiAzLjc4NDA5IDcuODMgMy45NjQwOSA3LjI5VjQuOTU4MThIMC45NzIyNzNDMC4zNTQ1NDUgNi4xNzMxOCAwIDcuNTQ3NzMgMCA5QzAgMTAuNDUyMyAwLjM1NDU0NSAxMS44MjY4IDAuOTcyMjczIDEzLjA0MThMMy45NjQwOSAxMC43MVoiIGZpbGw9IiNGQkJDMDUiLz4KPHBhdGggZmlsbC1ydWxlPSJldmVub2RkIiBjbGlwLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik05IDMuNTc5NTVDMTAuMzIxNCAzLjU3OTU1IDExLjUwNzcgNC4wMzM2NCAxMi40NDA1IDQuOTI1NDVMNS4wMjI1NCAxMi41MDA1SDQuNTY3MjdWNS42MjczSDguNzY3MjdMOSA2LjU2MzY0VjYuMDM2MzZMOS4yMjcyNyA1LjgwOTA5TDkgNi4wMzYzNlY1LjU4MTgyTDguNzcyNzMgNS44MDkwOUw5IDYuMDM2MzZWMy41Nzk1NVoiIGZpbGw9IiNFQTQzMzUiLz4KPC9zdmc+';

export function GoogleOAuthButton({ mode, onSuccess, onError }: GoogleOAuthButtonProps) {
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [success, setSuccess] = useState(false);
  
  const { signInWithGoogle } = useAuth();

  const handleGoogleAuth = async () => {
    setError(null);
    setSuccess(false);

    try {
      setIsLoading(true);

      await signInWithGoogle({
        mode,
        scopes: ['openid', 'profile', 'email'],
        metadata: {
          authMethod: 'google-oauth2',
          signupMethod: mode === 'signup' ? 'google' : undefined,
        },
      });

      setSuccess(true);
      onSuccess?.();
    } catch (err) {
      const error = err as Error;
      setError(error.message || 'Google authentication failed');
      onError?.(error);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <View style={styles.container}>
      <TouchableOpacity
        style={[styles.button, isLoading && styles.buttonDisabled]}
        onPress={handleGoogleAuth}
        disabled={isLoading}
      >
        {isLoading ? (
          <ActivityIndicator color="#4285F4" />
        ) : (
          <>
            <Image source={{ uri: GOOGLE_LOGO }} style={styles.icon} />
            <Text style={styles.buttonText}>
              {mode === 'signin' ? 'Continue with Google' : 'Sign up with Google'}
            </Text>
          </>
        )}
      </TouchableOpacity>

      {error && (
        <View style={styles.errorContainer}>
          <Text style={styles.errorText}>{error}</Text>
        </View>
      )}

      {success && (
        <Text style={styles.successText}>
          Successfully authenticated with Google!
        </Text>
      )}
    </View>
  );
}
