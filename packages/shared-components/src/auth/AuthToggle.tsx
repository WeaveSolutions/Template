import React, { useState } from 'react';
import { View, Text, TouchableOpacity, StyleSheet, Platform } from 'react-native';
import { EmailPasswordAuth } from './EmailPasswordAuth';
import { GoogleOAuthButton } from './GoogleOAuthButton';
import { PasskeyButton } from './PasskeyButton';
import { SocialConnections } from './SocialConnections';

interface AuthToggleProps {
  onSuccess?: () => void;
  onError?: (error: Error) => void;
  showPasskeys?: boolean;
  showSocialConnections?: boolean;
  mode?: 'signin' | 'signup';
}

const styles = StyleSheet.create({
  container: {
    width: '100%',
    maxWidth: 400,
    alignSelf: 'center',
    padding: 20,
  },
  toggleContainer: {
    flexDirection: 'row',
    marginBottom: 20,
    borderRadius: 8,
    backgroundColor: '#f0f0f0',
    padding: 2,
  },
  toggleButton: {
    flex: 1,
    paddingVertical: 12,
    paddingHorizontal: 16,
    alignItems: 'center',
    borderRadius: 6,
  },
  activeToggle: {
    backgroundColor: '#ffffff',
    ...Platform.select({
      web: {
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
  toggleText: {
    fontSize: 14,
    fontWeight: '500',
    color: '#666',
  },
  activeToggleText: {
    color: '#007AFF',
    fontWeight: '600',
  },
  dividerContainer: {
    flexDirection: 'row',
    alignItems: 'center',
    marginVertical: 20,
  },
  dividerLine: {
    flex: 1,
    height: 1,
    backgroundColor: '#e0e0e0',
  },
  dividerText: {
    marginHorizontal: 16,
    color: '#666',
    fontSize: 14,
  },
  passkeysContainer: {
    marginTop: 20,
  },
  modeToggle: {
    marginTop: 16,
    alignItems: 'center',
  },
  modeToggleText: {
    color: '#007AFF',
    fontSize: 14,
  },
});

export function AuthToggle({ 
  onSuccess, 
  onError, 
  showPasskeys = true,
  showSocialConnections = true,
  mode: initialMode = 'signin' 
}: AuthToggleProps) {
  const [authMethod, setAuthMethod] = useState<'email' | 'google'>('email');
  const [mode, setMode] = useState<'signin' | 'signup'>(initialMode);

  const renderDivider = () => (
    <View style={styles.dividerContainer}>
      <View style={styles.dividerLine} />
      <Text style={styles.dividerText}>or</Text>
      <View style={styles.dividerLine} />
    </View>
  );

  return (
    <View style={styles.container}>
      {/* Auth Method Toggle */}
      <View style={styles.toggleContainer}>
        <TouchableOpacity
          style={[styles.toggleButton, authMethod === 'email' && styles.activeToggle]}
          onPress={() => setAuthMethod('email')}
        >
          <Text style={[styles.toggleText, authMethod === 'email' && styles.activeToggleText]}>
            Email/Password
          </Text>
        </TouchableOpacity>
        <TouchableOpacity
          style={[styles.toggleButton, authMethod === 'google' && styles.activeToggle]}
          onPress={() => setAuthMethod('google')}
        >
          <Text style={[styles.toggleText, authMethod === 'google' && styles.activeToggleText]}>
            Google
          </Text>
        </TouchableOpacity>
      </View>

      {/* Auth Forms */}
      {authMethod === 'email' ? (
        <EmailPasswordAuth 
          mode={mode}
          onSuccess={onSuccess}
          onError={onError}
        />
      ) : (
        <GoogleOAuthButton
          mode={mode}
          onSuccess={onSuccess}
          onError={onError}
        />
      )}

      {/* Social Connections */}
      {(authMethod === 'email' && showSocialConnections) && (
        <>
          {renderDivider()}
          <SocialConnections
            onSuccess={onSuccess}
            onError={onError}
            showOnlyEnabled={true}
          />
        </>
      )}

      {/* Passkeys Option */}
      {showPasskeys && mode === 'signin' && (
        <>
          {renderDivider()}
          <View style={styles.passkeysContainer}>
            <PasskeyButton
              onSuccess={onSuccess}
              onError={onError}
            />
          </View>
        </>
      )}

      {/* Mode Toggle */}
      <TouchableOpacity
        style={styles.modeToggle}
        onPress={() => setMode(mode === 'signin' ? 'signup' : 'signin')}
      >
        <Text style={styles.modeToggleText}>
          {mode === 'signin' 
            ? "Don't have an account? Sign up" 
            : 'Already have an account? Sign in'}
        </Text>
      </TouchableOpacity>
    </View>
  );
}
