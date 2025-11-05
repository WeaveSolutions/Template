import React from 'react';
import { View, Text, TextInput, TouchableOpacity, StyleSheet, Platform } from 'react-native';
import { useAuth } from '@nexpo/shared-provider';

interface EmailPasswordAuthProps {
  mode: 'signin' | 'signup';
  onSuccess?: () => void;
  onError?: (error: Error) => void;
}

const styles = StyleSheet.create({
  container: {
    width: '100%',
  },
  input: {
    width: '100%',
    height: 50,
    borderWidth: 1,
    borderColor: '#ddd',
    borderRadius: 8,
    marginBottom: 16,
    paddingHorizontal: 16,
    fontSize: 16,
    backgroundColor: '#fff',
    color: '#333',
  },
  button: {
    width: '100%',
    height: 50,
    borderRadius: 8,
    alignItems: 'center',
    justifyContent: 'center',
    backgroundColor: '#007AFF',
    ...Platform.select({
      web: {
        cursor: 'pointer',
      },
    }),
  },
  buttonDisabled: {
    backgroundColor: '#ccc',
  },
  buttonText: {
    color: 'white',
    fontSize: 16,
    fontWeight: '600',
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
  nameContainer: {
    flexDirection: 'row',
    gap: 8,
  },
  nameInput: {
    flex: 1,
    height: 50,
    borderWidth: 1,
    borderColor: '#ddd',
    borderRadius: 8,
    marginBottom: 16,
    paddingHorizontal: 16,
    fontSize: 16,
    backgroundColor: '#fff',
    color: '#333',
  },
});

export function EmailPasswordAuth({ mode, onSuccess, onError }: EmailPasswordAuthProps) {
  const [email, setEmail] = React.useState('');
  const [password, setPassword] = React.useState('');
  const [firstName, setFirstName] = React.useState('');
  const [lastName, setLastName] = React.useState('');
  const [isLoading, setIsLoading] = React.useState(false);
  const [error, setError] = React.useState<string | null>(null);
  
  const { signIn, signUp } = useAuth();

  const validateForm = () => {
    if (!email || !password) {
      setError('Email and password are required');
      return false;
    }
    
    if (mode === 'signup' && (!firstName || !lastName)) {
      setError('First and last name are required');
      return false;
    }

    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(email)) {
      setError('Please enter a valid email address');
      return false;
    }

    if (password.length < 8) {
      setError('Password must be at least 8 characters');
      return false;
    }

    return true;
  };

  const handleSubmit = async () => {
    setError(null);
    
    if (!validateForm()) {
      return;
    }

    try {
      setIsLoading(true);

      if (mode === 'signin') {
        await signIn({
          email,
          password,
        });
      } else {
        await signUp({
          email,
          password,
          firstName,
          lastName,
          metadata: {
            authMethod: 'email-password',
          },
        });
      }

      onSuccess?.();
    } catch (err) {
      const error = err as Error;
      setError(error.message || 'Authentication failed');
      onError?.(error);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <View style={styles.container}>
      {mode === 'signup' && (
        <View style={styles.nameContainer}>
          <TextInput
            style={styles.nameInput}
            placeholder="First Name"
            value={firstName}
            onChangeText={setFirstName}
            autoCapitalize="words"
            placeholderTextColor="#999"
            editable={!isLoading}
          />
          <TextInput
            style={styles.nameInput}
            placeholder="Last Name"
            value={lastName}
            onChangeText={setLastName}
            autoCapitalize="words"
            placeholderTextColor="#999"
            editable={!isLoading}
          />
        </View>
      )}
      
      <TextInput
        style={styles.input}
        placeholder="Email"
        value={email}
        onChangeText={setEmail}
        autoCapitalize="none"
        keyboardType="email-address"
        placeholderTextColor="#999"
        editable={!isLoading}
      />
      
      <TextInput
        style={styles.input}
        placeholder="Password"
        value={password}
        onChangeText={setPassword}
        secureTextEntry
        placeholderTextColor="#999"
        editable={!isLoading}
      />

      <TouchableOpacity
        style={[
          styles.button,
          (!email || !password || isLoading) && styles.buttonDisabled
        ]}
        onPress={handleSubmit}
        disabled={!email || !password || isLoading}
      >
        {isLoading ? (
          <ActivityIndicator color="white" />
        ) : (
          <Text style={styles.buttonText}>
            {mode === 'signin' ? 'Sign In' : 'Sign Up'}
          </Text>
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
