import React from 'react';
import { TouchableOpacity, Text, StyleSheet, View, ActivityIndicator, TextInput, Platform } from 'react-native';
import { useAuth0 } from '@shared/provider';
import { useRouter } from 'solito/router';

interface LoginButtonProps {
  containerStyle?: any;
  buttonStyle?: any;
  textStyle?: any;
  loadingColor?: string;
  onLoginSuccess?: () => void;
  onLoginError?: (error: Error) => void;
}

const styles = StyleSheet.create({
  container: {
    width: '100%',
    maxWidth: 400,
    alignSelf: 'center',
    padding: 20,
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
    marginTop: 8,
    ...Platform.select({
      web: {
        cursor: 'pointer',
      },
    }),
  },
  logoutButton: {
    backgroundColor: '#ff3b30',
  },
  loginButton: {
    backgroundColor: '#007AFF',
  },
  buttonText: {
    color: 'white',
    fontSize: 16,
    fontWeight: '600',
  },
  loadingContainer: {
    justifyContent: 'center',
    alignItems: 'center',
    padding: 16,
  },
  toggleButton: {
    marginTop: 16,
    padding: 8,
  },
  toggleText: {
    color: '#007AFF',
    textAlign: 'center',
    fontSize: 14,
  },
});

export function LoginButton({
  containerStyle,
  buttonStyle,
  textStyle,
  loadingColor = '#ffffff',
  onLoginSuccess,
  onLoginError,
}: LoginButtonProps) {
  const [email, setEmail] = React.useState('');
  const [password, setPassword] = React.useState('');
  const [isLogin, setIsLogin] = React.useState(true);
  const [isLoading, setIsLoading] = React.useState(false);
  const { loginWithRedirect, logout, isAuthenticated } = useAuth0();
  const router = useRouter();

  const handleAuth = async () => {
    try {
      setIsLoading(true);
      
      if (isLogin) {
        await loginWithRedirect();
        onLoginSuccess?.();
      } else {
        // Registration not supported with Auth0Context
        throw new Error('Registration is not supported. Please use the Auth0 dashboard to create accounts.');
      }
    } catch (error) {
      console.error('Auth error:', error);
      onLoginError?.(error as Error);
    } finally {
      setIsLoading(false);
    }
  };

  if (isLoading) {
    return (
      <View style={[styles.loadingContainer, containerStyle]}>
        <ActivityIndicator size="small" color={loadingColor} />
      </View>
    );
  }

  if (isAuthenticated) {
    return (
      <View style={[styles.container, containerStyle]}>
        <TouchableOpacity
          onPress={() => logout()}
          style={[styles.button, styles.logoutButton, buttonStyle]}
        >
          <Text style={[styles.buttonText, textStyle]}>Logout</Text>
        </TouchableOpacity>
      </View>
    );
  }

  return (
    <View style={[styles.container, containerStyle]}>
      <TextInput
        style={styles.input}
        placeholder="Email"
        value={email}
        onChangeText={setEmail}
        autoCapitalize="none"
        keyboardType="email-address"
        placeholderTextColor="#999"
      />
      <TextInput
        style={styles.input}
        placeholder="Password"
        value={password}
        onChangeText={setPassword}
        secureTextEntry
        placeholderTextColor="#999"
      />
      <TouchableOpacity
        onPress={handleAuth}
        style={[styles.button, styles.loginButton, buttonStyle]}
        disabled={!email || !password || isLoading}
      >
        <Text style={[styles.buttonText, textStyle]}>
          {isLogin ? 'Login' : 'Sign Up'}
        </Text>
      </TouchableOpacity>
      <TouchableOpacity
        onPress={() => setIsLogin(!isLogin)}
        style={styles.toggleButton}
      >
        <Text style={styles.toggleText}>
          {isLogin ? 'Need an account? Sign up' : 'Already have an account? Login'}
        </Text>
      </TouchableOpacity>
    </View>
  );
}
