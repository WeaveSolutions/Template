import React, { useState } from 'react';
import { 
  View, 
  Text, 
  TextInput,
  TouchableOpacity, 
  StyleSheet, 
  Platform, 
  ScrollView, 
  ActivityIndicator
} from 'react-native';
import { useAuth } from '@shared/provider';
import { useRouter } from 'solito/router';
import { useTheme, WebHeader, Footer } from '@shared/components';
import { supabase } from '@shared/utils';

export default function ForgotPasswordScreen() {
  const [email, setEmail] = useState('');
  const [loading, setLoading] = useState(false);
  const [message, setMessage] = useState('');
  const [error, setError] = useState('');
  const [emailSent, setEmailSent] = useState(false);
  
  const router = useRouter();
  const { theme } = useTheme();
  const { colors } = theme;

  const handleResetPassword = async () => {
    if (!email) {
      setError('Please enter your email address');
      return;
    }

    setLoading(true);
    setError('');
    setMessage('');

    try {
      const { error } = await supabase.auth.resetPasswordForEmail(email, {
        redirectTo: `${Platform.OS === 'web' ? window.location.origin : 'exp://localhost:8081'}/reset-password`,
      });

      if (error) throw error;

      setEmailSent(true);
      setMessage('Check your email for the password reset link!');
    } catch (error: any) {
      setError(error.message || 'Failed to send reset email');
    } finally {
      setLoading(false);
    }
  };

  const styles = StyleSheet.create({
    container: {
      flex: 1,
      backgroundColor: colors.background,
    },
    keyboardView: {
      flex: 1,
    },
    scrollContent: {
      flexGrow: 1,
      justifyContent: 'center',
      padding: 20,
      paddingBottom: 0,
    },
    formContainer: {
      maxWidth: 400,
      width: '100%',
      alignSelf: 'center',
    },
    title: {
      fontSize: 28,
      fontWeight: 'bold',
      color: colors.text,
      marginBottom: 8,
      textAlign: 'center',
    },
    subtitle: {
      fontSize: 16,
      color: colors.textSecondary,
      marginBottom: 32,
      textAlign: 'center',
    },
    input: {
      backgroundColor: colors.surface,
      borderWidth: 1,
      borderColor: colors.border,
      borderRadius: 8,
      padding: 16,
      fontSize: 16,
      color: colors.text,
      marginBottom: 16,
    },
    button: {
      backgroundColor: colors.primary,
      padding: 16,
      borderRadius: 8,
      alignItems: 'center',
      marginBottom: 16,
    },
    buttonDisabled: {
      opacity: 0.7,
    },
    buttonText: {
      color: '#fff',
      fontSize: 16,
      fontWeight: '600',
    },
    backButton: {
      alignItems: 'center',
      padding: 12,
    },
    backButtonText: {
      color: colors.primary,
      fontSize: 16,
    },
    errorContainer: {
      backgroundColor: '#fee',
      borderWidth: 1,
      borderColor: '#fcc',
      borderRadius: 8,
      padding: 12,
      marginBottom: 16,
    },
    errorText: {
      color: '#c00',
      fontSize: 14,
      textAlign: 'center',
    },
    successContainer: {
      backgroundColor: '#efe',
      borderWidth: 1,
      borderColor: '#cfc',
      borderRadius: 8,
      padding: 12,
      marginBottom: 16,
    },
    successText: {
      color: '#060',
      fontSize: 14,
      textAlign: 'center',
    },
    successIcon: {
      fontSize: 48,
      marginBottom: 16,
      textAlign: 'center',
    },
  });

  return (
    <View style={styles.container}>
      {Platform.OS === 'web' && <WebHeader />}
      
      <View 
        style={styles.keyboardView}
      >
        <ScrollView contentContainerStyle={styles.scrollContent}>
          <View style={styles.formContainer}>
            {emailSent ? (
              <>
                <Text style={styles.successIcon}>✉️</Text>
                <Text style={styles.title}>Check Your Email</Text>
                <Text style={styles.subtitle}>
                  We've sent a password reset link to {email}
                </Text>
                
                {message && (
                  <View style={styles.successContainer}>
                    <Text style={styles.successText}>{message}</Text>
                  </View>
                )}

                <TouchableOpacity
                  style={styles.backButton}
                  onPress={() => router.push('/login')}
                >
                  <Text style={styles.backButtonText}>Back to Sign In</Text>
                </TouchableOpacity>
              </>
            ) : (
              <>
                <Text style={styles.title}>Reset Password</Text>
                <Text style={styles.subtitle}>
                  Enter your email address and we'll send you a link to reset your password
                </Text>

                {error && (
                  <View style={styles.errorContainer}>
                    <Text style={styles.errorText}>{error}</Text>
                  </View>
                )}

                <TextInput
                  style={styles.input}
                  placeholder="Email address"
                  placeholderTextColor={colors.textSecondary}
                  value={email}
                  onChangeText={setEmail}
                  keyboardType="email-address"
                  autoCapitalize="none"
                  editable={!loading}
                />

                <TouchableOpacity 
                  style={[styles.button, loading && styles.buttonDisabled]}
                  onPress={handleResetPassword}
                  disabled={loading}
                >
                  {loading ? (
                    <ActivityIndicator color="#fff" />
                  ) : (
                    <Text style={styles.buttonText}>Send Reset Link</Text>
                  )}
                </TouchableOpacity>

                <TouchableOpacity
                  style={styles.backButton}
                  onPress={() => router.push('/login')}
                  disabled={loading}
                >
                  <Text style={styles.backButtonText}>Back to Sign In</Text>
                </TouchableOpacity>
              </>
            )}
          </View>
        </ScrollView>
      </View>
      
      <Footer />
    </View>
  );
}
