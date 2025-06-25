import React, { useEffect } from 'react';
import { 
  View, 
  Text, 
  StyleSheet, 
  Platform, 
  ScrollView, 
  ActivityIndicator
} from 'react-native';
import { useAuth } from '@shared/provider';
import { useRouter } from 'solito/router';
import { useTheme, WebHeader, Footer, AuthToggle } from '@shared/components';

function LoginScreen() {
  const { isAuthenticated, loading: authLoading } = useAuth();
  const { theme } = useTheme();
  const { colors } = theme;
  const router = useRouter();

  const styles = StyleSheet.create({
    container: {
      flex: 1,
      backgroundColor: colors.background,
      minHeight: Platform.OS === 'web' ? '100vh' as any : undefined,
    },
    scrollContent: {
      flexGrow: 1,
      justifyContent: 'center',
      padding: 20,
      paddingTop: Platform.OS === 'web' ? 100 : 20,
      paddingBottom: 40,
    },
    contentContainer: {
      width: '100%',
      maxWidth: 480,
      alignSelf: 'center',
      paddingVertical: 40,
    },
    logo: {
      fontSize: 48,
      marginBottom: 24,
      textAlign: 'center',
    },
    title: {
      fontSize: 32,
      fontWeight: 'bold',
      color: colors.text,
      marginBottom: 8,
      textAlign: 'center',
    },
    subtitle: {
      fontSize: 16,
      color: colors.textSecondary,
      marginBottom: 48,
      textAlign: 'center',
      lineHeight: 24,
    },
    authContainer: {
      width: '100%',
      marginBottom: 32,
    },
    features: {
      marginTop: 48,
      alignItems: 'center',
    },
    featureRow: {
      flexDirection: 'row',
      flexWrap: 'wrap',
      justifyContent: 'center',
      marginHorizontal: -8,
    },
    featureItem: {
      flexDirection: 'row',
      alignItems: 'center',
      marginBottom: 16,
      marginHorizontal: 8,
      paddingHorizontal: 16,
      paddingVertical: 8,
      backgroundColor: colors.surface,
      borderRadius: 20,
      borderWidth: 1,
      borderColor: colors.border,
    },
    featureIcon: {
      fontSize: 16,
      marginRight: 8,
    },
    featureText: {
      fontSize: 14,
      color: colors.text,
      fontWeight: '500',
    },
    footer: {
      marginTop: 48,
      alignItems: 'center',
    },
    footerText: {
      fontSize: 12,
      color: colors.textSecondary,
      textAlign: 'center',
      lineHeight: 20,
    },
    link: {
      color: colors.primary,
      textDecorationLine: 'underline',
    },
  });

  // Redirect if already authenticated
  useEffect(() => {
    if (isAuthenticated) {
      router.push('/dashboard');
    }
  }, [isAuthenticated, router]);

  if (authLoading) {
    return (
      <View style={[styles.container, { justifyContent: 'center', alignItems: 'center' }]}>
        <ActivityIndicator size="large" color={colors.primary} />
      </View>
    );
  }

  return (
    <View style={styles.container}>
      {Platform.OS === 'web' && <WebHeader />}
      
      <ScrollView contentContainerStyle={styles.scrollContent}>
        <View style={styles.contentContainer}>
          <Text style={styles.logo}>🏛️</Text>
          
          <Text style={styles.title}>
            Central Rank Authority
          </Text>
          <Text style={styles.subtitle}>
            Sign in to access your unified account and manage your digital identity
          </Text>

          <View style={styles.authContainer}>
            <AuthToggle 
              showPasskeys={true}
              showSocialConnections={true}              onSuccess={() => router.push('/dashboard')}
            />
          </View>

          <View style={styles.features}>
            <View style={styles.featureRow}>
              <View style={styles.featureItem}>
                <Text style={styles.featureIcon}>🔐</Text>
                <Text style={styles.featureText}>Secure Authentication</Text>
              </View>
              <View style={styles.featureItem}>
                <Text style={styles.featureIcon}>🌐</Text>
                <Text style={styles.featureText}>OAuth Providers</Text>
              </View>
              <View style={styles.featureItem}>
                <Text style={styles.featureIcon}>🔑</Text>
                <Text style={styles.featureText}>Passkeys</Text>
              </View>
              <View style={styles.featureItem}>
                <Text style={styles.featureIcon}>👤</Text>
                <Text style={styles.featureText}>Single Identity</Text>
              </View>
            </View>
          </View>

          <View style={styles.footer}>
            <Text style={styles.footerText}>
              By signing in, you agree to our{' '}
              <Text style={styles.link} onPress={() => router.push('/terms')}>
                Terms of Service
              </Text>{' '}
              and{' '}
              <Text style={styles.link} onPress={() => router.push('/privacy')}>
                Privacy Policy
              </Text>
            </Text>
          </View>
        </View>
      </ScrollView>
      
      <Footer />
    </View>
  );
}

export default LoginScreen;
