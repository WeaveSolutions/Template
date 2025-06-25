import React, { useEffect } from 'react';
import { View, Text, TouchableOpacity, ScrollView, StyleSheet, Platform } from 'react-native';
import { useRouter } from 'solito/router';
import { useAuth } from '@shared/provider';
import { useTheme, WebHeader, Footer } from '@shared/components';

function DashboardScreen() {
  const { user, signOut, loading: authLoading, isAuthenticated } = useAuth();
  const { theme } = useTheme();
  const { colors } = theme;
  const router = useRouter();

  const styles = StyleSheet.create({
    container: {
      flex: 1,
      backgroundColor: colors.background,
    },
    scrollView: {
      flexGrow: 1,
      padding: 16,
      ...(Platform.OS === 'web' && {
        minHeight: '100vh' as any,
      }),
    },
    contentContainer: {
      flex: 1,
      justifyContent: 'flex-start',
      alignItems: 'center',
      paddingTop: 40,
    },
    loadingContainer: {
      flex: 1,
      justifyContent: 'center',
      alignItems: 'center',
    },
    profileContainer: {
      padding: 24,
      maxWidth: 400,
      width: '100%',
      alignSelf: 'center',
    },
    welcomeText: {
      fontSize: 24,
      fontWeight: '600' as any,
      color: colors.text,
      marginBottom: 16,
    },
    sectionTitle: {
      fontSize: 18,
      fontWeight: '600' as any,
      color: colors.text,
      marginBottom: 16,
    },
    profileRow: {
      flexDirection: 'row',
      justifyContent: 'space-between',
      paddingVertical: 12,
      borderBottomWidth: 1,
      borderBottomColor: colors.border,
    },
    profileLabel: {
      fontSize: 16,
      color: colors.textSecondary,
      fontWeight: '500' as any,
    },
    profileAvatar: {
      width: 80,
      height: 80,
      borderRadius: 40,
      backgroundColor: colors.primary,
      alignItems: 'center',
      justifyContent: 'center',
      marginBottom: 16,
    },
    profileInitial: {
      fontSize: 32,
      fontWeight: '600' as any,
      color: '#FFFFFF',
    },
    profileInfo: {
      flexDirection: 'row',
      justifyContent: 'space-between',
      paddingVertical: 12,
      borderBottomWidth: 1,
      borderBottomColor: colors.border,
    },
    profileValue: {
      fontSize: 16,
      color: colors.text,
      fontWeight: '500' as any,
    },
    quickStatsContainer: {
      flexDirection: 'row',
      flexWrap: 'wrap',
      marginHorizontal: -8,
    },
    statCard: {
      flex: 1,
      minWidth: 150,
      backgroundColor: colors.cardBackground,
      borderRadius: 12,
      padding: 16,
      margin: 8,
      alignItems: 'center',
    },
    statTitle: {
      fontSize: 14,
      fontWeight: '500' as any,
      color: colors.textSecondary,
      marginBottom: 4,
    },
    button: {
      backgroundColor: colors.primary,
      borderRadius: 8,
      padding: 12,
      alignItems: 'center',
      marginTop: 16,
    },
    buttonText: {
      color: '#FFFFFF',
      fontSize: 16,
      fontWeight: '600' as any,
    },
    dangerButton: {
      backgroundColor: '#FF4444',
    },
  });

  useEffect(() => {
    // Redirect to login if not authenticated
    if (!authLoading && !isAuthenticated) {
      router.push('/login');
    }
  }, [isAuthenticated, authLoading, router]);

  const handleSignOut = async () => {
    await signOut();
  };

  if (authLoading) {
    return (
      <View style={[styles.container, styles.loadingContainer]}>
        <Text>Loading...</Text>
      </View>
    );
  }

  if (!isAuthenticated || !user) {
    return null;
  }

  // Extract user info from Supabase user object
  const userName = user.user_metadata.name || user.user_metadata.nickname || user.email || 'User';
  const userEmail = user.email || 'No email provided';
  const userPicture = user.user_metadata.avatar_url;
  const userId = user.id || 'Unknown';
  const userUpdatedAt = user.updated_at ? new Date(user.updated_at).toLocaleDateString() : 'Unknown';

  return (
    <View style={styles.container}>
      <WebHeader />
      
      <ScrollView style={styles.scrollView}>
        <View style={styles.contentContainer}>
          <View style={styles.profileContainer}>
            <Text style={styles.welcomeText}>Welcome, {userName}!</Text>
            
            <View style={styles.profileRow}>
              <Text style={styles.profileLabel}>Email</Text>
              <Text style={styles.profileValue}>{userEmail}</Text>
            </View>
            
            <View style={styles.profileRow}>
              <Text style={styles.profileLabel}>User ID</Text>
              <Text style={styles.profileValue}>{userId}</Text>
            </View>
            
            <View style={styles.profileRow}>
              <Text style={styles.profileLabel}>Last Updated</Text>
              <Text style={styles.profileValue}>{userUpdatedAt}</Text>
            </View>
          </View>

          <View style={styles.quickStatsContainer}>
            <View style={styles.statCard}>
              <Text style={styles.statTitle}>Projects</Text>
              <Text style={styles.statTitle}>12</Text>
            </View>
            
            <View style={styles.statCard}>
              <Text style={styles.statTitle}>Tasks</Text>
              <Text style={styles.statTitle}>48</Text>
            </View>
            
            <View style={styles.statCard}>
              <Text style={styles.statTitle}>Team Members</Text>
              <Text style={styles.statTitle}>6</Text>
            </View>
            
            <View style={styles.statCard}>
              <Text style={styles.statTitle}>Completion</Text>
              <Text style={styles.statTitle}>92%</Text>
            </View>
          </View>

          <TouchableOpacity style={styles.button} onPress={() => router.push('/profile')}>
            <Text style={styles.buttonText}>Edit Profile</Text>
          </TouchableOpacity>

          <TouchableOpacity
            style={[styles.button, styles.dangerButton]}
            onPress={handleSignOut}
          >
            <Text style={styles.buttonText}>Sign Out</Text>
          </TouchableOpacity>
        </View>
      </ScrollView>
      <Footer />
    </View>
  );
}

export { DashboardScreen };
export default DashboardScreen;
