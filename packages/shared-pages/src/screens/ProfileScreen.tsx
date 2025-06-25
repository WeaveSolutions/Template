import React, { useState, useEffect } from 'react';
import {
  View,
  Text,
  TextInput,
  TouchableOpacity,
  ScrollView,
  StyleSheet,
  Platform,
  ActivityIndicator,
  Image,
  KeyboardAvoidingView
} from 'react-native';
import { useRouter } from 'solito/router';
import { useAuth } from '@shared/provider';
import { useTheme, WebHeader, Footer } from '@shared/components';
import { supabase } from '@shared/utils';

interface Profile {
  username: string | null;
  full_name: string | null;
  avatar_url: string | null;
}

export function ProfileScreen() {
  const { user } = useAuth();
  const router = useRouter();
  const { theme } = useTheme();
  const { colors } = theme;
  
  const [loading, setLoading] = useState(false);
  const [saving, setSaving] = useState(false);
  const [profile, setProfile] = useState({
    username: null,
    full_name: null,
    avatar_url: null,
  } as Profile);

  useEffect(() => {
    if (user) {
      getProfile();
    } else {
      router.push('/login');
    }
  }, [user]);

  const getProfile = async () => {
    try {
      setLoading(true);
      const { data, error } = await supabase
        .from('profiles')
        .select('username, full_name, avatar_url')
        .eq('id', user!.id)
        .single();

      if (error && error.code !== 'PGRST116') {
        throw error;
      }

      if (data) {
        setProfile(data);
      }
    } catch (error) {
      console.error('Error loading profile:', error);
    } finally {
      setLoading(false);
    }
  };

  const updateProfile = async () => {
    try {
      setSaving(true);
      const updates = {
        id: user!.id,
        username: profile.username,
        full_name: profile.full_name,
        avatar_url: profile.avatar_url,
        updated_at: new Date().toISOString(),
      };

      const { error } = await supabase.from('profiles').upsert(updates);

      if (error) {
        throw error;
      }

      alert('Profile updated successfully!');
    } catch (error) {
      console.error('Error updating profile:', error);
      alert('Error updating profile');
    } finally {
      setSaving(false);
    }
  };

  if (!user) return null;

  const styles = StyleSheet.create({
    container: {
      flex: 1,
      backgroundColor: colors.background,
    },
    scrollView: {
      flexGrow: 1,
    },
    content: {
      padding: 20,
      maxWidth: 600,
      width: '100%',
      alignSelf: 'center',
    },
    header: {
      marginBottom: 32,
      paddingTop: 20,
    },
    title: {
      fontSize: 32,
      fontWeight: 'bold',
      color: colors.text,
      marginBottom: 8,
    },
    avatarContainer: {
      alignItems: 'center',
      marginBottom: 32,
    },
    avatar: {
      width: 120,
      height: 120,
      borderRadius: 60,
      backgroundColor: colors.surface,
      marginBottom: 16,
    },
    avatarPlaceholder: {
      width: 120,
      height: 120,
      borderRadius: 60,
      backgroundColor: colors.primary + '20',
      alignItems: 'center',
      justifyContent: 'center',
      marginBottom: 16,
    },
    avatarText: {
      fontSize: 48,
      color: colors.primary,
      fontWeight: 'bold',
    },
    changeAvatarButton: {
      paddingHorizontal: 16,
      paddingVertical: 8,
      borderRadius: 20,
      backgroundColor: colors.surface,
      borderWidth: 1,
      borderColor: colors.border,
    },
    changeAvatarText: {
      color: colors.text,
      fontSize: 14,
    },
    form: {
      marginBottom: 32,
    },
    inputContainer: {
      marginBottom: 20,
    },
    label: {
      fontSize: 14,
      color: colors.text,
      marginBottom: 8,
      fontWeight: '500',
    },
    input: {
      backgroundColor: colors.surface,
      borderRadius: 8,
      padding: 12,
      fontSize: 16,
      color: colors.text,
      borderWidth: 1,
      borderColor: colors.border,
    },
    readOnlyInput: {
      opacity: 0.6,
    },
    button: {
      backgroundColor: colors.primary,
      borderRadius: 8,
      padding: 16,
      alignItems: 'center',
      marginBottom: 12,
    },
    buttonText: {
      color: '#FFFFFF',
      fontSize: 16,
      fontWeight: '600',
    },
    secondaryButton: {
      backgroundColor: 'transparent',
      borderWidth: 1,
      borderColor: colors.border,
    },
    secondaryButtonText: {
      color: colors.text,
    },
    loadingContainer: {
      flex: 1,
      justifyContent: 'center',
      alignItems: 'center',
    },
    keyboardView: {
      flex: 1,
    },
    scrollContent: {
      padding: 20,
      maxWidth: 600,
      width: '100%',
      alignSelf: 'center',
    },
  });

  if (loading) {
    return (
      <View style={[styles.container, styles.loadingContainer]}>
        <ActivityIndicator />
      </View>
    );
  }

  return (
    <View style={styles.container}>
      <WebHeader />
      
      <KeyboardAvoidingView style={styles.keyboardView} behavior={Platform.OS === 'ios' ? 'padding' : undefined}>
        <ScrollView contentContainerStyle={styles.scrollContent}>
          <View style={styles.content}>
            <View style={styles.header}>
              <Text style={styles.title}>Profile</Text>
            </View>

            <View style={styles.avatarContainer}>
              {profile.avatar_url ? (
                <Image source={{ uri: profile.avatar_url }} style={styles.avatar} />
              ) : (
                <View style={styles.avatarPlaceholder}>
                  <Text style={styles.avatarText}>
                    {(profile.full_name || user.email || '?')[0].toUpperCase()}
                  </Text>
                </View>
              )}
              <TouchableOpacity style={styles.changeAvatarButton}>
                <Text style={styles.changeAvatarText}>Change Avatar</Text>
              </TouchableOpacity>
            </View>

            <View style={styles.form}>
              <View style={styles.inputContainer}>
                <Text style={styles.label}>Email</Text>
                <TextInput
                  style={[styles.input, styles.readOnlyInput]}
                  value={user.email || ''}
                  editable={false}
                />
              </View>

              <View style={styles.inputContainer}>
                <Text style={styles.label}>Username</Text>
                <TextInput
                  style={styles.input}
                  placeholder="Enter your username"
                  placeholderTextColor={colors.text + '50'}
                  value={profile.username || ''}
                  onChangeText={(text: string) => setProfile({ ...profile, username: text })}
                />
              </View>

              <View style={styles.inputContainer}>
                <Text style={styles.label}>Full Name</Text>
                <TextInput
                  style={styles.input}
                  placeholder="Enter your full name"
                  placeholderTextColor={colors.text + '50'}
                  value={profile.full_name || ''}
                  onChangeText={(text: string) => setProfile({ ...profile, full_name: text })}
                />
              </View>
            </View>

            <TouchableOpacity
              style={[styles.button, saving && { opacity: 0.7 }]}
              onPress={updateProfile}
              disabled={saving}
            >
              {saving ? (
                <ActivityIndicator />
              ) : (
                <Text style={styles.buttonText}>Save Changes</Text>
              )}
            </TouchableOpacity>

            <TouchableOpacity
              style={[styles.button, styles.secondaryButton]}
              onPress={() => router.push('/dashboard')}
            >
              <Text style={[styles.buttonText, styles.secondaryButtonText]}>
                Back to Dashboard
              </Text>
            </TouchableOpacity>
          </View>
        </ScrollView>
      </KeyboardAvoidingView>
      
      <Footer />
    </View>
  );
}
