import React from 'react';
import { TouchableOpacity, Text, StyleSheet } from 'react-native';
import { useAuth0 } from '@shared/provider';

type Auth0LogoutButtonProps = {
  title?: string;
};

export const Auth0LogoutButton: React.FC<Auth0LogoutButtonProps> = ({
  title = 'Log Out',
}) => {
  const { logout, isLoading } = useAuth0();

  return (
    <TouchableOpacity
      onPress={logout}
      disabled={isLoading}
      style={styles.button}
    >
      <Text style={styles.text}>{isLoading ? 'Loading...' : title}</Text>
    </TouchableOpacity>
  );
};

const styles = StyleSheet.create({
  button: {
    backgroundColor: '#dc3545',
    padding: 12,
    borderRadius: 6,
    alignItems: 'center',
  },
  text: {
    color: 'white',
    fontWeight: 'bold',
  },
});
