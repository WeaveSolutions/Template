import React from 'react';
import { TouchableOpacity, Text, StyleSheet } from 'react-native';
import { useAuth0 } from '@shared/provider';

type Auth0LoginButtonProps = {
  title?: string;
};

export const Auth0LoginButton: React.FC<Auth0LoginButtonProps> = ({
  title = 'Log In with Auth0',
}) => {
  const { loginWithRedirect, isLoading } = useAuth0();

  return (
    <TouchableOpacity
      onPress={loginWithRedirect}
      disabled={isLoading}
      style={styles.button}
    >
      <Text style={styles.text}>{isLoading ? 'Loading...' : title}</Text>
    </TouchableOpacity>
  );
};

const styles = StyleSheet.create({
  button: {
    backgroundColor: '#007bff',
    padding: 12,
    borderRadius: 6,
    alignItems: 'center',
  },
  text: {
    color: 'white',
    fontWeight: 'bold',
  },
});
