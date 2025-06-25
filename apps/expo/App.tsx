import React from 'react';
import { StatusBar } from 'expo-status-bar';
import { View, LogBox } from 'react-native';
import { NativeNavigation } from './navigation';

// Ignore non-critical warnings that might cause issues
LogBox.ignoreLogs([
  'Possible Unhandled Promise Rejection',
  'Warning: componentWillReceiveProps has been renamed',
]);

export default function App() {
  return (
    <View style={{ flex: 1 }}>
      <StatusBar style="auto" />
      <NativeNavigation />
    </View>
  );
}
