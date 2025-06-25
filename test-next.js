// Test script to verify @expo/next-adapter installation
try {
  const { withExpo } = require('@expo/next-adapter');
  console.log('✅ @expo/next-adapter found and working');
  console.log('withExpo function:', typeof withExpo);
} catch (error) {
  console.log('❌ @expo/next-adapter not found:', error.message);
}
