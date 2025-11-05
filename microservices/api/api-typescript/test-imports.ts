// Test imports to identify the issue
console.log('Starting import test...');

try {
  console.log('Testing Express...');
  const express = require('express');
  console.log('✓ Express imported successfully');
  
  console.log('Testing middleware imports...');
  const { kongLogger } = require('./middleware/logger');
  console.log('✓ Logger middleware imported');
  
  console.log('Testing route imports...');
  const authRoutes = require('./routes/auth');
  console.log('✓ Auth routes imported');
  
  const userRoutes = require('./routes/user');
  console.log('✓ User routes imported');
  
  const linkingRoutes = require('./routes/linking');
  console.log('✓ Linking routes imported');
  
  const metricsRoutes = require('./routes/metrics');
  console.log('✓ Metrics routes imported');
  
  console.log('All imports successful!');
} catch (error) {
  console.error('Import error:', error);
  process.exit(1);
}
