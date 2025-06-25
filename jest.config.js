const nextJest = require('next/jest');

const createJestConfig = nextJest({
  // Provide the path to your Next.js app to load next.config.js and .env files in your test environment
  dir: './apps/next',
});

// Add any custom config to be passed to Jest
const customJestConfig = {
  setupFilesAfterEnv: ['<rootDir>/jest.setup.js'],
  testEnvironment: 'jest-environment-jsdom',
  moduleFileExtensions: ['ts', 'tsx', 'js', 'jsx', 'json', 'node'],
  moduleNameMapper: {
    '^shared-ui(.*)$': '<rootDir>/packages/shared-ui/src$1',
  },
  testPathIgnorePatterns: ['<rootDir>/node_modules/', '<rootDir>/.next/'],
  collectCoverage: true,
  collectCoverageFrom: ['**/*.{ts,tsx}', '!**/node_modules/**', '!**/vendor/**'],
  coverageDirectory: 'coverage',
};

// createJestConfig is exported this way to ensure that next/jest can load the Next.js config which is async
module.exports = createJestConfig(customJestConfig);
