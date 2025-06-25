# Jest Testing Setup Guide for Nexpo

This guide provides instructions for setting up and using Jest for unit testing in the Nexpo template.

## Overview

Jest is a delightful JavaScript testing framework with a focus on simplicity. It's used in this template for unit testing React components and logic across both web (Next.js) and mobile (Expo) platforms.

## Prerequisites

- Node.js and pnpm installed

## Setting Up Jest

### 1. Install Jest Dependencies

Jest and related testing libraries are already added to the `package.json` file. To install them, run:

```bash
pnpm install --save-dev jest jest-environment-jsdom @testing-library/react @testing-library/jest-dom @testing-library/react-native
```

### 2. Jest Configuration

The Jest configuration is set up in `jest.config.js` at the root of the project:

```js
const nextJest = require('next/jest');

const createJestConfig = nextJest({
  dir: './apps/next',
});

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

module.exports = createJestConfig(customJestConfig);
```

### 3. Setup File

A setup file `jest.setup.js` is included to extend Jest with additional matchers from `@testing-library/jest-dom`:

```js
import '@testing-library/jest-dom';
```

### 4. Writing Tests

Create test files with the `.test.ts` or `.test.tsx` extension. Place them next to the files they test or in a `__tests__` folder. Example structure:

```
packages/shared-ui/src/components/
  MyComponent.tsx
  MyComponent.test.tsx
```

Example test for a component:

```tsx
import { render, screen } from '@testing-library/react';
import MyComponent from './MyComponent';

test('renders component with text', () => {
  render(<MyComponent />);
  const linkElement = screen.getByText(/Hello, World!/i);
  expect(linkElement).toBeInTheDocument();
});
```

### 5. Running Tests

Add test scripts to your `package.json`:

```json
{
  "scripts": {
    "test": "jest",
    "test:watch": "jest --watch",
    "test:coverage": "jest --coverage"
  }
}
```

Run tests with:

- `pnpm test` - Run all tests once
- `pnpm run test:watch` - Run tests in watch mode (reruns on file changes)
- `pnpm run test:coverage` - Run tests and generate a coverage report

## Best Practices

- **Colocation**: Keep test files close to the source files they test for easier maintenance.
- **Mocking**: Use Jest's mocking capabilities for external dependencies or API calls.
- **Snapshot Testing**: Use snapshots for UI components to detect unexpected changes.
- **Coverage**: Aim for high test coverage, especially for critical logic and components.

## Troubleshooting

- **Jest Not Finding Tests**: Ensure test files follow the naming convention (`*.test.ts` or `*.spec.ts`) and are not ignored in `testPathIgnorePatterns`.
- **Environment Issues**: For React Native tests, you might need to adjust the `testEnvironment` or use `@testing-library/react-native` specific methods.
- **Mocking Errors**: Double-check mock implementations if tests fail unexpectedly due to external dependencies.

For more detailed information, refer to the official Jest documentation at [https://jestjs.io/docs/getting-started](https://jestjs.io/docs/getting-started).
