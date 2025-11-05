# API Client

This module provides a unified API client for making authenticated requests across all platforms (Web, Mobile, Desktop).

## Usage

### Basic Usage

```typescript
import { apiClient } from '@shared/utils';

// Make a GET request
const data = await apiClient.get('/api/users');

// Make a POST request
const newUser = await apiClient.post('/api/users', {
  name: 'John Doe',
  email: 'john@example.com'
});
```

### Authentication Setup

For the API client to include authentication tokens in requests, you need to set up the auth token provider at the root of your application:

#### In React/React Native Apps

```typescript
import { useAuthTokenProvider } from '@shared/provider';

function App() {
  // This hook sets up the token provider for the API client
  useAuthTokenProvider();
  
  return (
    // Your app content
  );
}
```

#### In Non-React Contexts

If you need to use the API client outside of React components, you can manually set the token provider:

```typescript
import { setAuthTokenProvider } from '@shared/utils';

// Set a custom token provider
setAuthTokenProvider(async () => {
  // Your custom logic to get the auth token
  const token = await getTokenFromSomeSource();
  return token;
});
```

## Platform-Specific Behavior

### Web Platform
- Authentication is handled via HTTP-only cookies
- No need to manually include tokens in requests
- CORS credentials are automatically included

### Mobile Platform (iOS/Android)
- Tokens are stored in secure storage (Keychain/Keystore)
- Tokens are automatically included in the Authorization header
- Falls back to direct secure storage access if no token provider is set

### Desktop Platform (Tauri)
- Similar to mobile, tokens are stored securely
- Tokens are included in the Authorization header

## API Methods

- `apiClient.get(endpoint, options?)` - GET request
- `apiClient.post(endpoint, data?, options?)` - POST request
- `apiClient.put(endpoint, data?, options?)` - PUT request
- `apiClient.patch(endpoint, data?, options?)` - PATCH request
- `apiClient.delete(endpoint, options?)` - DELETE request

## Error Handling

The API client automatically handles common errors:
- Network errors
- Authentication errors (401)
- Server errors (5xx)

Errors are thrown with meaningful messages that can be caught and handled in your application.
