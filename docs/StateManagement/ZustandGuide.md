# Zustand State Management Guide for Nexpo

This guide provides instructions for using Zustand as the state management solution in the Nexpo template.

## Overview

Zustand is a small, fast, and scalable state-management library that provides a simple API for managing global state in React and React Native applications. It's lightweight compared to alternatives like Redux and integrates seamlessly with both web and mobile platforms in this monorepo.

## Prerequisites

- Node.js and pnpm installed
- Zustand library installed in the project

## Setting Up Zustand

### 1. Install Zustand

If not already installed, add Zustand to your project:

```bash
pnpm install zustand
```

Zustand is already included in the `shared-ui` package for this template.

### 2. Define Stores

Stores in Zustand are where you define your state and actions. In this template, stores are created in `packages/shared-ui/src/state/zustandStore.ts` to be reused across Next.js, Expo, and Tauri apps.

```typescript
import { create } from 'zustand';
import { persist } from 'zustand/middleware';

// User authentication state with persistence
interface UserState {
  user: {
    id: string;
    email: string;
    fullName?: string;
  } | null;
  isAuthenticated: boolean;
  setUser: (user: { id: string; email: string; fullName?: string } | null) => void;
  setAuthenticated: (isAuthenticated: boolean) => void;
  logout: () => void;
}

export const useUserStore = create<UserState>()(
  persist(
    (set) => ({
      user: null,
      isAuthenticated: false,
      setUser: (user) => set({ user }),
      setAuthenticated: (isAuthenticated) => set({ isAuthenticated }),
      logout: () => set({ user: null, isAuthenticated: false }),
    }),
    {
      name: 'user-storage', // key for localStorage
    }
  )
);

// Theme state with persistence
interface ThemeState {
  theme: 'light' | 'dark';
  setTheme: (theme: 'light' | 'dark') => void;
  toggleTheme: () => void;
}

export const useThemeStore = create<ThemeState>()(
  persist(
    (set, get) => ({
      theme: 'light',
      setTheme: (theme) => set({ theme }),
      toggleTheme: () => set({ theme: get().theme === 'light' ? 'dark' : 'light' }),
    }),
    {
      name: 'theme-storage',
    }
  )
);

// General app state
interface AppState {
  isLoading: boolean;
  error: string | null;
  setLoading: (isLoading: boolean) => void;
  setError: (error: string | null) => void;
  clearError: () => void;
}

export const useAppStore = create<AppState>()((set) => ({
  isLoading: false,
  error: null,
  setLoading: (isLoading) => set({ isLoading }),
  setError: (error) => set({ error }),
  clearError: () => set({ error: null }),
}));

// **User Store**: Manages authentication state with persistence using localStorage (via `persist` middleware) to retain login state across sessions.
- **Theme Store**: Handles theme preference (light/dark mode) with persistence.
- **App Store**: Manages transient app-wide state like loading indicators or error messages.

### 3. Using Stores in Components

Access state and actions from any component using the store hooks provided by Zustand.

#### Accessing User State

```typescript
import { useUserStore } from 'shared-ui';

const LoginComponent = () => {
  const { user, isAuthenticated, setUser, setAuthenticated, logout } = useUserStore();

  const handleLogin = (userData: { id: string; email: string; fullName?: string }) => {
    setUser(userData);
    setAuthenticated(true);
  };

  const handleLogout = () => {
    logout();
  };

  if (isAuthenticated) {
    return (
      <div>
        <p>Welcome, {user?.fullName || user?.email}</p>
        <button onClick={handleLogout}>Logout</button>
      </div>
    );
  }

  return (
    <button onClick={() => handleLogin({ id: '123', email: 'user@example.com', fullName: 'John Doe' })}>
      Login
    </button>
  );
};
```

#### Managing Theme

```typescript
import { useThemeStore } from 'shared-ui';

const ThemeSwitcher = () => {
  const { theme, toggleTheme, setTheme } = useThemeStore();

  return (
    <div>
      <p>Current Theme: {theme}</p>
      <button onClick={toggleTheme}>Toggle Theme</button>
      <button onClick={() => setTheme('light')}>Set Light Theme</button>
      <button onClick={() => setTheme('dark')}>Set Dark Theme</button>
    </div>
  );
};
```

#### Handling App State

```typescript
import { useAppStore } from 'shared-ui';

const LoadingIndicator = () => {
  const { isLoading, error, clearError } = useAppStore();

  if (!isLoading && !error) return null;

  return (
    <div>
      {isLoading && <p>Loading...</p>}
      {error && (
        <div>
          <p>Error: {error}</p>
          <button onClick={clearError}>Dismiss</button>
        </div>
      )}
    </div>
  );
};
```

### 4. Integrating with Supabase Authentication

Sync Zustand's user store with Supabase authentication state for seamless login/logout:

```typescript
import { useEffect } from 'react';
import { supabase } from 'shared-ui';
import { useUserStore } from 'shared-ui';

const AuthListener = () => {
  const { setUser, setAuthenticated, logout } = useUserStore();

  useEffect(() => {
    // Set initial session
    supabase.auth.getSession().then(({ data: { session } }) => {
      if (session?.user) {
        setUser({
          id: session.user.id,
          email: session.user.email || '',
          fullName: session.user.user_metadata?.full_name,
        });
        setAuthenticated(true);
      }
    });

    // Listen for auth changes
    const {
      data: { subscription },
    } = supabase.auth.onAuthStateChange((_event, session) => {
      if (session?.user) {
        setUser({
          id: session.user.id,
          email: session.user.email || '',
          fullName: session.user.user_metadata?.full_name,
        });
        setAuthenticated(true);
      } else {
        logout();
      }
    });

    return () => subscription.unsubscribe();
  }, [setUser, setAuthenticated, logout]);

  return null; // This component doesn't render anything
};

export default AuthListener;
```

Add `<AuthListener />` to your root component (e.g., `App.tsx` or `_app.tsx`) to keep the Zustand store in sync with Supabase auth state.

### 5. Persistence and Storage Options

- Zustand's `persist` middleware saves state to `localStorage` by default, which works across web and React Native (via AsyncStorage internally).
- For sensitive data, avoid persistence or use secure storage solutions like `react-native-keychain` with a custom storage adapter for Zustand.

Example of custom storage for Zustand in React Native:

```typescript
import { create } from 'zustand';
import { persist } from 'zustand/middleware';
import AsyncStorage from '@react-native-async-storage/async-storage';

const customStorage = {
  getItem: async (name: string) => {
    const value = await AsyncStorage.getItem(name);
    return value ? JSON.parse(value) : null;
  },
  setItem: async (name: string, value: any) => {
    await AsyncStorage.setItem(name, JSON.stringify(value));
  },
  removeItem: async (name: string) => {
    await AsyncStorage.removeItem(name);
  },
};

export const useSecureStore = create<any>()(
  persist(
    (set) => ({
      // Your state here
    }),
    {
      name: 'secure-storage',
      storage: customStorage,
    }
  )
);
```

## Best Practices

- **Modular Stores**: Split state into multiple stores based on domain (e.g., user, theme, app) for better organization and performance.
- **Minimal State**: Only store necessary data in Zustand to avoid bloating global state. Use local component state for transient UI state.
- **Type Safety**: Always define TypeScript interfaces for your stores to catch errors early.
- **Memoization**: Use Zustand's selector functions for derived state to prevent unnecessary re-renders:
  ```typescript
  const email = useUserStore((state) => state.user?.email);
  ```
- **Middleware**: Leverage Zustand middleware like `persist` for storage or `immer` for immutable updates if dealing with complex nested state.
- **Testing**: Test store logic independently since stores are plain JavaScript functions:
  ```typescript
  import { useUserStore } from 'shared-ui';
  test('user store logout', () => {
    const store = useUserStore.getState();
    store.setUser({ id: '123', email: 'test@example.com' });
    store.setAuthenticated(true);
    store.logout();
    expect(store.user).toBeNull();
    expect(store.isAuthenticated).toBe(false);
  });
  ```

## Troubleshooting

- **State Not Updating**: Ensure you're using `set` correctly within actions. Check for stale closures in `useEffect` or callbacks.
- **Persistence Issues**: Verify storage keys (`name` in `persist`) are unique across stores to avoid conflicts. Clear app data or localStorage if old persisted data causes issues.
- **React Native AsyncStorage Errors**: Ensure `@react-native-async-storage/async-storage` is installed and linked if using persistence in React Native.
- **Performance**: If re-renders are excessive, use selectors to subscribe only to specific parts of the state.

For more detailed information, refer to the official Zustand documentation at [https://zustand-demo.pmnd.rs/](https://zustand-demo.pmnd.rs/).
