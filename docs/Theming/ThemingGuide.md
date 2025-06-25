# Theming Guide for Nexpo

This guide provides instructions for implementing and using light and dark themes in the Nexpo template with Tailwind CSS.

## Overview

Theming allows your app to support light and dark modes, enhancing user experience by adapting to different lighting conditions or user preferences. This template uses Tailwind CSS with a class-based dark mode strategy and a custom `ThemeContext` for theme management.

## Prerequisites

- Node.js and pnpm installed
- Tailwind CSS already configured in the project

## Setting Up Theming

### 1. Tailwind CSS Dark Mode Configuration

Tailwind CSS is configured to support dark mode using the `class` strategy in `tailwind.config.js`:

```js
module.exports = {
  content: ['./apps/**/*.{js,ts,jsx,tsx}', './packages/**/*.{js,ts,jsx,tsx}'],
  darkMode: 'class', // Enable dark mode with class strategy
  theme: {
    extend: {
      colors: {
        primary: {
          DEFAULT: '#007BFF',
          foreground: '#FFFFFF',
          dark: '#0056b3',
        },
        secondary: {
          DEFAULT: '#6C757D',
          foreground: '#FFFFFF',
          dark: '#494c50',
        },
        background: {
          DEFAULT: '#FFFFFF',
          dark: '#121212',
        },
        text: {
          DEFAULT: '#212529',
          dark: '#E9ECEF',
        },
      },
    },
  },
  plugins: [],
};
```

- `darkMode: 'class'` allows toggling dark mode by adding a `dark` class to the HTML element or a parent container.
- Custom colors are defined for light and dark variants to ensure consistent theming.

### 2. Theme Context Setup

A `ThemeContext` is provided in `packages/shared-ui/src/context/ThemeContext.tsx` to manage theme state and provide theme toggling functionality:

```tsx
import React, { createContext, useContext, useEffect, useState } from 'react';
import { useColorScheme } from 'react-native';

interface ThemeContextType {
  theme: 'light' | 'dark';
  toggleTheme: () => void;
  setTheme: (theme: 'light' | 'dark') => void;
  isDark: boolean;
}

const ThemeContext = createContext<ThemeContextType | undefined>(undefined);

export const ThemeProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  const systemColorScheme = useColorScheme();
  const [theme, setTheme] = useState<'light' | 'dark'>(
    systemColorScheme === 'dark' ? 'dark' : 'light'
  );

  useEffect(() => {
    setTheme(systemColorScheme === 'dark' ? 'dark' : 'light');
  }, [systemColorScheme]);

  const toggleTheme = () => {
    setTheme((prevTheme) => (prevTheme === 'light' ? 'dark' : 'light'));
  };

  return (
    <ThemeContext.Provider
      value={{
        theme,
        toggleTheme,
        setTheme,
        isDark: theme === 'dark',
      }}
    >
      {children}
    </ThemeContext.Provider>
  );
};

export const useTheme = () => {
  const context = useContext(ThemeContext);
  if (context === undefined) {
    throw new Error('useTheme must be used within a ThemeProvider');
  }
  return context;
};

export const getThemeClassName = (theme: 'light' | 'dark', lightClass: string, darkClass: string) => {
  return theme === 'light' ? lightClass : darkClass;
};
```

- `ThemeProvider` initializes the theme based on the system's color scheme (using `useColorScheme` from React Native).
- It provides methods to toggle or set the theme explicitly.
- `useTheme` hook allows components to access the current theme state.

### 3. Wrapping Your App with ThemeProvider

Ensure your app is wrapped with the `ThemeProvider` to make theme information available throughout your application. Update both `apps/next/pages/_app.tsx` and `apps/expo/App.tsx`:

#### For Next.js (`_app.tsx`):

```tsx
import { ThemeProvider } from 'shared-ui';

function MyApp({ Component, pageProps }) {
  return (
    <ThemeProvider>
      <Component {...pageProps} />
    </ThemeProvider>
  );
}

export default MyApp;
```

#### For Expo (`App.tsx`):

```tsx
import { ThemeProvider } from 'shared-ui';

export default function App() {
  return (
    <ThemeProvider>
      {/* Your app content */}
    </ThemeProvider>
  );
}
```

### 4. Applying Themes in Components

#### Using Tailwind CSS Dark Mode

With `darkMode: 'class'` in Tailwind, you can apply dark mode styles by adding the `dark` class to a parent element (typically `<html>` or `<body>`). Update your root component to apply this class based on the theme:

##### For Next.js:

In `apps/next/pages/_document.tsx` or a layout component:

```tsx
import { useTheme } from 'shared-ui';
import { Html, Head, Main, NextScript } from 'next/document';

export default function Document() {
  const { theme } = useTheme();

  return (
    <Html lang="en" className={theme === 'dark' ? 'dark' : ''}>
      <Head />
      <body>
        <Main />
        <NextScript />
      </body>
    </Html>
  );
}
```

##### For Expo:

In a top-level component like `App.tsx` or a layout wrapper:

```tsx
import { useTheme } from 'shared-ui';
import { View } from 'react-native';

const AppWrapper = ({ children }) => {
  const { theme } = useTheme();

  return (
    <View className={theme === 'dark' ? 'dark' : ''} style={{ flex: 1 }}>
      {children}
    </View>
  );
};
```

Use Tailwind's `dark:` prefix for dark mode styles:

```tsx
<View className="bg-background dark:bg-background-dark p-4">
  <Text className="text-text dark:text-text-dark">Hello, World!</Text>
</View>
```

#### Using ThemeContext for Custom Logic

Access the theme in any component to apply conditional styling or logic:

```tsx
import { useTheme } from 'shared-ui';
import { View, Text, Button } from 'react-native';

const SettingsScreen = () => {
  const { theme, toggleTheme } = useTheme();

  return (
    <View className={`p-4 ${theme === 'dark' ? 'bg-background-dark' : 'bg-background'}`}>
      <Text className={theme === 'dark' ? 'text-text-dark' : 'text-text'}>
        Current Theme: {theme}
      </Text>
      <Button title="Toggle Theme" onPress={toggleTheme} />
    </View>
  );
};
```

### 5. Theming Shadcn UI Components (Web)

Shadcn UI components automatically adapt to dark mode when the `dark` class is applied to a parent. Ensure your root component or HTML element has the `dark` class based on the theme, as shown above.

## Best Practices

- **System Preference**: Initialize theme based on system preference (`useColorScheme`) to respect user settings.
- **Consistency**: Use defined theme colors from Tailwind config for consistent branding across light and dark modes.
- **Accessibility**: Ensure sufficient contrast between text and background in both themes. Tailwind's default palette is designed with accessibility in mind.
- **Performance**: Avoid excessive re-renders when toggling themes by memoizing components if necessary.
- **Testing**: Test UI in both light and dark modes to catch visibility issues with icons, images, or custom styles.

## Troubleshooting

- **Dark Mode Not Applying**: Ensure the `dark` class is added to the root element when the theme is set to dark. Check if `darkMode: 'class'` is correctly set in `tailwind.config.js`.
- **Theme Not Toggling**: Verify that `ThemeProvider` wraps your app and that `toggleTheme` is called correctly.
- **Platform Differences**: Some React Native styles may not map directly to Tailwind's `dark:` classes. Use conditional styling with `useTheme` if needed.

For more detailed information, refer to:
- [Tailwind CSS Dark Mode](https://tailwindcss.com/docs/dark-mode)
- [React Native useColorScheme](https://reactnative.dev/docs/usecolorscheme)
