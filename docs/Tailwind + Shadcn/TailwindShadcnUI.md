# Tailwind CSS and Shadcn UI Guide for Nexpo

This guide provides instructions for setting up and using Tailwind CSS and Shadcn UI in the Nexpo template for consistent styling across web and mobile platforms.

## Overview

- **Tailwind CSS**: A utility-first CSS framework for rapid UI development.
- **Shadcn UI**: A collection of reusable components built on Tailwind CSS and Radix UI, designed for web applications.

This template already has Tailwind CSS configured for Next.js, Expo, and Tauri apps. Shadcn UI components can be added for web-focused UI elements.

## Prerequisites

- Node.js and pnpm installed
- Familiarity with React and React Native development

## Tailwind CSS Setup

Tailwind CSS is pre-configured in this template for web (Next.js), mobile (Expo), and desktop (Tauri) platforms.

### Configuration Files

- **Root `tailwind.config.js`**: Central configuration for Tailwind CSS used across the monorepo.
- **Next.js**: Tailwind is set up in `apps/next/postcss.config.js`.
- **Expo**: Tailwind is integrated via `babel.config.js` using `nativewind` for React Native compatibility.
- **Tauri**: Tailwind is configured via the shared Next.js configuration for the desktop frontend.

### Using Tailwind CSS

1. **In Next.js Components (Web)**:
   - Use Tailwind classes directly in your React components:
     ```jsx
     <div className="bg-blue-500 text-white p-4 rounded-lg">
       Web Content
     </div>
     ```

2. **In Expo Components (Mobile)**:
   - Use Tailwind with NativeWind, which maps Tailwind classes to React Native styles:
     ```jsx
     import { View, Text } from 'react-native';

     <View className="bg-blue-500 p-4 rounded-lg">
       <Text className="text-white">Mobile Content</Text>
     </View>
     ```

3. **Platform-Specific Styling**:
   - Use `Platform.OS` conditionals for styles that differ between platforms:
     ```jsx
     import { Platform } from 'react-native';

     <View className={Platform.OS === 'web' ? 'p-6' : 'p-4'}>
       {/* Content */}
     </View>
     ```

## Adding Shadcn UI Components (Web-Focused)

Shadcn UI provides accessible, customizable components for web applications. Since Shadcn UI relies on web-specific libraries like Radix UI, it's primarily used in the Next.js app.

### Installation

1. **Install Dependencies**:
   Run the following command in the root of your project to install necessary packages for Shadcn UI:
   ```bash
   pnpm install -D @radix-ui/* clsx tailwind-merge
   ```

2. **CSS Variables**:
   Ensure your `tailwind.config.js` includes CSS variables for theming (already configured in this template).

3. **Using Shadcn CLI (Optional)**:
   For an automated setup, install the Shadcn CLI:
   ```bash
   pnpm install -D @shadcn/cli
   npx shadcn init
   ```
   Follow the prompts to configure your project and add components as needed.

### Manually Adding Components

Since Shadcn UI components are copied into your project for customization, here's how to add a component like a Button:

1. **Create Button Component**:
   Create a file at `packages/shared-ui/src/components/ui/Button.tsx`:
   ```tsx
   import React from 'react';
   import { Slot } from '@radix-ui/react-slot';
   import { cva, type VariantProps } from 'class-variance-authority';
   import { cn } from '../../utils/cn';

   const buttonVariants = cva(
     'inline-flex items-center justify-center whitespace-nowrap rounded-md text-sm font-medium ring-offset-background transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:pointer-events-none disabled:opacity-50',
     {
       variants: {
         variant: {
           default: 'bg-primary text-primary-foreground hover:bg-primary/90',
           destructive:
             'bg-destructive text-destructive-foreground hover:bg-destructive/90',
           outline:
             'border border-input bg-background hover:bg-input/10',
           secondary: 'bg-secondary text-secondary-foreground hover:bg-secondary/80',
           ghost: 'hover:bg-accent hover:text-accent-foreground',
           link: 'text-primary underline-offset-4 hover:underline',
         },
         size: {
           default: 'h-10 px-4 py-2',
           sm: 'h-9 rounded-md px-3',
           lg: 'h-11 rounded-md px-8',
           icon: 'h-10 w-10',
         },
       },
       defaultVariants: {
         variant: 'default',
         size: 'default',
       },
     }
   );

   export interface ButtonProps
     extends React.ButtonHTMLAttributes<HTMLButtonElement>,
       VariantProps<typeof buttonVariants> {
     asChild?: boolean;
   }

   const Button = React.forwardRef<HTMLButtonElement, ButtonProps>(
     ({ className, variant, size, asChild = false, ...props }, ref) => {
       const Comp = asChild ? Slot : 'button';
       return <Comp className={cn(buttonVariants({ variant, size, className }))} ref={ref} {...props} />;
     }
   );
   Button.displayName = 'Button';

   export { Button, buttonVariants };
   ```

2. **Create Utility for Class Names**:
   Create a utility file at `packages/shared-ui/src/utils/cn.ts`:
   ```ts
   import { ClassValue, clsx } from 'clsx';
   import { twMerge } from 'tailwind-merge';

   export function cn(...inputs: ClassValue[]) {
     return twMerge(clsx(inputs));
   }
   ```

3. **Use the Button in Your Web App**:
   In your Next.js components, import and use the Button:
   ```tsx
   import { Button } from 'shared-ui';

   <Button variant="default">Click Me</Button>
   ```

### Platform Considerations

Since Shadcn UI components are web-focused:
- Use them primarily in Next.js components.
- For mobile (Expo), rely on React Native components styled with Tailwind via NativeWind.
- For desktop (Tauri), use standard Tailwind classes similar to Next.js.
- Create platform-specific rendering logic if a component needs to appear on both web and mobile:
  ```tsx
  import { Platform } from 'react-native';
  import { Button } from 'shared-ui'; // Web component
  import MobileButton from './MobileButton'; // Custom mobile component

  const PlatformButton = Platform.OS === 'web' ? Button : MobileButton;
  ```

## Customizing Tailwind CSS

1. **Modify Colors and Themes**:
   Update `tailwind.config.js` to extend the default theme:
   ```js
   module.exports = {
     content: ['./apps/**/*.{js,ts,jsx,tsx}', './packages/**/*.{js,ts,jsx,tsx}'],
     theme: {
       extend: {
         colors: {
           primary: {
             DEFAULT: '#007BFF',
             foreground: '#FFFFFF',
           },
         },
       },
     },
     plugins: [],
   };
   ```

2. **Responsive Design**:
   Use Tailwind's responsive prefixes for different screen sizes:
   ```jsx
   <div className="text-base md:text-lg lg:text-xl">Responsive Text</div>
   ```

## Best Practices

- **Consistent Styling**: Maintain a design system by defining reusable styles or components in the `shared-ui` package.
- **Platform Awareness**: Be mindful of CSS properties not supported in React Native (like `display: flex` with certain values or `gap`). Use `Platform.OS` conditionals to handle these differences.
- **Component Reusability**: Create shared components in `packages/shared-ui/src/components` for elements used across your app.
- **Performance**: Avoid excessive inline styles in favor of Tailwind classes for better caching and performance.

## Troubleshooting

- **Tailwind Not Applying**: Ensure Tailwind is correctly imported in your entry files (`apps/next/pages/_app.tsx`, `apps/expo/App.tsx`, and `apps/tauri/src/App.tsx`). Check if `nativewind` is configured in `babel.config.js` for Expo.
- **Shadcn Components Not Rendering**: Verify dependencies (`@radix-ui/*`, `clsx`, `tailwind-merge`) are installed. Check browser console for errors related to missing CSS variables.
- **Styling Differences Between Platforms**: Use tools like `react-native-debugger` for mobile or browser dev tools for web to inspect rendered styles and adjust accordingly.

For more detailed information, refer to:
- [Tailwind CSS Documentation](https://tailwindcss.com/docs)
- [NativeWind Documentation](https://www.nativewind.dev/) (for React Native)
- [Shadcn UI Documentation](https://ui.shadcn.com/)
