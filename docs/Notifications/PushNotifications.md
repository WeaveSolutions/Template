# Push Notifications Setup Guide for Nexpo

This guide provides instructions for setting up push notifications for the mobile app using Expo's push notification system and Supabase Edge Functions for sending notifications.

## Overview

Push notifications allow you to engage users by sending messages directly to their devices. Expo provides a robust push notification service that works for both iOS and Android. We'll integrate this with Supabase Edge Functions to send notifications from your backend.

## Prerequisites

- An Expo account ([https://expo.dev/](https://expo.dev/))
- Node.js and pnpm installed
- Supabase project set up with the Supabase CLI installed
- Apple Developer Account for iOS notifications
- Google Firebase account for Android notifications (optional, for FCM integration)

## Setting Up Push Notifications with Expo

### 1. Install Expo Notifications Module

Install the necessary Expo module for push notifications:

```bash
npx expo install expo-notifications
```

### 2. Configure Notifications in app.json

Update your `app.json` in `apps/expo` to enable notifications:

```json
{
  "expo": {
    "name": "My App",
    "slug": "my-app",
    "version": "1.0.0",
    "orientation": "portrait",
    "icon": "./assets/icon.png",
    "userInterfaceStyle": "light",
    "splash": {
      "image": "./assets/splash.png",
      "resizeMode": "contain",
      "backgroundColor": "#ffffff"
    },
    "updates": {
      "fallbackToCacheTimeout": 0
    },
    "assetBundlePatterns": [
      "**/*"
    ],
    "ios": {
      "supportsTablet": true,
      "infoPlist": {
        "UILaunchStoryboardName": "SplashScreen",
        "UIBackgroundModes": ["remote-notification"]
      }
    },
    "android": {
      "adaptiveIcon": {
        "foregroundImage": "./assets/adaptive-icon.png",
        "backgroundColor": "#FFFFFF"
      },
      "permissions": ["RECEIVE_BOOT_COMPLETED", "VIBRATE", "WAKE_LOCK", "com.google.android.c2dm.permission.RECEIVE"]
    },
    "web": {
      "favicon": "./assets/favicon.png"
    },
    "plugins": [
      [
        "expo-notifications",
        {
          "icon": "./assets/notification-icon.png",
          "color": "#ffffff",
          "sounds": []
        }
      ]
    ]
  }
}
```

- `UIBackgroundModes` for iOS enables background notifications.
- Android permissions allow the app to receive notifications.
- The `expo-notifications` plugin configures notification icons and colors.

### 3. Set Up Notification Handling in App

Create a notification handler in your Expo app to manage incoming notifications. Add the following to `apps/expo/src/utils/notifications.ts`:

```tsx
import * as Notifications from 'expo-notifications';
import { Platform } from 'react-native';

// Set up notification handler
Notifications.setNotificationHandler({
  handleNotification: async () => ({
    shouldShowAlert: true,
    shouldPlaySound: false,
    shouldSetBadge: false,
  }),
});

// Request notification permissions
export const requestNotificationPermissions = async () => {
  const { status: existingStatus } = await Notifications.getPermissionsAsync();
  let finalStatus = existingStatus;
  if (existingStatus !== 'granted') {
    const { status } = await Notifications.requestPermissionsAsync();
    finalStatus = status;
  }
  if (finalStatus !== 'granted') {
    console.log('Notification permissions not granted');
    return false;
  }

  if (Platform.OS === 'android') {
    Notifications.setNotificationChannelAsync('default', {
      name: 'default',
      importance: Notifications.AndroidImportance.MAX,
      vibrationPattern: [0, 250, 250, 250],
      lightColor: '#FF231F7C',
    });
  }

  return true;
};

// Get push token
export const getPushToken = async () => {
  const hasPermission = await requestNotificationPermissions();
  if (!hasPermission) {
    return null;
  }

  const pushToken = await Notifications.getExpoPushTokenAsync({
    projectId: 'your-expo-project-id', // Replace with your Expo project ID from app.json
  });
  return pushToken.data;
};

// Listen for incoming notifications
export const listenForNotifications = (callback: (notification: Notifications.Notification) => void) => {
  const subscription = Notifications.addNotificationReceivedListener(callback);
  return () => subscription.remove();
};

// Listen for notification responses (when user taps on notification)
export const listenForNotificationResponses = (callback: (response: Notifications.NotificationResponse) => void) => {
  const subscription = Notifications.addNotificationResponseReceivedListener(callback);
  return () => subscription.remove();
};
```

### 4. Initialize Notifications in App.tsx

Initialize notifications and request permissions when your app starts. Update `apps/expo/App.tsx`:

```tsx
import { useEffect } from 'react';
import { LogBox } from 'react-native';
import { getPushToken, listenForNotifications, listenForNotificationResponses } from './src/utils/notifications';

LogBox.ignoreLogs(['Possible Unhandled Promise Rejection']);

export default function App() {
  useEffect(() => {
    const setupNotifications = async () => {
      const token = await getPushToken();
      if (token) {
        console.log('Push Token:', token);
        // Send this token to your backend (e.g., store in Supabase for the user)
        // Example: await storePushToken(token);
      }

      // Handle incoming notifications
      listenForNotifications((notification) => {
        console.log('Notification received:', notification);
        // Handle notification (e.g., update UI, navigate to a screen)
      });

      // Handle notification taps
      listenForNotificationResponses((response) => {
        console.log('Notification response:', response);
        // Navigate or perform action based on notification data
        const data = response.notification.request.content.data;
        if (data && data.screen) {
          // Navigate to specific screen
          // Example: navigation.navigate(data.screen);
        }
      });
    };

    setupNotifications();
  }, []);

  return (
    // Your app content
  );
}
```

### 5. Store Push Tokens in Supabase

Create a table in Supabase to store user push tokens for sending notifications later. Run this SQL in the Supabase dashboard:

```sql
CREATE TABLE IF NOT EXISTS user_push_tokens (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id UUID REFERENCES auth.users(id) ON DELETE CASCADE,
  push_token TEXT NOT NULL,
  device_type TEXT NOT NULL DEFAULT 'mobile',
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  UNIQUE (user_id, push_token)
);
```

After obtaining the push token, store it in Supabase for the logged-in user:

```tsx
import { supabase } from 'shared-ui';

const storePushToken = async (pushToken: string) => {
  const { data: { session } } = await supabase.auth.getSession();
  if (!session?.user) {
    console.error('No user logged in to store push token');
    return;
  }

  const { error } = await supabase.from('user_push_tokens').upsert({
    user_id: session.user.id,
    push_token: pushToken,
    device_type: 'mobile',
  });

  if (error) {
    console.error('Error storing push token:', error);
  } else {
    console.log('Push token stored successfully');
  }
};
```

## Setting Up Supabase Edge Functions for Sending Notifications

### 1. Create an Edge Function to Send Notifications

Create a new edge function to send push notifications using Expo's API. Run the following command to create a function:

```bash
supabase functions new send-push-notification
```

Update `supabase/functions/send-push-notification/index.ts` with the following code:

```typescript
import { serve } from "https://deno.land/std@0.131.0/http/server.ts";
import axios from "https://deno.land/x/axios@1.3.3/mod.ts";

serve(async (req) => {
  const { user_id, title, body, data } = await req.json();

  if (!user_id || !title || !body) {
    return new Response(JSON.stringify({ error: "Missing required fields" }), {
      headers: { "Content-Type": "application/json" },
      status: 400,
    });
  }

  // Get Supabase client (assumes SUPABASE_URL and SUPABASE_ANON_KEY are set in environment)
  const supabaseClient = createClient(
    Deno.env.get("SUPABASE_URL") ?? "",
    Deno.env.get("SUPABASE_ANON_KEY") ?? ""
  );

  // Fetch push tokens for the user
  const { data: tokens, error } = await supabaseClient
    .from("user_push_tokens")
    .select("push_token")
    .eq("user_id", user_id);

  if (error || !tokens || tokens.length === 0) {
    return new Response(JSON.stringify({ error: "No push tokens found for user" }), {
      headers: { "Content-Type": "application/json" },
      status: 404,
    });
  }

  // Send push notification to all tokens for this user
  const expoApiUrl = "https://exp.host/--/api/v2/push/send";
  const pushTokens = tokens.map((t: any) => t.push_token);

  try {
    const response = await axios.post(
      expoApiUrl,
      {
        to: pushTokens,
        title,
        body,
        data: data || {},
        sound: "default",
        priority: "default",
      },
      {
        headers: {
          "Content-Type": "application/json",
          "Accept": "application/json",
        },
      }
    );

    return new Response(JSON.stringify({ success: true, response: response.data }), {
      headers: { "Content-Type": "application/json" },
      status: 200,
    });
  } catch (error: any) {
    return new Response(JSON.stringify({ error: error.message }), {
      headers: { "Content-Type": "application/json" },
      status: 500,
    });
  }
});
```

### 2. Deploy the Edge Function

Deploy the function to your Supabase project:

```bash
supabase functions deploy send-push-notification
```

### 3. Call the Edge Function to Send Notifications

You can trigger notifications from your app or another backend service by calling this edge function. Example from a Next.js API route:

```typescript
// In apps/next/pages/api/send-notification.ts
export default async function handler(req: any, res: any) {
  if (req.method !== 'POST') {
    return res.status(405).json({ error: 'Method Not Allowed' });
  }

  const { user_id, title, body, data } = req.body;

  if (!user_id || !title || !body) {
    return res.status(400).json({ error: 'Missing required fields' });
  }

  try {
    const response = await fetch(
      `${process.env.NEXT_PUBLIC_SUPABASE_URL}/functions/v1/send-push-notification`,
      {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          Authorization: `Bearer ${process.env.NEXT_PUBLIC_SUPABASE_ANON_KEY}`,
        },
        body: JSON.stringify({ user_id, title, body, data }),
      }
    );

    const result = await response.json();
    return res.status(response.status).json(result);
  } catch (error: any) {
    return res.status(500).json({ error: error.message });
  }
}
```

Or call it directly from the client (ensure proper authentication):

```typescript
import { supabase } from 'shared-ui';

const sendNotification = async (userId: string, title: string, body: string, data?: any) => {
  const { data: result, error } = await supabase.functions.invoke('send-push-notification', {
    body: { user_id: userId, title, body, data },
  });

  if (error) {
    console.error('Error sending notification:', error);
    return { success: false, error };
  }

  return result;
};
```

## Best Practices

- **Permissions**: Always request notification permissions early in the user journey, explaining why notifications are beneficial.
- **Token Management**: Keep push tokens updated in Supabase when users log in/out or reinstall the app.
- **Personalization**: Tailor notifications based on user preferences or behavior for better engagement.
- **Rate Limiting**: Avoid spamming users with too many notifications. Implement rate limits or user controls.
- **Testing**: Test notifications on both iOS and Android devices using Expo's push notification tool ([https://expo.dev/notifications](https://expo.dev/notifications)).

## Troubleshooting

- **Permissions Denied**: If users deny notification permissions, provide a way to re-request or direct them to app settings.
- **Tokens Not Generating**: Ensure `getExpoPushTokenAsync` is called after permissions are granted. Verify your Expo project ID is correct.
- **Notifications Not Received**: Check if tokens are correctly stored in Supabase. Use Expo's push notification tool to test sending to specific tokens.
- **Edge Function Errors**: Check Supabase logs for function errors (`supabase functions logs send-push-notification`). Ensure dependencies like `axios` are correctly imported.
- **iOS Specific Issues**: For iOS, ensure your Apple Developer account has push notification capabilities enabled and certificates uploaded to Expo.

For more detailed information, refer to:
- [Expo Notifications](https://docs.expo.dev/versions/latest/sdk/notifications/)
- [Supabase Edge Functions](https://supabase.com/docs/guides/functions)
