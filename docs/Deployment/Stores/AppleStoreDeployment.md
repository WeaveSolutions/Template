Appl# Apple App Store Deployment Guide for Expo

This guide walks you through deploying your Expo app to the Apple App Store.

## Prerequisites

1. **Apple Developer Account** ($99/year)
   - Sign up at [developer.apple.com](https://developer.apple.com)
   
2. **macOS with Xcode** (for final submission)
   - Xcode 14+ recommended
   - Install from Mac App Store

3. **EAS CLI** (Expo Application Services)
   ```bash
   pnpm install -g eas-cli
   ```

## Step 1: Configure Your App

### Update app.json
Edit `apps/expo/app.json`:

```json
{
  "expo": {
    "name": "Your App Name",
    "slug": "your-app-slug",
    "version": "1.0.0",
    "orientation": "portrait",
    "icon": "./assets/icon.png",
    "userInterfaceStyle": "automatic",
    "splash": {
      "image": "./assets/splash.png",
      "resizeMode": "contain",
      "backgroundColor": "#ffffff"
    },
    "ios": {
      "bundleIdentifier": "com.yourcompany.yourapp",
      "buildNumber": "1",
      "supportsTablet": true,
      "infoPlist": {
        "NSCameraUsageDescription": "This app uses the camera to...",
        "NSPhotoLibraryUsageDescription": "This app accesses photos to..."
      }
    },
    "extra": {
      "eas": {
        "projectId": "your-project-id"
      }
    }
  }
}
```

### Create App Icons and Splash Screen
- **Icon**: 1024x1024px PNG (no transparency)
- **Splash**: 2732x2732px PNG (largest iPad size)

Place in `apps/expo/assets/`

## Step 2: Set Up EAS Build

### Login to EAS
```bash
cd apps/expo
eas login
```

### Configure EAS
```bash
eas build:configure
```

This creates `eas.json`. Update it:

```json
{
  "cli": {
    "version": ">= 5.0.0"
  },
  "build": {
    "development": {
      "developmentClient": true,
      "distribution": "internal",
      "ios": {
        "simulator": true
      }
    },
    "preview": {
      "distribution": "internal"
    },
    "production": {
      "ios": {
        "buildConfiguration": "Release"
      },
      "env": {
        "EXPO_PUBLIC_SUPABASE_URL": "your-production-url",
        "EXPO_PUBLIC_SUPABASE_ANON_KEY": "your-production-key"
      }
    }
  },
  "submit": {
    "production": {
      "ios": {
        "appleId": "your-apple-id@email.com",
        "ascAppId": "your-app-store-connect-app-id",
        "appleTeamId": "your-team-id"
      }
    }
  }
}
```

## Step 3: Create App Store Connect Entry

1. Go to [App Store Connect](https://appstoreconnect.apple.com)
2. Click "My Apps" → "+"
3. Fill in:
   - Platform: iOS
   - App Name
   - Primary Language
   - Bundle ID (must match app.json)
   - SKU (unique identifier)

## Step 4: Build for Production

### Install Dependencies
```bash
cd apps/expo
pnpm install
```

### Build iOS App
```bash
eas build --platform ios --profile production
```

This will:
- Prompt for Apple credentials
- Create provisioning profiles
- Build your app in the cloud
- Provide a download link for the .ipa file

## Step 5: Submit to App Store

### Option 1: Using EAS Submit (Recommended)
```bash
eas submit --platform ios --latest
```

### Option 2: Manual Submission
1. Download the .ipa file from EAS
2. Open Xcode
3. Window → Organizer
4. Drag .ipa to Organizer
5. Click "Distribute App"

## Step 6: App Store Connect Configuration

### App Information
- App Name
- Subtitle
- Category
- Content Rights

### Pricing and Availability
- Price (Free or Paid)
- Available territories

### App Privacy
- Privacy policy URL (required)
- Data collection practices

### Version Information
- Screenshots (required sizes):
  - 6.7" (1290 × 2796)
  - 6.5" (1284 × 2778 or 1242 × 2688)
  - 5.5" (1242 × 2208)
  - iPad Pro 12.9" (2048 × 2732)

- What's New (version notes)
- Keywords
- Support URL
- Marketing URL (optional)

### App Review Information
- Contact information
- Demo account (if needed)
- Notes for reviewer

## Step 7: Submit for Review

1. Ensure all required fields are complete
2. Click "Add for Review"
3. Answer export compliance questions
4. Submit to App Review

## Review Timeline
- Usually 24-48 hours
- Can take up to 7 days
- Expedited review available for critical issues

## Common Rejection Reasons

1. **Crashes or Bugs**
   - Test thoroughly on real devices
   - Check crash reports in Xcode

2. **Incomplete Information**
   - Provide detailed app description
   - Include all required screenshots

3. **Broken Links**
   - Verify all URLs work
   - Privacy policy must be accessible

4. **Guideline Violations**
   - Review [App Store Guidelines](https://developer.apple.com/app-store/review/guidelines/)
   - No placeholder content
   - Must use native iOS UI elements appropriately

## Production Checklist

- [ ] App icons (all sizes)
- [ ] Launch screen
- [ ] Screenshots for all device sizes
- [ ] App description and keywords
- [ ] Privacy policy URL
- [ ] Support URL
- [ ] Test on real devices
- [ ] Configure push notifications (if used)
- [ ] Set up analytics
- [ ] Configure crash reporting
- [ ] Update version number
- [ ] Create App Store Connect listing
- [ ] Submit for review

## Environment Variables for Production

Create `.env.production` in `apps/expo/`:

```bash
EXPO_PUBLIC_SUPABASE_URL=https://your-prod-project.supabase.co
EXPO_PUBLIC_SUPABASE_ANON_KEY=your-production-anon-key
EXPO_PUBLIC_API_URL=https://your-api.com
```

## Useful Commands

```bash
# Build for iOS simulator
eas build --platform ios --profile development

# Build for internal testing
eas build --platform ios --profile preview

# List builds
eas build:list

# Submit specific build
eas submit --platform ios --id=<build-id>

# Update over-the-air
eas update --branch production --message "Bug fixes"
```

## Post-Launch

1. **Monitor Crashes**
   - Use Sentry or Bugsnag
   - Check Xcode Organizer

2. **User Feedback**
   - Respond to App Store reviews
   - Monitor support emails

3. **Updates**
   - OTA updates for JS changes
   - New builds for native changes

## Resources

- [Expo iOS Distribution](https://docs.expo.dev/distribution/app-stores/)
- [App Store Connect Help](https://help.apple.com/app-store-connect/)
- [Human Interface Guidelines](https://developer.apple.com/design/human-interface-guidelines/)
- [App Store Review Guidelines](https://developer.apple.com/app-store/review/guidelines/)
