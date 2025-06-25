# Google Play Store Deployment Guide for Expo

This guide walks you through deploying your Expo app to the Google Play Store.

## Prerequisites

1. **Google Play Developer Account** ($25 one-time fee)
   - Sign up at [play.google.com/console](https://play.google.com/console)

2. **EAS CLI** (Expo Application Services)
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
    "android": {
      "package": "com.yourcompany.yourapp",
      "versionCode": 1,
      "adaptiveIcon": {
        "foregroundImage": "./assets/adaptive-icon.png",
        "backgroundColor": "#FFFFFF"
      },
      "permissions": [
        "CAMERA",
        "READ_EXTERNAL_STORAGE"
      ]
    },
    "extra": {
      "eas": {
        "projectId": "your-project-id"
      }
    }
  }
}
```

### Create App Assets
- **Icon**: 512x512px PNG
- **Adaptive Icon**: 108x108px PNG (foreground)
- **Splash**: 1920x1080px PNG
- **Feature Graphic**: 1024x500px (for Play Store)

Place icons in `apps/expo/assets/`

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

Update `eas.json`:

```json
{
  "cli": {
    "version": ">= 5.0.0"
  },
  "build": {
    "development": {
      "developmentClient": true,
      "distribution": "internal",
      "android": {
        "buildType": "apk",
        "gradleCommand": ":app:assembleDebug"
      }
    },
    "preview": {
      "distribution": "internal",
      "android": {
        "buildType": "apk"
      }
    },
    "production": {
      "android": {
        "buildType": "app-bundle",
        "gradleCommand": ":app:bundleRelease"
      },
      "env": {
        "EXPO_PUBLIC_SUPABASE_URL": "your-production-url",
        "EXPO_PUBLIC_SUPABASE_ANON_KEY": "your-production-key"
      }
    }
  },
  "submit": {
    "production": {
      "android": {
        "track": "production",
        "releaseStatus": "completed",
        "rollout": 1.0
      }
    }
  }
}
```

## Step 3: Generate Upload Key

### First Build
When you run your first production build, EAS will:
1. Generate an upload keystore
2. Store it securely
3. Use it for all future builds

```bash
eas build --platform android --profile production
```

### Manual Key (Optional)
If you need to use your own keystore:

```bash
keytool -genkey -v -keystore my-upload-key.keystore -alias my-key-alias -keyalg RSA -keysize 2048 -validity 10000
```

## Step 4: Create Google Play Console Entry

1. Go to [Google Play Console](https://play.google.com/console)
2. Click "Create app"
3. Fill in:
   - App name
   - Default language
   - App or game
   - Free or paid
4. Complete declarations

## Step 5: Build for Production

### Install Dependencies
```bash
cd apps/expo
pnpm install
```

### Build Android App Bundle
```bash
eas build --platform android --profile production
```

This will:
- Build your app in the cloud
- Generate an .aab file (Android App Bundle)
- Provide a download link

## Step 6: Submit to Google Play

### Option 1: Using EAS Submit (Recommended)
```bash
eas submit --platform android --latest
```

You'll need:
- Google Service Account Key (see below)
- Or manual upload credentials

### Option 2: Manual Upload
1. Download the .aab file
2. Go to Google Play Console
3. Navigate to Release > Production
4. Create new release
5. Upload .aab file

### Creating Service Account (for EAS Submit)
1. Google Cloud Console → Create Project
2. Enable Google Play Android Developer API
3. Create Service Account
4. Download JSON key
5. Add to Play Console users with "Release Manager" role

## Step 7: Play Store Listing

### Store Listing
- **Title** (30 chars max)
- **Short Description** (80 chars)
- **Full Description** (4000 chars)
- **Screenshots** (min 2, max 8):
  - Phone: 16:9 or 9:16
  - 7-inch tablet (optional)
  - 10-inch tablet (optional)

### Graphics Assets
- **App Icon**: 512x512px PNG
- **Feature Graphic**: 1024x500px
- **Promo Video** (optional): YouTube URL

### Categorization
- Application type
- Category
- Content rating questionnaire
- Target audience and content
- Privacy policy URL (required)

### Contact Details
- Email
- Phone (optional)
- Website

## Step 8: App Content

### Privacy Policy
- Required for all apps
- Must be hosted on active URL
- Should cover data collection and usage

### App Access
If login required:
- Provide test credentials
- Instructions for reviewers

### Content Rating
Complete questionnaire covering:
- Violence
- Sexual content
- Language
- Controlled substances
- Miscellaneous

## Step 9: Pricing & Distribution

### Pricing
- Free or set price
- In-app purchases setup (if applicable)

### Countries
- Select distribution countries
- Consider legal requirements
- Export compliance

### Device Categories
- Phone and tablet (default)
- Wear OS
- Android TV
- Android Auto

## Step 10: Release

### Testing Tracks
1. **Internal Testing** (100 testers)
2. **Closed Testing** (custom lists)
3. **Open Testing** (unlimited)

### Production Release
1. Start with limited rollout (e.g., 10%)
2. Monitor crash reports and feedback
3. Increase rollout percentage
4. Full release

## Common Issues

### App Rejected
Common reasons:
- Policy violations
- Metadata issues
- Broken functionality
- Missing privacy policy

### Signing Issues
- Use same keystore for updates
- EAS handles this automatically
- Keep keystore backup

### Version Codes
- Must increment for each release
- EAS auto-increments
- Manual: update in app.json

## Production Checklist

- [ ] App icons and graphics ready
- [ ] Screenshots for all device types
- [ ] Store listing complete
- [ ] Privacy policy published
- [ ] Content rating completed
- [ ] Test on real devices
- [ ] Production environment variables set
- [ ] Version code incremented
- [ ] APK/AAB size optimized (<150MB)
- [ ] ProGuard rules configured (if needed)

## Environment Variables

Create `.env.production` in `apps/expo/`:

```bash
EXPO_PUBLIC_SUPABASE_URL=https://your-prod-project.supabase.co
EXPO_PUBLIC_SUPABASE_ANON_KEY=your-production-anon-key
EXPO_PUBLIC_API_URL=https://your-api.com
```

## Useful Commands

```bash
# Build APK for testing
eas build --platform android --profile preview

# Build AAB for production
eas build --platform android --profile production

# List builds
eas build:list --platform android

# Submit to Play Store
eas submit --platform android --latest

# Update over-the-air
eas update --branch production --message "Bug fixes"
```

## Post-Launch

### Monitor Performance
- Crash reports in Play Console
- User feedback and ratings
- Vitals (ANRs, crashes)

### Updates
- OTA updates for JS/assets
- New builds for native changes
- Staged rollouts recommended

### Respond to Reviews
- Address user concerns
- Thank positive reviews
- Fix reported issues

## Resources

- [Expo Android Distribution](https://docs.expo.dev/distribution/app-stores/)
- [Google Play Console Help](https://support.google.com/googleplay/android-developer)
- [Android App Bundle](https://developer.android.com/guide/app-bundle)
- [Play Store Policies](https://play.google.com/about/developer-content-policy/)
