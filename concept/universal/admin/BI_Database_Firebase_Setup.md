# Firebase Database Setup for BI Dashboard - Nexpo & Taurte

**Database:** Firebase (Firestore + Auth)  
**Visualization:** Looker Studio  
**Platforms:** Next.js, Expo, Svelte, Tauri

---

## Checklist Overview

- [ ] Firebase project setup
- [ ] Firestore database schema
- [ ] Auth0 + Firebase integration
- [ ] BigQuery export configuration
- [ ] Looker Studio dashboard creation
- [ ] Nexpo implementation
- [ ] Taurte implementation
- [ ] Embed integration
- [ ] Security rules
- [ ] Testing and deployment

---

## 1. Firebase Project Setup

### Initial Configuration
- [ ] Create Firebase project at console.firebase.google.com
- [ ] Enable Firestore Database (production mode)
- [ ] Enable Firebase Authentication
- [ ] Enable BigQuery integration
- [ ] Set up Firebase Admin SDK
- [ ] Configure Blaze Plan for BigQuery

### App Registration
- [ ] Create web app configuration
- [ ] Create iOS app (Expo/Tauri Mobile)
- [ ] Create Android app (Expo/Tauri Mobile)
- [ ] Download google-services.json
- [ ] Download GoogleService-Info.plist
- [ ] Save Firebase config objects

---

## 2. Firestore Collections

### Users Collection
```
/users/{userId}
- uid, email, displayName, roles[], createdAt, lastLogin
```

### Subscriptions Collection
```
/subscriptions/{subscriptionId}
- userId, plan, status, mrr, startDate, billingCycle
```

### Analytics Events Collection
```
/analytics_events/{eventId}
- eventType, userId, timestamp, platform, properties{}
```

### KPI Snapshots Collection
```
/kpi_snapshots/{date}
- date, timestamp, metrics{mrr, arr, totalUsers, activeUsers, etc}
```

---

## 3. Security Rules
- [ ] Implement role-based access
- [ ] Users can read own data, admins read all
- [ ] Analytics events: admin-only read
- [ ] KPI snapshots: admin/analyst read

---

## 4. Auth0 Integration
- [ ] Create Auth0 Post-Login Action
- [ ] Generate Firebase custom tokens
- [ ] Sync user roles to Firestore
- [ ] Store Firebase token in Auth0 claims
- [ ] Configure Auth0 secrets

---

## 5. BigQuery Export
- [ ] Link Firebase to BigQuery
- [ ] Enable daily Firestore export
- [ ] Wait 24 hours for initial data
- [ ] Create aggregation SQL views
- [ ] Test query performance

---

## 6. Looker Studio Setup
- [ ] Create new report at lookerstudio.google.com
- [ ] Connect BigQuery data source
- [ ] Design Executive Summary page
- [ ] Design Revenue Analytics page
- [ ] Design User Metrics page
- [ ] Configure RBAC permissions
- [ ] Generate embed URL
- [ ] Test dashboard loads

---

## 7. Nexpo Implementation

### Next.js Web
- [ ] Install: firebase, firebase-admin, @auth0/nextjs-auth0
- [ ] Create lib/firebase.ts client
- [ ] Create lib/firebase-admin.ts server
- [ ] Create API routes: /api/bi/users, /api/bi/revenue
- [ ] Create LookerStudioEmbed component
- [ ] Add to dashboard page
- [ ] Test role-based access

### Expo Mobile
- [ ] Install: firebase, react-native-webview, react-native-auth0
- [ ] Initialize Firebase with AsyncStorage
- [ ] Create BiDashboardScreen with WebView
- [ ] Implement role checking
- [ ] Test on iOS and Android

### Tauri Desktop
- [ ] Use Next.js Firebase client
- [ ] Ensure webview access to Firebase
- [ ] Embed Looker Studio iframe
- [ ] Test desktop build

---

## 8. Taurte Implementation

### Svelte Web
- [ ] Install: firebase, @auth0/auth0-spa-js
- [ ] Create lib/firebase.ts
- [ ] Create stores/biData.ts
- [ ] Create LookerStudioEmbed.svelte
- [ ] Add to dashboard route
- [ ] Test authentication flow

### Tauri Mobile/Desktop
- [ ] Add reqwest to Cargo.toml
- [ ] Create Tauri command: fetch_bi_metrics
- [ ] Call Firebase REST API from Rust
- [ ] Integrate with Svelte frontend
- [ ] Test on all platforms

---

## Success Criteria
- [ ] Admin users authenticate via Auth0
- [ ] Firebase custom tokens generated
- [ ] User roles stored in Firestore
- [ ] BigQuery exports daily data
- [ ] Looker Studio shows live metrics
- [ ] Dashboards embed in all platforms
- [ ] Role-based access enforced
- [ ] MRR, ARR, Users display correctly
