import { initializeApp, getApps, getApp } from 'firebase/app';
import { getAuth, signInWithCustomToken, onAuthStateChanged, signOut as firebaseSignOut } from 'firebase/auth';
import { getFirestore, doc, onSnapshot, setDoc, getDoc, collection, query, where, orderBy, limit } from 'firebase/firestore';
import { getStorage, ref, uploadBytes, getDownloadURL } from 'firebase/storage';
import { getMessaging, getToken, onMessage } from 'firebase/messaging';
import { getAnalytics } from 'firebase/analytics';

// Firebase configuration with safer fallback values
const firebaseConfig = {
  apiKey: process.env.NEXT_PUBLIC_FIREBASE_API_KEY || 'placeholder-api-key',
  authDomain: process.env.NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN || 'placeholder-auth-domain',
  projectId: process.env.NEXT_PUBLIC_FIREBASE_PROJECT_ID || 'placeholder-project-id',
  storageBucket: process.env.NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET || 'placeholder-storage-bucket',
  messagingSenderId: process.env.NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID || 'placeholder-sender-id',
  appId: process.env.NEXT_PUBLIC_FIREBASE_APP_ID || 'placeholder-app-id',
  measurementId: process.env.NEXT_PUBLIC_FIREBASE_MEASUREMENT_ID || 'placeholder-measurement-id',
};

// Check if GCP is enabled
const isGcpEnabled = process.env.NEXT_PUBLIC_GCP_ENABLED === 'true' || process.env.EXPO_PUBLIC_GCP_ENABLED === 'true';

// Initialize Firebase only on client side and if GCP is enabled
let firebaseApp: any = null;
let isInitialized = false;
if (typeof window !== 'undefined' && isGcpEnabled) {
  try {
    firebaseApp = initializeApp(firebaseConfig);
    isInitialized = true;
    console.log('Firebase initialized successfully');
  } catch (error) {
    console.error('Firebase initialization failed:', error);
  }
} else if (!isGcpEnabled) {
  console.log('Firebase initialization skipped: GCP is not enabled');
}

// Initialize Analytics only if API key is provided, not a placeholder, and GCP is enabled
if (firebaseApp && typeof window !== 'undefined' && isGcpEnabled) {
  const apiKey = firebaseConfig.apiKey;
  if (apiKey && apiKey !== 'placeholder-api-key') {
    try {
      getAnalytics(firebaseApp);
      console.log('Firebase Analytics initialized');
    } catch (error) {
      console.error('Error initializing Firebase Analytics:', error);
    }
  } else {
    console.warn('Firebase Analytics not initialized: Missing or placeholder API key');
  }
} else if (!isGcpEnabled) {
  console.log('Firebase Analytics initialization skipped: GCP is not enabled');
}

// Initialize other Firebase services with null checks
export const auth = firebaseApp && typeof window !== 'undefined' && isGcpEnabled && isInitialized ? getAuth(firebaseApp) : null;
export const db = firebaseApp && typeof window !== 'undefined' && isGcpEnabled && isInitialized ? getFirestore(firebaseApp) : null;
export const storage = firebaseApp && typeof window !== 'undefined' && isGcpEnabled && isInitialized ? getStorage(firebaseApp) : null;

// Initialize messaging only in browser with service worker support
export const messaging = typeof window !== 'undefined' && 'serviceWorker' in navigator && isGcpEnabled && firebaseApp && isInitialized
  ? getMessaging(firebaseApp)
  : null;

// Utility function to get Firebase app instance
export function getFirebaseApp() {
  return firebaseApp && isGcpEnabled && isInitialized ? firebaseApp : null;
}

// Token exchange URL (from Terraform output or environment variable)
const TOKEN_EXCHANGE_URL = process.env.NEXT_PUBLIC_TOKEN_EXCHANGE_URL || '';

/**
 * Exchange Auth0 token for Firebase custom token
 */
export async function exchangeAuth0Token(auth0Token: string): Promise<string> {
  if (!isGcpEnabled || !firebaseApp || !isInitialized) {
    console.log('Firebase token exchange skipped: GCP not enabled or Firebase not initialized');
    throw new Error('Firebase not initialized or GCP not enabled');
  }

  try {
    const response = await fetch(TOKEN_EXCHANGE_URL, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${auth0Token}`,
      },
    });

    if (!response.ok) throw new Error(`Token exchange failed with status: ${response.status}`);
    const data = await response.json();
    return data.firebaseToken;
  } catch (error) {
    console.error('Error exchanging Auth0 token:', error);
    throw error;
  }
}

/**
 * Sign in to Firebase with Auth0 token
 */
export async function signInWithAuth0(auth0Token: string): Promise<any> {
  try {
    // Exchange Auth0 token for Firebase token
    const firebaseToken = await exchangeAuth0Token(auth0Token);
    
    // Sign in to Firebase
    if (auth) {
      const userCredential = await signInWithCustomToken(auth, firebaseToken);
      return userCredential.user;
    } else {
      throw new Error('Auth not initialized');
    }
  } catch (error) {
    console.error('Error signing in with Auth0 token:', error);
    throw error;
  }
}

/**
 * Sign out from Firebase
 */
export async function signOut(): Promise<void> {
  if (auth) {
    await firebaseSignOut(auth);
  }
}

/**
 * Subscribe to auth state changes
 */
export function subscribeToAuthState(callback: (user: any) => void) {
  if (auth) {
    return onAuthStateChanged(auth, callback);
  }
  return () => {};
}

/**
 * Get current user with claims
 */
export async function getCurrentUserWithClaims() {
  if (!auth) return null;
  const user = auth.currentUser;
  if (!user) return null;

  const idTokenResult = await user.getIdTokenResult();
  return {
    ...user,
    claims: idTokenResult.claims,
  };
}

/**
 * Subscribe to user profile updates
 */
export function subscribeToUserProfile(
  userId: string,
  callback: (profile: any) => void
) {
  if (!db) return () => {};
  const userRef = doc(db, 'users', userId);
  return onSnapshot(userRef, (docSnapshot) => {
    if (docSnapshot.exists()) {
      callback({ id: docSnapshot.id, ...docSnapshot.data() });
    } else {
      callback(null);
    }
  });
}

/**
 * Update user profile
 */
export async function updateUserProfile(userId: string, data: any) {
  if (!db) return;
  const userRef = doc(db, 'users', userId);
  await setDoc(userRef, {
    ...data,
    updatedAt: new Date(),
  }, { merge: true });
}

/**
 * Upload file to Firebase Storage
 */
export async function uploadFile(
  path: string,
  file: File | Blob,
  metadata?: any
): Promise<string> {
  if (!storage) throw new Error('Storage not initialized');
  const storageRef = ref(storage, path);
  const snapshot = await uploadBytes(storageRef, file, metadata);
  return getDownloadURL(snapshot.ref);
}

/**
 * Subscribe to chat messages
 */
export function subscribeToChatMessages(
  chatId: string,
  callback: (messages: any[]) => void,
  limitCount: number = 50
) {
  if (!db) return () => {};
  const messagesRef = collection(db, 'chats', chatId, 'messages');
  const q = query(
    messagesRef,
    orderBy('createdAt', 'desc'),
    limit(limitCount)
  );

  return onSnapshot(q, (snapshot) => {
    const messages = snapshot.docs.map(docSnapshot => ({
      id: docSnapshot.id,
      ...docSnapshot.data(),
    }));
    callback(messages.reverse());
  });
}

/**
 * Send chat message
 */
export async function sendChatMessage(
  chatId: string,
  message: {
    content: string;
    type?: string;
    attachments?: string[];
  }
) {
  if (!auth || !db) throw new Error('Auth or DB not initialized');
  const user = auth.currentUser;
  if (!user) throw new Error('User not authenticated');

  const messagesRef = collection(db, 'chats', chatId, 'messages');
  await setDoc(doc(messagesRef), {
    authorId: user.uid,
    authorName: user.displayName,
    authorPhoto: user.photoURL,
    content: message.content,
    type: message.type || 'text',
    attachments: message.attachments || [],
    createdAt: new Date(),
  });
}

/**
 * Subscribe to user notifications
 */
export function subscribeToNotifications(
  userId: string,
  callback: (notifications: any[]) => void
) {
  if (!db) return () => {};
  const notificationsRef = collection(db, 'notifications', userId, 'items');
  const q = query(
    notificationsRef,
    where('read', '==', false),
    orderBy('createdAt', 'desc')
  );

  return onSnapshot(q, (snapshot) => {
    const notifications = snapshot.docs.map(docSnapshot => ({
      id: docSnapshot.id,
      ...docSnapshot.data(),
    }));
    callback(notifications);
  });
}

/**
 * Register for push notifications
 */
export async function registerForPushNotifications() {
  if (!messaging) return null;

  try {
    const permission = await Notification.requestPermission();
    if (permission === 'granted') {
      const token = await getToken(messaging, {
        vapidKey: process.env.NEXT_PUBLIC_FIREBASE_VAPID_KEY,
      });
      
      console.log('FCM Token:', token);
      if (auth && db) {
        const user = auth.currentUser;
        if (user) {
          await updateUserProfile(user.uid, {
            fcmToken: token,
          });
        }
      }
      return token;
    } else {
      console.warn('Push notification permission denied');
      return null;
    }
  } catch (error) {
    console.error('Error registering for push notifications:', error);
    return null;
  }
}

/**
 * Listen for foreground messages
 */
export function onForegroundMessage(callback: (payload: any) => void) {
  if (!messaging) return () => {};
  
  return onMessage(messaging, callback);
}

/**
 * Firebase + Auth0 integration logic
 */
export async function getFirebaseTokenFromAuth0(auth0Token: string): Promise<string | null> {
  if (!isGcpEnabled || !firebaseApp || !isInitialized) {
    console.log('Firebase token exchange skipped: GCP not enabled or Firebase not initialized');
    return null;
  }
  // Implement API call to exchange Auth0 token for Firebase token
  // This could call a backend endpoint that verifies the Auth0 token
  // and mints a Firebase custom token
  try {
    // Placeholder for actual implementation
    console.log('Exchanging Auth0 token for Firebase token');
    return auth0Token; // Replace with actual Firebase token
  } catch (error) {
    console.error('Error exchanging Auth0 token for Firebase token', error);
    return null;
  }
}

/**
 * Check if user has a specific permission
 */
export async function hasPermission(permission: string): Promise<boolean> {
  const userWithClaims = await getCurrentUserWithClaims();
  if (!userWithClaims) return false;
  
  return Array.isArray(userWithClaims.claims.permissions) && userWithClaims.claims.permissions.includes(permission) || false;
}

/**
 * Check if user has a specific role
 */
export async function hasRole(role: string): Promise<boolean> {
  const userWithClaims = await getCurrentUserWithClaims();
  if (!userWithClaims) return false;
  
  return Array.isArray(userWithClaims.claims.roles) && userWithClaims.claims.roles.includes(role) || false;
}

// Function to get user auth providers
function getUserAuthProviders(user: any): string[] {
  const providerData = user?.providerData || [];
  return providerData
    .map((pd: any) => pd?.providerId || '')
    .filter((id: string) => id && typeof id === 'string' && id.length > 0);
}

// Check if user is linked with Auth0
function isAuth0Linked(user: any): boolean {
  const providers = getUserAuthProviders(user);
  return providers.some(provider => provider && typeof provider === 'string' && provider.includes('oidc.auth0'));
}

// Check if user is anonymous
function isAnonymousUser(user: any): boolean {
  const providers = getUserAuthProviders(user);
  return providers.length === 0 || providers.some(provider => provider && typeof provider === 'string' && provider.includes('anonymous'));
}
