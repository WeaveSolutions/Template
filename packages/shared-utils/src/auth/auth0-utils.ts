import * as AuthSession from 'expo-auth-session';
import * as WebBrowser from 'expo-web-browser';
import * as SecureStore from 'expo-secure-store';
import { Platform } from 'react-native';
import jwtDecode from 'jwt-decode';

// Types
type TokenResponse = {
  access_token: string;
  id_token: string;
  expires_in: number;
  token_type: string;
  scope: string;
};

type User = {
  email: string;
  email_verified: boolean;
  name?: string;
  nickname?: string;
  picture?: string;
  sub: string;
  updated_at?: string;
  [key: string]: any;
};

// Configuration
const config = {
  domain: process.env.EXPO_PUBLIC_AUTH0_DOMAIN || '',
  clientId: process.env.EXPO_PUBLIC_AUTH0_CLIENT_ID || '',
  audience: process.env.EXPO_PUBLIC_AUTH0_AUDIENCE || '',
  redirectUri: AuthSession.makeRedirectUri({
    scheme: 'your.app',
    path: 'auth/callback',
  }),
};

// Storage keys
const STORAGE_KEYS = {
  ACCESS_TOKEN: 'auth0_access_token',
  ID_TOKEN: 'auth0_id_token',
  EXPIRES_AT: 'auth0_expires_at',
  USER: 'auth0_user',
};

// Auth0 Authentication API
class Auth0Client {
  private discovery: AuthSession.DiscoveryDocument | null = null;
  private tokenResponse: TokenResponse | null = null;
  private user: User | null = null;

  // Initialize the Auth0 client
  async initialize() {
    try {
      this.discovery = await AuthSession.fetchDiscoveryAsync(
        `https://${config.domain}`
      );
      
      // Try to load existing session
      await this.loadSession();
      return true;
    } catch (error) {
      console.error('Auth0 initialization error:', error);
      return false;
    }
  }

  // Login with Auth0 Universal Login
  async login() {
    if (!this.discovery) {
      await this.initialize();
    }

    try {
      const authRequest = new AuthSession.AuthRequest({
        clientId: config.clientId,
        scopes: ['openid', 'profile', 'email', 'offline_access'],
        redirectUri: config.redirectUri,
        usePKCE: true,
        responseType: AuthSession.ResponseType.Code,
        extraParams: {
          audience: config.audience,
        },
      });

      const result = await authRequest.promptAsync(this.discovery!, {
        useProxy: Platform.select({ web: false, default: true }),
        createTask: true,
      });

      if (result.type === 'success') {
        const tokenResult = await AuthSession.exchangeCodeAsync(
          {
            clientId: config.clientId,
            redirectUri: config.redirectUri,
            code: result.params.code,
            extraParams: {
              code_verifier: authRequest.codeVerifier || '',
            },
          },
          this.discovery!
        );

        await this.setSession(tokenResult);
        return true;
      }
      return false;
    } catch (error) {
      console.error('Login error:', error);
      throw error;
    }
  }

  // Logout the user
  async logout() {
    try {
      // Clear local storage
      await Promise.all([
        SecureStore.deleteItemAsync(STORAGE_KEYS.ACCESS_TOKEN),
        SecureStore.deleteItemAsync(STORAGE_KEYS.ID_TOKEN),
        SecureStore.deleteItemAsync(STORAGE_KEYS.EXPIRES_AT),
        SecureStore.deleteItemAsync(STORAGE_KEYS.USER),
      ]);

      // Clear in-memory state
      this.tokenResponse = null;
      this.user = null;

      // Redirect to Auth0 logout
      const logoutUrl = `https://${config.domain}/v2/logout?client_id=${config.clientId}&returnTo=${encodeURIComponent(
        config.redirectUri
      )}`;
      
      if (Platform.OS === 'web') {
        window.location.href = logoutUrl;
      } else {
        await WebBrowser.openBrowserAsync(logoutUrl);
      }

      return true;
    } catch (error) {
      console.error('Logout error:', error);
      return false;
    }
  }

  // Get the current user
  async getUser(): Promise<User | null> {
    if (this.user) {
      return this.user;
    }

    try {
      const userJson = await SecureStore.getItemAsync(STORAGE_KEYS.USER);
      if (userJson) {
        this.user = JSON.parse(userJson);
        return this.user;
      }
      return null;
    } catch (error) {
      console.error('Get user error:', error);
      return null;
    }
  }

  // Get the access token
  async getAccessToken(): Promise<string | null> {
    if (this.tokenResponse?.access_token) {
      return this.tokenResponse.access_token;
    }

    try {
      const accessToken = await SecureStore.getItemAsync(STORAGE_KEYS.ACCESS_TOKEN);
      if (accessToken) {
        return accessToken;
      }
      return null;
    } catch (error) {
      console.error('Get access token error:', error);
      return null;
    }
  }

  // Check if the user is authenticated
  async isAuthenticated(): Promise<boolean> {
    try {
      const expiresAt = await SecureStore.getItemAsync(STORAGE_KEYS.EXPIRES_AT);
      if (!expiresAt) return false;

      const expiresAtMs = parseInt(expiresAt, 10);
      return new Date().getTime() < expiresAtMs;
    } catch (error) {
      console.error('Auth check error:', error);
      return false;
    }
  }

  // Handle authentication callback
  async handleCallback(url: string) {
    if (url.includes('auth/callback')) {
      const parsedUrl = new URL(url);
      const code = parsedUrl.searchParams.get('code');
      
      if (code) {
        const tokenResult = await AuthSession.exchangeCodeAsync(
          {
            clientId: config.clientId,
            redirectUri: config.redirectUri,
            code,
          },
          this.discovery!
        );

        await this.setSession(tokenResult);
        return true;
      }
    }
    return false;
  }

  // Private methods
  private async setSession(tokenResponse: TokenResponse) {
    this.tokenResponse = tokenResponse;
    
    // Decode ID token to get user info
    const decodedToken = jwtDecode<User>(tokenResponse.id_token);
    this.user = decodedToken;
    
    // Calculate expiration time
    const expiresAt = JSON.stringify(
      (tokenResponse.expires_in || 86400) * 1000 + new Date().getTime()
    );

    // Store tokens and user info
    await Promise.all([
      SecureStore.setItemAsync(STORAGE_KEYS.ACCESS_TOKEN, tokenResponse.access_token),
      SecureStore.setItemAsync(STORAGE_KEYS.ID_TOKEN, tokenResponse.id_token),
      SecureStore.setItemAsync(STORAGE_KEYS.EXPIRES_AT, expiresAt),
      SecureStore.setItemAsync(STORAGE_KEYS.USER, JSON.stringify(decodedToken)),
    ]);
  }

  private async loadSession() {
    try {
      const [accessToken, idToken, expiresAt, userJson] = await Promise.all([
        SecureStore.getItemAsync(STORAGE_KEYS.ACCESS_TOKEN),
        SecureStore.getItemAsync(STORAGE_KEYS.ID_TOKEN),
        SecureStore.getItemAsync(STORAGE_KEYS.EXPIRES_AT),
        SecureStore.getItemAsync(STORAGE_KEYS.USER),
      ]);

      if (accessToken && idToken && expiresAt && userJson) {
        this.tokenResponse = {
          access_token: accessToken,
          id_token: idToken,
          expires_in: 86400, // Default to 24h
          token_type: 'Bearer',
          scope: 'openid profile email',
        };
        
        this.user = JSON.parse(userJson);
        return true;
      }
      return false;
    } catch (error) {
      console.error('Load session error:', error);
      return false;
    }
  }
}

export const auth0Client = new Auth0Client();

// Helper hooks for React components
export const useAuth = () => {
  const [isLoading, setIsLoading] = React.useState(true);
  const [isAuthenticated, setIsAuthenticated] = React.useState(false);
  const [user, setUser] = React.useState<User | null>(null);
  const [error, setError] = React.useState<Error | null>(null);

  // Initialize auth state
  React.useEffect(() => {
    const initAuth = async () => {
      try {
        await auth0Client.initialize();
        const isAuth = await auth0Client.isAuthenticated();
        const currentUser = await auth0Client.getUser();
        
        setIsAuthenticated(isAuth);
        setUser(currentUser);
      } catch (err) {
        setError(err as Error);
      } finally {
        setIsLoading(false);
      }
    };

    initAuth();
  }, []);

  // Handle deep links (for mobile)
  React.useEffect(() => {
    const handleDeepLink = async (event: { url: string }) => {
      try {
        await auth0Client.handleCallback(event.url);
        const isAuth = await auth0Client.isAuthenticated();
        const currentUser = await auth0Client.getUser();
        
        setIsAuthenticated(isAuth);
        setUser(currentUser);
      } catch (err) {
        setError(err as Error);
      }
    };

    // Subscribe to deep links
    const subscription = AuthSession.addDeepLinkListener(handleDeepLink);
    return () => subscription?.remove();
  }, []);

  return {
    isLoading,
    isAuthenticated,
    user,
    error,
    login: auth0Client.login.bind(auth0Client),
    logout: auth0Client.logout.bind(auth0Client),
    getAccessToken: auth0Client.getAccessToken.bind(auth0Client),
  };
};

// Higher Order Component for protected routes
export const withAuth = <P extends object>(
  Component: React.ComponentType<P>
) => {
  const WithAuth: React.FC<P> = (props) => {
    const { isLoading, isAuthenticated, login } = useAuth();

    React.useEffect(() => {
      if (!isLoading && !isAuthenticated) {
        login();
      }
    }, [isLoading, isAuthenticated, login]);

    if (isLoading) {
      return <div>Loading...</div>; // Or your custom loading component
    }

    if (!isAuthenticated) {
      return null; // Or redirect to login
    }

    return <Component {...props} />;
  };

  return WithAuth;
};
