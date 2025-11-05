import type { User } from '@auth0/auth0-spa-js';

interface AuthState {
  isLoading: boolean;
  isAuthenticated: boolean;
  user: User | null;
  error: string | null;
}

class AuthStore {
  private state = $state<AuthState>({
    isLoading: true,
    isAuthenticated: false,
    user: null,
    error: null
  });

  get isLoading() {
    return this.state.isLoading;
  }

  get isAuthenticated() {
    return this.state.isAuthenticated;
  }

  get user() {
    return this.state.user;
  }

  get error() {
    return this.state.error;
  }

  setLoading(loading: boolean) {
    this.state.isLoading = loading;
  }

  setAuthenticated(authenticated: boolean) {
    this.state.isAuthenticated = authenticated;
  }

  setUser(user: User | null) {
    this.state.user = user;
  }

  setError(error: string | null) {
    this.state.error = error;
  }

  login(user: User) {
    this.state.isAuthenticated = true;
    this.state.user = user;
    this.state.error = null;
    this.state.isLoading = false;
  }

  logout() {
    this.state.isAuthenticated = false;
    this.state.user = null;
    this.state.error = null;
    this.state.isLoading = false;
  }

  reset() {
    this.state = {
      isLoading: true,
      isAuthenticated: false,
      user: null,
      error: null
    };
  }
}

export const authStore = new AuthStore();
