import { writable } from "svelte/store";

export interface User {
  id: string;
  name: string;
  email: string;
  avatar_url?: string;
}

export interface AuthState {
  user: User | null;
  isAuthenticated: boolean;
  isLoading: boolean;
}

const initialState: AuthState = {
  user: null,
  isAuthenticated: false,
  isLoading: false,
};

function createAuthStore() {
  const { subscribe, set, update } = writable(initialState);

  return {
    subscribe,
    setUser: (user: User) => {
      update((state) => ({
        ...state,
        user,
        isAuthenticated: true,
        isLoading: false,
      }));
    },
    clearUser: () => {
      set(initialState);
    },
    setLoading: (isLoading: boolean) => {
      update((state) => ({
        ...state,
        isLoading,
      }));
    },
    updateUser: (userData: Partial<User>) => {
      update((state) => ({
        ...state,
        user: state.user ? { ...state.user, ...userData } : null,
      }));
    },
  };
}

export const authStore = createAuthStore();
