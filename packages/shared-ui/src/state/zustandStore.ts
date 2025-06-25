import { create } from 'zustand';
import { persist } from 'zustand/middleware';

interface UserState {
  user: {
    id: string;
    email: string;
    fullName?: string;
  } | null;
  isAuthenticated: boolean;
  setUser: (user: { id: string; email: string; fullName?: string } | null) => void;
  setAuthenticated: (isAuthenticated: boolean) => void;
  logout: () => void;
}

// User authentication state with persistence
export const useUserStore = create<UserState>()(
  persist(
    (set) => ({
      user: null,
      isAuthenticated: false,
      setUser: (user) => set({ user }),
      setAuthenticated: (isAuthenticated) => set({ isAuthenticated }),
      logout: () => set({ user: null, isAuthenticated: false }),
    }),
    {
      name: 'user-storage', // key for localStorage
    }
  )
);

interface ThemeState {
  theme: 'light' | 'dark';
  setTheme: (theme: 'light' | 'dark') => void;
  toggleTheme: () => void;
}

// Theme state with persistence
export const useThemeStore = create<ThemeState>()(
  persist(
    (set, get) => ({
      theme: 'light',
      setTheme: (theme) => set({ theme }),
      toggleTheme: () => set({ theme: get().theme === 'light' ? 'dark' : 'light' }),
    }),
    {
      name: 'theme-storage',
    }
  )
);

interface AppState {
  isLoading: boolean;
  error: string | null;
  setLoading: (isLoading: boolean) => void;
  setError: (error: string | null) => void;
  clearError: () => void;
}

// General app state
export const useAppStore = create<AppState>()((set) => ({
  isLoading: false,
  error: null,
  setLoading: (isLoading) => set({ isLoading }),
  setError: (error) => set({ error }),
  clearError: () => set({ error: null }),
}));
