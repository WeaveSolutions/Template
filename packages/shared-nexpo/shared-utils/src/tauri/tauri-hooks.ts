import { useEffect, useRef } from 'react';
import { listen, type UnlistenFn } from '@tauri-apps/api/event';
import { invoke } from '@tauri-apps/api/tauri';
import { TauriEventName, TauriEventPayloadMap } from './tauri-events';

// Type guard to check if we're running in Tauri
function isTauriApp(): boolean {
  return typeof window !== 'undefined' && window.__TAURI__ !== undefined;
}

// Alias for backwards compatibility
export const isDesktopApp = isTauriApp;

// Check if we're running in Tauri
export const isTauri = isTauriApp;

// Type-safe hook for listening to Tauri events
export function useTauriEvent<T extends TauriEventName>(
  eventName: T,
  handler: (payload: TauriEventPayloadMap[T]) => void
) {
  const unlistenRef = useRef<UnlistenFn | null>(null);

  useEffect(() => {
    if (!isTauri()) {
      console.warn(`[useTauriEvent] Not running in Tauri environment, skipping event: ${eventName}`);
      return;
    }

    const setupListener = async () => {
      try {
        unlistenRef.current = await listen<TauriEventPayloadMap[T]>(eventName, (event) => {
          handler(event.payload);
        });
      } catch (error) {
        console.error(`[useTauriEvent] Failed to listen to event ${eventName}:`, error);
      }
    };

    setupListener();

    return () => {
      if (unlistenRef.current) {
        unlistenRef.current();
      }
    };
  }, [eventName]);
}

// Type-safe wrapper for Tauri commands
export async function invokeTauriCommand<T = any>(
  command: string,
  args?: Record<string, any>
): Promise<T> {
  if (!isTauri()) {
    throw new Error(`[invokeTauriCommand] Not running in Tauri environment`);
  }

  try {
    return await invoke<T>(command, args);
  } catch (error) {
    console.error(`[invokeTauriCommand] Command ${command} failed:`, error);
    throw error;
  }
}

// Typed Tauri command functions
export const tauriCommands = {
  greet: (name: string) => 
    invokeTauriCommand<string>('greet', { name }),
  
  authenticate: async (email: string, password: string) => {
    if (!isDesktopApp()) return null;
    return await invoke('authenticate', { email, password });
  },
  
  logout: async () => {
    if (!isDesktopApp()) return;
    return await invoke('logout');
  },
  
  getCurrentUser: async () => {
    if (!isDesktopApp()) return null;
    return await invoke('get_current_user');
  },
  
  processData: async (data: any) => {
    if (!isDesktopApp()) return null;
    return await invoke('process_data', { data });
  },
  
  sendNotification: async (title: string, body: string, notificationType: string = 'info') => {
    if (!isDesktopApp()) return;
    return await invoke('send_notification', { title, body, notificationType });
  },
  
  // Desktop-specific commands
  getSystemInfo: async () => {
    if (!isDesktopApp()) return null;
    return await invoke('get_system_info');
  },
  
  openFileDialog: async () => {
    if (!isDesktopApp()) return null;
    return await invoke('open_file_dialog');
  },
  
  saveFileDialog: async () => {
    if (!isDesktopApp()) return null;
    return await invoke('save_file_dialog');
  },
  
  showNativeNotification: async (title: string, body: string) => {
    if (!isDesktopApp()) return;
    return await invoke('show_native_notification', { title, body });
  },
  
  checkForUpdates: async () => {
    if (!isDesktopApp()) return null;
    return await invoke('check_for_updates');
  },
  
  getAppVersion: async () => {
    if (!isDesktopApp()) return null;
    return await invoke('get_app_version');
  },
};
