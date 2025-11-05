import { useEffect, useState } from 'react';
import { invoke } from '@tauri-apps/api/tauri';
import { listen } from '@tauri-apps/api/event';
import { appWindow } from '@tauri-apps/api/window';
import { isDesktopApp } from '@shared/utils';

export interface DesktopFeatures {
  // System Information
  systemInfo: {
    os: string;
    arch: string;
    version: string;
  } | null;
  
  // Window Management
  windowState: {
    isMaximized: boolean;
    isFullscreen: boolean;
    isMinimized: boolean;
    isVisible: boolean;
  };
  
  // App State
  appVersion: string | null;
  
  // Actions
  openFileDialog: () => Promise<string | null>;
  saveFileDialog: () => Promise<string | null>;
  showNotification: (title: string, body: string) => Promise<void>;
  minimizeWindow: () => Promise<void>;
  maximizeWindow: () => Promise<void>;
  toggleFullscreen: () => Promise<void>;
  closeWindow: () => Promise<void>;
  setAlwaysOnTop: (alwaysOnTop: boolean) => Promise<void>;
  checkForUpdates: () => Promise<any>;
}

export const useDesktopFeatures = (): DesktopFeatures => {
  const [systemInfo, setSystemInfo] = useState<DesktopFeatures['systemInfo']>(null);
  const [windowState, setWindowState] = useState<DesktopFeatures['windowState']>({
    isMaximized: false,
    isFullscreen: false,
    isMinimized: false,
    isVisible: true,
  });
  const [appVersion, setAppVersion] = useState<string | null>(null);

  useEffect(() => {
    if (!isDesktopApp()) return;

    // Load initial system info
    const loadSystemInfo = async () => {
      try {
        const info = await invoke('get_system_info');
        setSystemInfo(info as any);
      } catch (error) {
        console.error('Failed to load system info:', error);
      }
    };

    // Load app version
    const loadAppVersion = async () => {
      try {
        const version = await invoke('get_app_version');
        setAppVersion(version as string);
      } catch (error) {
        console.error('Failed to load app version:', error);
      }
    };

    // Update window state
    const updateWindowState = async () => {
      try {
        const [isMaximized, isFullscreen, isMinimized, isVisible] = await Promise.all([
          appWindow.isMaximized(),
          appWindow.isFullscreen(),
          appWindow.isMinimized(),
          appWindow.isVisible(),
        ]);

        setWindowState({
          isMaximized,
          isFullscreen,
          isMinimized,
          isVisible,
        });
      } catch (error) {
        console.error('Failed to update window state:', error);
      }
    };

    // Initialize
    loadSystemInfo();
    loadAppVersion();
    updateWindowState();

    // Listen for window events
    const unlistenPromises = [
      listen('tauri://resize', updateWindowState),
      listen('tauri://move', updateWindowState),
      listen('tauri://close-requested', updateWindowState),
      listen('tauri://focus', updateWindowState),
      listen('tauri://blur', updateWindowState),
    ];

    // Cleanup function
    return () => {
      Promise.all(unlistenPromises).then(unlisteners => {
        unlisteners.forEach(unlisten => unlisten());
      });
    };
  }, []);

  // Action functions
  const openFileDialog = async (): Promise<string | null> => {
    if (!isDesktopApp()) return null;
    try {
      return await invoke('open_file_dialog') as string;
    } catch (error) {
      console.error('Failed to open file dialog:', error);
      return null;
    }
  };

  const saveFileDialog = async (): Promise<string | null> => {
    if (!isDesktopApp()) return null;
    try {
      return await invoke('save_file_dialog') as string;
    } catch (error) {
      console.error('Failed to open save dialog:', error);
      return null;
    }
  };

  const showNotification = async (title: string, body: string): Promise<void> => {
    if (!isDesktopApp()) return;
    try {
      await invoke('show_native_notification', { title, body });
    } catch (error) {
      console.error('Failed to show notification:', error);
    }
  };

  const minimizeWindow = async (): Promise<void> => {
    if (!isDesktopApp()) return;
    try {
      await appWindow.minimize();
    } catch (error) {
      console.error('Failed to minimize window:', error);
    }
  };

  const maximizeWindow = async (): Promise<void> => {
    if (!isDesktopApp()) return;
    try {
      await appWindow.toggleMaximize();
    } catch (error) {
      console.error('Failed to maximize window:', error);
    }
  };

  const toggleFullscreen = async (): Promise<void> => {
    if (!isDesktopApp()) return;
    try {
      await appWindow.setFullscreen(!windowState.isFullscreen);
    } catch (error) {
      console.error('Failed to toggle fullscreen:', error);
    }
  };

  const closeWindow = async (): Promise<void> => {
    if (!isDesktopApp()) return;
    try {
      await appWindow.close();
    } catch (error) {
      console.error('Failed to close window:', error);
    }
  };

  const setAlwaysOnTop = async (alwaysOnTop: boolean): Promise<void> => {
    if (!isDesktopApp()) return;
    try {
      await appWindow.setAlwaysOnTop(alwaysOnTop);
    } catch (error) {
      console.error('Failed to set always on top:', error);
    }
  };

  const checkForUpdates = async (): Promise<any> => {
    if (!isDesktopApp()) return null;
    try {
      return await invoke('check_for_updates');
    } catch (error) {
      console.error('Failed to check for updates:', error);
      return null;
    }
  };

  return {
    systemInfo,
    windowState,
    appVersion,
    openFileDialog,
    saveFileDialog,
    showNotification,
    minimizeWindow,
    maximizeWindow,
    toggleFullscreen,
    closeWindow,
    setAlwaysOnTop,
    checkForUpdates,
  };
};
