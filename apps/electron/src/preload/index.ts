import { contextBridge, ipcRenderer } from 'electron';

// Expose protected methods that allow the renderer process to use
// the ipcRenderer without exposing the entire object
contextBridge.exposeInMainWorld('electronAPI', {
  // Store API
  store: {
    get: (key: string) => ipcRenderer.invoke('get-store-value', key),
    set: (key: string, value: any) => ipcRenderer.invoke('set-store-value', key, value),
  },
  
  // Platform API
  platform: process.platform,
  
  // Menu events
  onMenuAction: (callback: (action: string) => void) => {
    const validChannels = ['menu-new', 'menu-open', 'menu-save'];
    validChannels.forEach(channel => {
      ipcRenderer.on(channel, () => callback(channel.replace('menu-', '')));
    });
  },
  
  // Version
  versions: {
    node: process.versions.node,
    chrome: process.versions.chrome,
    electron: process.versions.electron,
  },
});

// TypeScript declarations for the exposed API
declare global {
  interface Window {
    electronAPI: {
      store: {
        get: (key: string) => Promise<any>;
        set: (key: string, value: any) => Promise<void>;
      };
      platform: NodeJS.Platform;
      onMenuAction: (callback: (action: string) => void) => void;
      versions: {
        node: string;
        chrome: string;
        electron: string;
      };
    };
  }
}
