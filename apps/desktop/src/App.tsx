import { useState, useEffect } from 'react';
import { invoke } from '@tauri-apps/api/tauri';
import { TauriDemo } from '@shared/components';
import { useTauriEvent, tauriCommands, TAURI_EVENTS } from '@shared/utils/tauri';
import { appWindow } from '@tauri-apps/api/window';
import { isDesktopApp } from '@shared/utils/tauri';
import './styles/App.css';

interface SystemInfo {
  os: string;
  arch: string;
  version: string;
}

export function App() {
  const [systemInfo, setSystemInfo] = useState<SystemInfo | null>(null);
  const [isMaximized, setIsMaximized] = useState(false);
  const [loading, setLoading] = useState(true);

  // Listen for system status updates
  useTauriEvent(TAURI_EVENTS.SYSTEM_STATUS, (status: any) => {
    console.log('System Status:', status);
  });

  // Listen for app ready event
  useTauriEvent(TAURI_EVENTS.APP_READY, () => {
    setLoading(false);
  });

  useEffect(() => {
    if (isDesktopApp()) {
      // Get system information
      invoke('get_system_info').then((info: unknown) => {
        setSystemInfo(info as SystemInfo);
      });

      // Check if window is maximized
      appWindow.isMaximized().then(setIsMaximized);

      // Listen for window resize events
      const unlisten = appWindow.onResized(() => {
        appWindow.isMaximized().then(setIsMaximized);
      });

      return () => {
        unlisten.then(f => f());
      };
    }
  }, []);

  const handleMinimize = () => {
    appWindow.minimize();
  };

  const handleMaximize = () => {
    appWindow.toggleMaximize();
  };

  const handleClose = () => {
    appWindow.close();
  };

  if (loading) {
    return (
      <div className="loading-container">
        <div className="loading-spinner"></div>
        <p>Loading Nexpo Desktop...</p>
      </div>
    );
  }

  return (
    <div className="app">
      {/* Custom title bar for desktop */}
      <div className="titlebar" data-tauri-drag-region>
        <div className="titlebar-left">
          <h1>Nexpo Desktop</h1>
        </div>
        <div className="titlebar-right">
          <button
            className="titlebar-button"
            onClick={handleMinimize}
            title="Minimize"
          >
            &#8212;
          </button>
          <button
            className="titlebar-button"
            onClick={handleMaximize}
            title={isMaximized ? 'Restore' : 'Maximize'}
          >
            {isMaximized ? '❐' : '⬜'}
          </button>
          <button
            className="titlebar-button close"
            onClick={handleClose}
            title="Close"
          >
            ✕
          </button>
        </div>
      </div>

      {/* Main content */}
      <div className="content">
        <div className="desktop-header">
          <h2>Desktop Application Features</h2>
          {systemInfo && (
            <div className="system-info">
              <p><strong>OS:</strong> {systemInfo.os}</p>
              <p><strong>Architecture:</strong> {systemInfo.arch}</p>
              <p><strong>Version:</strong> {systemInfo.version}</p>
            </div>
          )}
        </div>

        <div className="features-grid">
          <div className="feature-card">
            <h3>Native File System</h3>
            <p>Access local files and directories with native OS dialogs</p>
            <button 
              className="feature-button"
              onClick={async () => {
                const result = await tauriCommands.openFileDialog();
                if (result) {
                  console.log('Selected file:', result);
                }
              }}
            >
              Open File Dialog
            </button>
          </div>

          <div className="feature-card">
            <h3>System Notifications</h3>
            <p>Send native system notifications</p>
            <button 
              className="feature-button"
              onClick={() => tauriCommands.sendNotification('Desktop Feature', 'This is a native notification!')}
            >
              Send Notification
            </button>
          </div>

          <div className="feature-card">
            <h3>System Tray</h3>
            <p>Run in background with system tray integration</p>
            <button 
              className="feature-button"
              onClick={() => appWindow.hide()}
            >
              Hide to Tray
            </button>
          </div>

          <div className="feature-card">
            <h3>Auto Updates</h3>
            <p>Automatic application updates</p>
            <button 
              className="feature-button"
              onClick={() => tauriCommands.checkForUpdates()}
            >
              Check for Updates
            </button>
          </div>
        </div>

        <div className="demo-section">
          <h3>Tauri Event System Demo</h3>
          <TauriDemo />
        </div>
      </div>
    </div>
  );
}
