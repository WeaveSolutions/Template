import React, { useEffect, useState } from 'react';
import type { FC, ReactNode } from 'react';
import { useDesktopFeatures } from '@/hooks/useDesktopFeatures';
import { isDesktopApp } from '@shared/utils';
import styles from './DesktopLayout.module.css';

interface DesktopLayoutProps {
  children: ReactNode;
  showTitleBar?: boolean;
  showSystemInfo?: boolean;
}

export const DesktopLayout: FC<DesktopLayoutProps> = ({ 
  children, 
  showTitleBar = true, 
  showSystemInfo = false 
}) => {
  const desktop = useDesktopFeatures();
  const [isDesktop, setIsDesktop] = useState(false);

  useEffect(() => {
    setIsDesktop(isDesktopApp());
  }, []);

  if (!isDesktop) {
    return <>{children}</>;
  }

  return (
    <div className={styles.desktopLayout}>
      {showTitleBar && (
        <div className={styles.titleBar} data-tauri-drag-region>
          <div className={styles.titleBarContent}>
            <div className={styles.titleBarLeft}>
              <span className={styles.appTitle}>Nexpo Desktop</span>
              {desktop.appVersion && (
                <span className={styles.appVersion}>v{desktop.appVersion}</span>
              )}
            </div>
            
            <div className={styles.titleBarRight}>
              <button 
                className={styles.titleBarButton}
                onClick={desktop.minimizeWindow}
                aria-label="Minimize"
              >
                <svg width="12" height="12" viewBox="0 0 12 12">
                  <path d="M2 6h8" stroke="currentColor" strokeWidth="2"/>
                </svg>
              </button>
              
              <button 
                className={styles.titleBarButton}
                onClick={desktop.maximizeWindow}
                aria-label="Maximize"
              >
                <svg width="12" height="12" viewBox="0 0 12 12">
                  <rect x="2" y="2" width="8" height="8" fill="none" stroke="currentColor" strokeWidth="2"/>
                </svg>
              </button>
              
              <button 
                className={`${styles.titleBarButton} ${styles.closeButton}`}
                onClick={desktop.closeWindow}
                aria-label="Close"
              >
                <svg width="12" height="12" viewBox="0 0 12 12">
                  <path d="M2 2l8 8M2 10l8-8" stroke="currentColor" strokeWidth="2"/>
                </svg>
              </button>
            </div>
          </div>
        </div>
      )}
      
      <div className={styles.content}>
        {children}
      </div>
      
      {showSystemInfo && desktop.systemInfo && (
        <div className={styles.systemInfo}>
          <div className={styles.systemInfoContent}>
            <span>
              {desktop.systemInfo.os} {desktop.systemInfo.arch} | 
              {desktop.systemInfo.version}
            </span>
            <div className={styles.windowControls}>
              <button 
                className={styles.controlButton}
                onClick={() => desktop.toggleFullscreen()}
                title="Toggle Fullscreen"
              >
                📱
              </button>
              <button 
                className={styles.controlButton}
                onClick={() => desktop.setAlwaysOnTop(!desktop.windowState.isMaximized)}
                title="Always on Top"
              >
                📌
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};
