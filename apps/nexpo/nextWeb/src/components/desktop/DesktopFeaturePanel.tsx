import React, { useState } from 'react';
import type { FC } from 'react';
import { useDesktopFeatures } from '@/hooks/useDesktopFeatures';
import { isDesktopApp } from '@shared/utils';
import styles from './DesktopFeaturePanel.module.css';

interface DesktopFeaturePanelProps {
  className?: string;
}

export const DesktopFeaturePanel: FC<DesktopFeaturePanelProps> = ({ className }) => {
  const desktop = useDesktopFeatures();
  const [isExpanded, setIsExpanded] = useState(false);
  const [notificationTitle, setNotificationTitle] = useState('Desktop Notification');
  const [notificationBody, setNotificationBody] = useState('This is a test notification from Nexpo Desktop!');

  if (!isDesktopApp()) {
    return null;
  }

  const handleFileOpen = async () => {
    const filePath = await desktop.openFileDialog();
    if (filePath) {
      await desktop.showNotification('File Selected', `Selected: ${filePath}`);
    }
  };

  const handleFileSave = async () => {
    const filePath = await desktop.saveFileDialog();
    if (filePath) {
      await desktop.showNotification('File Save Location', `Save to: ${filePath}`);
    }
  };

  const handleNotificationTest = async () => {
    await desktop.showNotification(notificationTitle, notificationBody);
  };

  const handleCheckUpdates = async () => {
    const updateInfo = await desktop.checkForUpdates();
    if (updateInfo) {
      await desktop.showNotification('Update Check', 'Update check completed!');
    }
  };

  return (
    <div className={`${styles.featurePanel} ${className || ''}`}>
      <button 
        className={styles.toggleButton}
        onClick={() => setIsExpanded(!isExpanded)}
        aria-expanded={isExpanded}
      >
        <span>🖥️ Desktop Features</span>
        <span className={`${styles.chevron} ${isExpanded ? styles.expanded : ''}`}>
          ▼
        </span>
      </button>
      
      {isExpanded && (
        <div className={styles.panelContent}>
          <div className={styles.featureSection}>
            <h3 className={styles.sectionTitle}>File Operations</h3>
            <div className={styles.buttonGroup}>
              <button 
                className={styles.featureButton}
                onClick={handleFileOpen}
              >
                📁 Open File
              </button>
              <button 
                className={styles.featureButton}
                onClick={handleFileSave}
              >
                💾 Save File
              </button>
            </div>
          </div>

          <div className={styles.featureSection}>
            <h3 className={styles.sectionTitle}>Window Controls</h3>
            <div className={styles.buttonGroup}>
              <button 
                className={styles.featureButton}
                onClick={desktop.minimizeWindow}
              >
                🗕 Minimize
              </button>
              <button 
                className={styles.featureButton}
                onClick={desktop.maximizeWindow}
              >
                🗖 Maximize
              </button>
              <button 
                className={styles.featureButton}
                onClick={desktop.toggleFullscreen}
              >
                🔳 Fullscreen
              </button>
            </div>
          </div>

          <div className={styles.featureSection}>
            <h3 className={styles.sectionTitle}>Notifications</h3>
            <div className={styles.notificationForm}>
              <input
                type="text"
                placeholder="Notification title"
                value={notificationTitle}
                onChange={(e) => setNotificationTitle(e.target.value)}
                className={styles.notificationInput}
              />
              <textarea
                placeholder="Notification body"
                value={notificationBody}
                onChange={(e) => setNotificationBody(e.target.value)}
                className={styles.notificationTextarea}
                rows={3}
              />
              <button 
                className={styles.featureButton}
                onClick={handleNotificationTest}
              >
                🔔 Send Notification
              </button>
            </div>
          </div>

          <div className={styles.featureSection}>
            <h3 className={styles.sectionTitle}>System Information</h3>
            {desktop.systemInfo && (
              <div className={styles.systemInfoGrid}>
                <div className={styles.infoItem}>
                  <span className={styles.infoLabel}>OS:</span>
                  <span className={styles.infoValue}>{desktop.systemInfo.os}</span>
                </div>
                <div className={styles.infoItem}>
                  <span className={styles.infoLabel}>Architecture:</span>
                  <span className={styles.infoValue}>{desktop.systemInfo.arch}</span>
                </div>
                <div className={styles.infoItem}>
                  <span className={styles.infoLabel}>Version:</span>
                  <span className={styles.infoValue}>{desktop.systemInfo.version}</span>
                </div>
              </div>
            )}
          </div>

          <div className={styles.featureSection}>
            <h3 className={styles.sectionTitle}>Updates</h3>
            <button 
              className={styles.featureButton}
              onClick={handleCheckUpdates}
            >
              🔄 Check for Updates
            </button>
          </div>

          <div className={styles.featureSection}>
            <h3 className={styles.sectionTitle}>Window State</h3>
            <div className={styles.windowStateGrid}>
              <div className={`${styles.stateItem} ${desktop.windowState.isMaximized ? styles.active : ''}`}>
                Maximized
              </div>
              <div className={`${styles.stateItem} ${desktop.windowState.isFullscreen ? styles.active : ''}`}>
                Fullscreen
              </div>
              <div className={`${styles.stateItem} ${desktop.windowState.isMinimized ? styles.active : ''}`}>
                Minimized
              </div>
              <div className={`${styles.stateItem} ${desktop.windowState.isVisible ? styles.active : ''}`}>
                Visible
              </div>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};
