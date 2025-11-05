// Tauri event types and interfaces
export interface TauriUser {
  id: string;
  name: string;
  email: string;
}

export interface TauriProgressPayload {
  progress: number;
  message: string;
}

export interface TauriNotificationPayload {
  title: string;
  body: string;
  notification_type: string;
}

export interface TauriSystemStatus {
  timestamp: string;
  cpu_usage: number;
  memory_usage: number;
}

// Event names as constants for type safety
export const TAURI_EVENTS = {
  // Authentication events
  AUTH_STARTED: 'auth:started',
  AUTH_SUCCESS: 'auth:success',
  AUTH_LOGOUT: 'auth:logout',
  
  // Process events
  PROCESS_STARTED: 'process:started',
  PROCESS_PROGRESS: 'process:progress',
  PROCESS_COMPLETED: 'process:completed',
  
  // Notification events
  NOTIFICATION_SHOW: 'notification:show',
  
  // System events
  SYSTEM_STATUS: 'system:status',
  APP_READY: 'app:ready',
} as const;

export type TauriEventName = typeof TAURI_EVENTS[keyof typeof TAURI_EVENTS];

// Type-safe event payload mapping
export interface TauriEventPayloadMap {
  [TAURI_EVENTS.AUTH_STARTED]: void;
  [TAURI_EVENTS.AUTH_SUCCESS]: TauriUser;
  [TAURI_EVENTS.AUTH_LOGOUT]: void;
  [TAURI_EVENTS.PROCESS_STARTED]: string;
  [TAURI_EVENTS.PROCESS_PROGRESS]: TauriProgressPayload;
  [TAURI_EVENTS.PROCESS_COMPLETED]: string;
  [TAURI_EVENTS.NOTIFICATION_SHOW]: TauriNotificationPayload;
  [TAURI_EVENTS.SYSTEM_STATUS]: TauriSystemStatus;
  [TAURI_EVENTS.APP_READY]: void;
}
