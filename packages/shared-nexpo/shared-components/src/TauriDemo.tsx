import { useState, useEffect } from 'react';
import type { FC } from 'react';
import { 
  useTauriEvent, 
  tauriCommands, 
  isTauri,
  TAURI_EVENTS,
  TauriUser,
  TauriProgressPayload,
  TauriSystemStatus
} from '@nexpo/shared-utils/tauri';

export const TauriDemo: FC = () => {
  const [user, setUser] = useState<TauriUser | null>(null);
  const [progress, setProgress] = useState<TauriProgressPayload | null>(null);
  const [systemStatus, setSystemStatus] = useState<TauriSystemStatus | null>(null);
  const [message, setMessage] = useState('');
  const [isProcessing, setIsProcessing] = useState(false);

  // Check if running in Tauri
  const isInTauri = isTauri();

  // Listen to authentication events
  useTauriEvent(TAURI_EVENTS.AUTH_SUCCESS, (user: TauriUser) => {
    setUser(user);
    setMessage(`Welcome, ${user.name}!`);
  });

  useTauriEvent(TAURI_EVENTS.AUTH_LOGOUT, () => {
    setUser(null);
    setMessage('Logged out successfully');
  });

  // Listen to process events
  useTauriEvent(TAURI_EVENTS.PROCESS_STARTED, (data: string) => {
    setIsProcessing(true);
    setMessage(`Processing: ${data}`);
  });

  useTauriEvent(TAURI_EVENTS.PROCESS_PROGRESS, (payload: TauriProgressPayload) => {
    setProgress(payload);
  });

  useTauriEvent(TAURI_EVENTS.PROCESS_COMPLETED, (data: string) => {
    setIsProcessing(false);
    setProgress(null);
    setMessage(`Completed: ${data}`);
  });

  // Listen to system status updates
  useTauriEvent(TAURI_EVENTS.SYSTEM_STATUS, (status: TauriSystemStatus) => {
    setSystemStatus(status);
  });

  // Check current user on mount
  useEffect(() => {
    if (isInTauri) {
      tauriCommands.getCurrentUser()
        .then((currentUser: TauriUser | null) => {
          if (currentUser) {
            setUser(currentUser);
          }
        })
        .catch(console.error);
    }
  }, [isInTauri]);

  const handleLogin = async () => {
    try {
      const user = await tauriCommands.authenticate('user@example.com', 'password');
      // User state will be updated via AUTH_SUCCESS event
    } catch (error) {
      setMessage(`Login failed: ${error}`);
    }
  };

  const handleLogout = async () => {
    try {
      await tauriCommands.logout();
      // User state will be cleared via AUTH_LOGOUT event
    } catch (error) {
      setMessage(`Logout failed: ${error}`);
    }
  };

  const handleProcess = async () => {
    try {
      const result = await tauriCommands.processData('Sample data for processing');
      // Progress and completion will be handled via events
    } catch (error) {
      setMessage(`Process failed: ${error}`);
    }
  };

  const handleNotification = async () => {
    try {
      await tauriCommands.sendNotification(
        'Tauri Notification',
        'This is a test notification from Tauri!',
        'info'
      );
    } catch (error) {
      setMessage(`Notification failed: ${error}`);
    }
  };

  if (!isInTauri) {
    return (
      <div className="p-4 border rounded bg-yellow-50">
        <p className="text-yellow-800">
          This component requires Tauri desktop environment.
          It will work when running inside the Tauri app.
        </p>
      </div>
    );
  }

  return (
    <div className="p-6 max-w-4xl mx-auto">
      <h2 className="text-2xl font-bold mb-6">Tauri Event System Demo</h2>
      
      {/* User Status */}
      <div className="mb-6 p-4 border rounded">
        <h3 className="font-semibold mb-2">User Status</h3>
        {user ? (
          <div>
            <p>Logged in as: {user.name} ({user.email})</p>
            <button
              onClick={handleLogout}
              className="mt-2 px-4 py-2 bg-red-500 text-white rounded hover:bg-red-600"
            >
              Logout
            </button>
          </div>
        ) : (
          <button
            onClick={handleLogin}
            className="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
          >
            Login
          </button>
        )}
      </div>

      {/* Process Demo */}
      <div className="mb-6 p-4 border rounded">
        <h3 className="font-semibold mb-2">Long-Running Process</h3>
        <button
          onClick={handleProcess}
          disabled={isProcessing}
          className="px-4 py-2 bg-green-500 text-white rounded hover:bg-green-600 disabled:opacity-50"
        >
          {isProcessing ? 'Processing...' : 'Start Process'}
        </button>
        
        {progress && (
          <div className="mt-4">
            <div className="w-full bg-gray-200 rounded-full h-4">
              <div
                className="bg-blue-600 h-4 rounded-full transition-all duration-300"
                style={{ width: `${progress.progress * 100}%` }}
              />
            </div>
            <p className="mt-2 text-sm text-gray-600">{progress.message}</p>
          </div>
        )}
      </div>

      {/* System Status */}
      <div className="mb-6 p-4 border rounded">
        <h3 className="font-semibold mb-2">System Status (Live)</h3>
        {systemStatus ? (
          <div className="space-y-2">
            <p className="text-sm">
              <span className="font-medium">CPU:</span> {systemStatus.cpu_usage.toFixed(2)}%
            </p>
            <p className="text-sm">
              <span className="font-medium">Memory:</span> {systemStatus.memory_usage.toFixed(2)}%
            </p>
            <p className="text-xs text-gray-500">
              Updated: {new Date(systemStatus.timestamp).toLocaleTimeString()}
            </p>
          </div>
        ) : (
          <p className="text-gray-500">Waiting for system status...</p>
        )}
      </div>

      {/* Notifications */}
      <div className="mb-6 p-4 border rounded">
        <h3 className="font-semibold mb-2">Notifications</h3>
        <button
          onClick={handleNotification}
          className="px-4 py-2 bg-purple-500 text-white rounded hover:bg-purple-600"
        >
          Send Notification
        </button>
      </div>

      {/* Messages */}
      {message && (
        <div className="mt-4 p-4 bg-gray-100 rounded">
          <p className="text-sm">{message}</p>
        </div>
      )}
    </div>
  );
};
