import React, { useState, useEffect } from 'react';
// Import from shared packages - adjust these imports based on what's exported
import { Button } from '@nexpo/shared-components';
import { formatDate } from '@nexpo/shared-utils';
import { TestShared } from './TestShared';

const App: React.FC = () => {
  const [platform, setPlatform] = useState<string>('');
  const [sharedData, setSharedData] = useState<any>(null);

  const handleGetSharedData = () => {
    // Example: Call a shared utility function
    // const sharedData = getSharedData();
    // setSharedData(sharedData);
  };

  useEffect(() => {
    // Access Electron APIs through the exposed API
    if (window.electronAPI) {
      setPlatform(window.electronAPI.platform);
      
      // Example: Listen for menu actions
      window.electronAPI.onMenuAction((action) => {
        console.log(`Menu action: ${action}`);
      });
    }
  }, []);

  return (
    <div className="app">
      <header className="app-header">
        <h1>Nexpo Electron App</h1>
        <p>Welcome to your cross-platform desktop application!</p>
        
        <TestShared />
        
        <div style={{ marginTop: '20px' }}>
          <button onClick={handleGetSharedData}>Get Shared Data</button>
          {sharedData && (
            <pre>{JSON.stringify(sharedData, null, 2)}</pre>
          )}
        </div>
        
        {/* Example of using shared components */}
        {/* <Button onClick={() => console.log('Clicked!')}>
          Shared Button Component
        </Button> */}
        
        <div className="versions">
          <p>
            This app is using Node.js {window.electronAPI?.versions.node}, 
            Chromium {window.electronAPI?.versions.chrome}, 
            and Electron {window.electronAPI?.versions.electron}
          </p>
        </div>
      </header>
      
      <main>
        {/* Your app content here, using shared components from packages */}
        <h2>Shared Components Integration</h2>
        <p>
          This Electron app can import and use components from:
        </p>
        <ul>
          <li>@nexpo/shared - Shared business logic and hooks</li>
          <li>@nexpo/ui - Shared UI components</li>
          <li>@nexpo/utils - Shared utilities</li>
        </ul>
      </main>
    </div>
  );
};

export default App;
