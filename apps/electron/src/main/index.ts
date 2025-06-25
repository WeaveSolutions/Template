import { app, BrowserWindow, Menu, ipcMain, dialog, shell } from 'electron';
import type { MenuItemConstructorOptions, IpcMainInvokeEvent } from 'electron';
import path from 'path';
import { autoUpdater } from 'electron-updater';
import Store from 'electron-store';

// Import shared packages through IPC (main process doesn't directly import React components)
// Shared utilities can be imported if they're Node-compatible
// import { someUtility } from '@nexpo/shared-utils';

// Handle creating/removing shortcuts on Windows when installing/uninstalling.
if (require('electron-squirrel-startup')) {
  app.quit();
}

declare const MAIN_WINDOW_WEBPACK_ENTRY: string;
declare const MAIN_WINDOW_PRELOAD_WEBPACK_ENTRY: string;

const store = new Store();
let mainWindow: BrowserWindow | null = null;

const createWindow = (): void => {
  // Create the browser window.
  mainWindow = new BrowserWindow({
    height: 768,
    width: 1366,
    minHeight: 600,
    minWidth: 800,
    webPreferences: {
      preload: MAIN_WINDOW_PRELOAD_WEBPACK_ENTRY,
      contextIsolation: true,
      nodeIntegration: false,
    },
    titleBarStyle: process.platform === 'darwin' ? 'hiddenInset' : 'default',
    frame: process.platform !== 'darwin',
    icon: path.join(__dirname, '../../assets/icon.png'),
  });

  // Load the index.html of the app.
  mainWindow.loadURL(MAIN_WINDOW_WEBPACK_ENTRY);

  // Open external links in the default browser
  mainWindow.webContents.setWindowOpenHandler(({ url }: { url: string }) => {
    dialog.showMessageBox({ message: 'Opening external link', detail: url });
    shell.openExternal(url);
    return { action: 'deny' };
  });

  mainWindow.on('closed', () => {
    mainWindow = null;
  });

  // Create application menu
  createApplicationMenu();

  // Check for updates
  autoUpdater.checkForUpdatesAndNotify();
};

const createApplicationMenu = (): void => {
  const menuTemplate: MenuItemConstructorOptions[] = [
    {
      label: 'File',
      submenu: [
        {
          label: 'New',
          accelerator: 'CmdOrCtrl+N',
          click: () => {
            mainWindow?.webContents.send('menu-new');
          },
        },
        {
          label: 'Open',
          accelerator: 'CmdOrCtrl+O',
          click: () => {
            mainWindow?.webContents.send('menu-open');
          },
        },
        {
          label: 'Save',
          accelerator: 'CmdOrCtrl+S',
          click: () => {
            mainWindow?.webContents.send('menu-save');
          },
        },
        { type: 'separator' },
        {
          label: 'Quit',
          accelerator: process.platform === 'darwin' ? 'Cmd+Q' : 'Ctrl+Q',
          click: () => {
            app.quit();
          },
        },
      ],
    },
    {
      label: 'Edit',
      submenu: [
        { label: 'Undo', accelerator: 'CmdOrCtrl+Z', role: 'undo' },
        { label: 'Redo', accelerator: 'Shift+CmdOrCtrl+Z', role: 'redo' },
        { type: 'separator' },
        { label: 'Cut', accelerator: 'CmdOrCtrl+X', role: 'cut' },
        { label: 'Copy', accelerator: 'CmdOrCtrl+C', role: 'copy' },
        { label: 'Paste', accelerator: 'CmdOrCtrl+V', role: 'paste' },
      ],
    },
    {
      label: 'View',
      submenu: [
        { label: 'Reload', accelerator: 'CmdOrCtrl+R', role: 'reload' },
        { label: 'Force Reload', accelerator: 'CmdOrCtrl+Shift+R', role: 'forceReload' },
        { label: 'Toggle Developer Tools', accelerator: 'F12', role: 'toggleDevTools' },
        { type: 'separator' },
        { label: 'Actual Size', accelerator: 'CmdOrCtrl+0', role: 'resetZoom' },
        { label: 'Zoom In', accelerator: 'CmdOrCtrl+Plus', role: 'zoomIn' },
        { label: 'Zoom Out', accelerator: 'CmdOrCtrl+-', role: 'zoomOut' },
        { type: 'separator' },
        { label: 'Toggle Fullscreen', accelerator: 'F11', role: 'togglefullscreen' },
      ],
    },
    {
      label: 'Help',
      submenu: [
        {
          label: 'Learn More',
          click: async () => {
            await shell.openExternal('https://nexpo.io');
          },
        },
      ],
    },
  ];

  if (process.platform === 'darwin') {
    menuTemplate.unshift({
      label: app.getName(),
      submenu: [
        { label: 'About ' + app.getName(), role: 'about' },
        { type: 'separator' },
        { label: 'Services', role: 'services', submenu: [] },
        { type: 'separator' },
        { label: 'Hide ' + app.getName(), accelerator: 'Command+H', role: 'hide' },
        { label: 'Hide Others', accelerator: 'Command+Shift+H', role: 'hideOthers' },
        { label: 'Show All', role: 'unhide' },
        { type: 'separator' },
        { label: 'Quit', accelerator: 'Command+Q', click: () => app.quit() },
      ],
    });
  }

  const menu = Menu.buildFromTemplate(menuTemplate);
  Menu.setApplicationMenu(menu);
};

// IPC handlers for shared packages integration
ipcMain.handle('get-store-value', (_event: IpcMainInvokeEvent, key: string) => {
  return store.get(key);
});

ipcMain.handle('set-store-value', (_event: IpcMainInvokeEvent, key: string, value: any) => {
  store.set(key, value);
});

ipcMain.handle('get-app-version', () => app.getVersion());

ipcMain.handle('get-shared-data', async (_: any) => {
  // Example of returning shared data or functionality
  return { message: 'Data from main process', timestamp: Date.now() };
});

ipcMain.handle('show-message-box', async (_: any, options: any) => {
  return dialog.showMessageBox(mainWindow!, options);
});

// This method will be called when Electron has finished initialization
app.on('ready', createWindow);

// Quit when all windows are closed, except on macOS
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') {
    app.quit();
  }
});

app.on('activate', () => {
  if (BrowserWindow.getAllWindows().length === 0) {
    createWindow();
  }
});

// Auto-updater events
autoUpdater.on('update-downloaded', () => {
  autoUpdater.quitAndInstall();
});
