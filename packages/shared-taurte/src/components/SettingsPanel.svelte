<script lang="ts">
  import { invoke } from "@tauri-apps/api/core";
  import { settingsStore, type Settings } from "../stores/settingsStore";
  
  let settings: Settings;
  let isSaving = false;
  let saveMessage = "";

  // Subscribe to settings store
  settingsStore.subscribe((currentSettings) => {
    settings = currentSettings;
  });

  const handleSettingChange = async (key: keyof Settings, value: any) => {
    settingsStore.updateSetting(key, value);
    await saveSetting(key, value);
  };

  const saveSetting = async (key: string, value: any) => {
    try {
      await invoke("update_setting", { 
        key, 
        value: typeof value === "boolean" ? value.toString() : value 
      });
      showSaveMessage("Setting saved successfully", "success");
    } catch (error) {
      console.error("Failed to save setting:", error);
      showSaveMessage("Failed to save setting", "error");
    }
  };

  const showSaveMessage = (message: string, type: "success" | "error") => {
    saveMessage = message;
    setTimeout(() => {
      saveMessage = "";
    }, 3000);
  };

  const resetSettings = async () => {
    if (confirm("Are you sure you want to reset all settings to defaults?")) {
      settingsStore.reset();
      
      // Save each default setting
      const defaultSettings = {
        theme: "auto",
        notifications: "true",
        language: "en",
        fontSize: "medium",
        autoSync: "true"
      };

      for (const [key, value] of Object.entries(defaultSettings)) {
        await saveSetting(key, value);
      }
      
      showSaveMessage("Settings reset to defaults", "success");
    }
  };
</script>

<div class="settings-panel">
  <h2>Application Settings</h2>
  <p class="subtitle">Customize your Nexpo experience</p>

  {#if saveMessage}
    <div class="save-message success">
      <span class="icon">✓</span>
      {saveMessage}
    </div>
  {/if}

  <div class="settings-sections">
    <!-- Appearance Settings -->
    <div class="settings-section">
      <h3>Appearance</h3>
      
      <div class="setting-item">
        <div class="setting-info">
          <label>Theme</label>
          <span class="description">Choose your preferred color scheme</span>
        </div>
        <select 
          bind:value={settings.theme} 
          on:change={(e) => handleSettingChange('theme', e.target.value)}
        >
          <option value="light">Light</option>
          <option value="dark">Dark</option>
          <option value="auto">System</option>
        </select>
      </div>

      <div class="setting-item">
        <div class="setting-info">
          <label>Font Size</label>
          <span class="description">Adjust text size for better readability</span>
        </div>
        <select 
          bind:value={settings.fontSize} 
          on:change={(e) => handleSettingChange('fontSize', e.target.value)}
        >
          <option value="small">Small</option>
          <option value="medium">Medium</option>
          <option value="large">Large</option>
        </select>
      </div>
    </div>

    <!-- Notification Settings -->
    <div class="settings-section">
      <h3>Notifications</h3>
      
      <div class="setting-item">
        <div class="setting-info">
          <label>Enable Notifications</label>
          <span class="description">Receive push notifications for updates</span>
        </div>
        <label class="switch">
          <input 
            type="checkbox" 
            bind:checked={settings.notifications}
            on:change={(e) => handleSettingChange('notifications', e.target.checked)}
          />
          <span class="slider"></span>
        </label>
      </div>
    </div>

    <!-- General Settings -->
    <div class="settings-section">
      <h3>General</h3>
      
      <div class="setting-item">
        <div class="setting-info">
          <label>Language</label>
          <span class="description">Select your preferred language</span>
        </div>
        <select 
          bind:value={settings.language} 
          on:change={(e) => handleSettingChange('language', e.target.value)}
        >
          <option value="en">English</option>
          <option value="es">Español</option>
          <option value="fr">Français</option>
          <option value="de">Deutsch</option>
        </select>
      </div>

      <div class="setting-item">
        <div class="setting-info">
          <label>Auto Sync</label>
          <span class="description">Automatically sync data in background</span>
        </div>
        <label class="switch">
          <input 
            type="checkbox" 
            bind:checked={settings.autoSync}
            on:change={(e) => handleSettingChange('autoSync', e.target.checked)}
          />
          <span class="slider"></span>
        </label>
      </div>
    </div>
  </div>

  <div class="actions">
    <button class="reset-btn" on:click={resetSettings}>
      Reset to Defaults
    </button>
  </div>
</div>

<style>
  .settings-panel h2 {
    color: #333;
    margin-bottom: 8px;
    font-size: 1.8rem;
    font-weight: 600;
  }

  .subtitle {
    color: #666;
    margin-bottom: 2rem;
    font-size: 0.9rem;
  }

  .save-message {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.75rem;
    margin-bottom: 1.5rem;
    border-radius: 6px;
    font-size: 0.875rem;
  }

  .save-message.success {
    background-color: #d1e7dd;
    color: #0f5132;
    border: 1px solid #badbcc;
  }

  .save-message .icon {
    font-weight: bold;
  }

  .settings-sections {
    display: flex;
    flex-direction: column;
    gap: 2rem;
  }

  .settings-section {
    background: #f8f9fa;
    border-radius: 8px;
    padding: 1.5rem;
  }

  .settings-section h3 {
    color: #495057;
    font-size: 1.1rem;
    font-weight: 600;
    margin-bottom: 1rem;
    border-bottom: 2px solid #dee2e6;
    padding-bottom: 0.5rem;
  }

  .setting-item {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 1rem 0;
    border-bottom: 1px solid #e9ecef;
  }

  .setting-item:last-child {
    border-bottom: none;
  }

  .setting-info {
    display: flex;
    flex-direction: column;
    flex: 1;
  }

  .setting-info label {
    color: #343a40;
    font-weight: 500;
    font-size: 0.95rem;
    margin-bottom: 0.25rem;
  }

  .setting-info .description {
    color: #6c757d;
    font-size: 0.8rem;
  }

  select {
    padding: 0.5rem;
    border: 1px solid #ced4da;
    border-radius: 4px;
    background-color: white;
    font-size: 0.9rem;
    min-width: 120px;
  }

  select:focus {
    outline: none;
    border-color: #86b7fe;
    box-shadow: 0 0 0 0.25rem rgba(13, 110, 253, 0.25);
  }

  .switch {
    position: relative;
    display: inline-block;
    width: 50px;
    height: 24px;
  }

  .switch input {
    opacity: 0;
    width: 0;
    height: 0;
  }

  .slider {
    position: absolute;
    cursor: pointer;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background-color: #ccc;
    transition: .4s;
    border-radius: 24px;
  }

  .slider:before {
    position: absolute;
    content: "";
    height: 18px;
    width: 18px;
    left: 3px;
    bottom: 3px;
    background-color: white;
    transition: .4s;
    border-radius: 50%;
  }

  input:checked + .slider {
    background-color: #28a745;
  }

  input:checked + .slider:before {
    transform: translateX(26px);
  }

  .actions {
    margin-top: 2rem;
    text-align: center;
  }

  .reset-btn {
    padding: 0.75rem 1.5rem;
    background-color: #dc3545;
    color: white;
    border: none;
    border-radius: 6px;
    font-size: 0.9rem;
    font-weight: 500;
    cursor: pointer;
    transition: background-color 0.2s ease;
  }

  .reset-btn:hover {
    background-color: #c82333;
  }

  /* Dark mode support */
  :global(.dark-mode) .settings-panel {
    color: white;
  }

  :global(.dark-mode) .settings-panel h2 {
    color: white;
  }

  :global(.dark-mode) .settings-section {
    background-color: #4a5568;
  }

  :global(.dark-mode) .settings-section h3 {
    color: #cbd5e0;
    border-bottom-color: #718096;
  }

  :global(.dark-mode) .setting-item {
    border-bottom-color: #718096;
  }

  :global(.dark-mode) .setting-info label {
    color: white;
  }

  :global(.dark-mode) .setting-info .description {
    color: #a0aec0;
  }

  :global(.dark-mode) select {
    background-color: #2d3748;
    border-color: #4a5568;
    color: white;
  }
</style>
