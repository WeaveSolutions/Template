<script lang="ts">
  import { onMount } from "svelte";
  import { invoke } from "@tauri-apps/api/core";
  import { listen } from "@tauri-apps/api/event";
  import { info } from "@tauri-apps/plugin-dialog";
  import { authStore, settingsStore } from "@shared/tauri";
  import { AuthPanel, UserProfile, SettingsPanel, NotificationPanel } from "@shared/tauri";
  import type { User } from "@shared/tauri";

  let appInfo: { name: string; version: string; platform: string } | null = null;
  let currentUser: User | null = null;
  let isAuthenticated = false;
  let activeTab = "auth";

  // Subscribe to auth store
  authStore.subscribe((auth) => {
    currentUser = auth.user;
    isAuthenticated = auth.isAuthenticated;
    if (isAuthenticated) {
      activeTab = "profile";
    }
  });

  onMount(async () => {
    try {
      // Get app information
      appInfo = await invoke<{ name: string; version: string; platform: string }>("get_app_info");
      
      // Listen for auth changes
      await listen("auth-changed", (event) => {
        if (event.payload) {
          authStore.setUser(event.payload as User);
        } else {
          authStore.logout();
        }
      });

      // Listen for settings changes
      await listen("settings-changed", (event) => {
        console.log("Settings changed:", event.payload);
      });

      // Listen for background updates
      await listen("background-update", (event) => {
        console.log("Background update:", event.payload);
      });

      // Load current user profile if authenticated
      try {
        const response = await invoke<{ success: boolean; data?: User; error?: string }>("get_user_profile");
        if (response.success && response.data) {
          authStore.setUser(response.data);
        }
      } catch (error) {
        console.log("No user currently authenticated");
      }

      // Load settings
      try {
        const settings = await invoke<Record<string, string>>("get_settings");
        settingsStore.updateSettings(settings);
      } catch (error) {
        console.error("Failed to load settings:", error);
      }

    } catch (error) {
      console.error("Failed to initialize app:", error);
      await info("Failed to initialize app", { title: "Error", kind: "error" });
    }
  });

  const showAbout = async () => {
    if (appInfo) {
      await info(
        `${appInfo.name} v${appInfo.version}\n\nPlatform: ${appInfo.platform}\n\nBuilt with Tauri and Svelte`,
        { title: "About Nexpo Mobile", kind: "info" }
      );
    }
  };

  const setActiveTab = (tab: string) => {
    activeTab = tab;
  };
</script>

<main>
  <header>
    <h1>
      {#if appInfo}
        {appInfo.name}
      {:else}
        Nexpo Mobile
      {/if}
    </h1>
    <button class="about-btn" on:click={showAbout}>About</button>
  </header>

  <nav class="tab-nav">
    <button 
      class="tab-btn"
      class:active={activeTab === "auth"}
      on:click={() => setActiveTab("auth")}
      disabled={isAuthenticated}
    >
      Authentication
    </button>
    <button 
      class="tab-btn"
      class:active={activeTab === "profile"}
      on:click={() => setActiveTab("profile")}
      disabled={!isAuthenticated}
    >
      Profile
    </button>
    <button 
      class="tab-btn"
      class:active={activeTab === "settings"}
      on:click={() => setActiveTab("settings")}
    >
      Settings
    </button>
    <button 
      class="tab-btn"
      class:active={activeTab === "notifications"}
      on:click={() => setActiveTab("notifications")}
    >
      Notifications
    </button>
  </nav>

  <div class="content">
    {#if activeTab === "auth"}
      <AuthPanel />
    {:else if activeTab === "profile"}
      <UserProfile />
    {:else if activeTab === "settings"}
      <SettingsPanel />
    {:else if activeTab === "notifications"}
      <NotificationPanel />
    {/if}
  </div>

  <footer>
    <p>
      Platform: <span class="platform-tag">{appInfo?.platform || "loading..."}</span>
    </p>
    <p>
      Status: <span class="status-tag" class:authenticated={isAuthenticated}>
        {isAuthenticated ? "Authenticated" : "Not Authenticated"}
      </span>
    </p>
  </footer>
</main>

<style>
  :global(body) {
    margin: 0;
    padding: 0;
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: #333;
    min-height: 100vh;
  }

  main {
    max-width: 400px;
    margin: 0 auto;
    padding: 20px;
    min-height: 100vh;
    display: flex;
    flex-direction: column;
  }

  header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 30px;
    padding: 20px;
    background: rgba(255, 255, 255, 0.1);
    border-radius: 15px;
    backdrop-filter: blur(10px);
  }

  h1 {
    color: white;
    font-size: 1.8rem;
    margin: 0;
    font-weight: 600;
  }

  .about-btn {
    background: rgba(255, 255, 255, 0.2);
    color: white;
    border: none;
    padding: 8px 16px;
    border-radius: 20px;
    cursor: pointer;
    font-size: 0.9rem;
    transition: background 0.3s ease;
  }

  .about-btn:hover {
    background: rgba(255, 255, 255, 0.3);
  }

  .tab-nav {
    display: flex;
    gap: 5px;
    margin-bottom: 20px;
    background: rgba(255, 255, 255, 0.1);
    padding: 5px;
    border-radius: 25px;
    backdrop-filter: blur(10px);
  }

  .tab-btn {
    flex: 1;
    padding: 12px 16px;
    border: none;
    border-radius: 20px;
    background: transparent;
    color: rgba(255, 255, 255, 0.7);
    cursor: pointer;
    transition: all 0.3s ease;
    font-size: 0.9rem;
    font-weight: 500;
  }

  .tab-btn:hover {
    color: white;
    background: rgba(255, 255, 255, 0.1);
  }

  .tab-btn.active {
    background: rgba(255, 255, 255, 0.9);
    color: #333;
    font-weight: 600;
  }

  .tab-btn:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  .content {
    flex: 1;
    background: rgba(255, 255, 255, 0.95);
    border-radius: 15px;
    padding: 25px;
    margin-bottom: 20px;
    box-shadow: 0 10px 30px rgba(0, 0, 0, 0.1);
  }

  footer {
    text-align: center;
    padding: 15px;
    background: rgba(255, 255, 255, 0.1);
    border-radius: 15px;
    backdrop-filter: blur(10px);
  }

  footer p {
    margin: 5px 0;
    color: rgba(255, 255, 255, 0.9);
    font-size: 0.9rem;
  }

  .platform-tag, .status-tag {
    font-weight: 600;
    padding: 4px 8px;
    border-radius: 12px;
    background: rgba(255, 255, 255, 0.2);
    color: white;
  }

  .status-tag.authenticated {
    background: #4CAF50;
    color: white;
  }
</style>
