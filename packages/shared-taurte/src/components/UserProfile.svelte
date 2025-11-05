<script lang="ts">
  import { invoke } from "@tauri-apps/api/core";
  import { authStore, type User } from "../stores/authStore";

  let currentUser: User | null = null;
  let isLoggingOut = false;

  // Subscribe to auth store
  authStore.subscribe((auth) => {
    currentUser = auth.user;
  });

  const handleLogout = async () => {
    isLoggingOut = true;
    
    try {
      await invoke("logout_user");
      authStore.clearUser();
    } catch (error) {
      console.error("Logout error:", error);
    } finally {
      isLoggingOut = false;
    }
  };

  const getInitials = (name: string): string => {
    return name
      .split(" ")
      .map(word => word.charAt(0))
      .join("")
      .toUpperCase()
      .slice(0, 2);
  };

  const formatUserId = (id: string): string => {
    return id.length > 8 ? `${id.slice(0, 8)}...` : id;
  };
</script>

<div class="profile-panel">
  {#if currentUser}
    <div class="user-header">
      <div class="avatar">
        {#if currentUser.avatar_url}
          <img src={currentUser.avatar_url} alt="User Avatar" />
        {:else}
          <div class="initials">{getInitials(currentUser.name)}</div>
        {/if}
      </div>
      <div class="user-info">
        <h2 class="name">{currentUser.name}</h2>
        <p class="email">{currentUser.email}</p>
        <p class="user-id">ID: {formatUserId(currentUser.id)}</p>
      </div>
    </div>

    <div class="user-details">
      <div class="detail-item">
        <span class="label">Status:</span>
        <span class="value authenticated">Authenticated</span>
      </div>
      <div class="detail-item">
        <span class="label">Account Type:</span>
        <span class="value">Standard User</span>
      </div>
      <div class="detail-item">
        <span class="label">Last Login:</span>
        <span class="value">Just now</span>
      </div>
    </div>

    <div class="actions">
      <button 
        class="logout-btn" 
        on:click={handleLogout}
        disabled={isLoggingOut}
      >
        {#if isLoggingOut}
          <span class="spinner"></span>
          Logging out...
        {:else}
          Logout
        {/if}
      </button>
    </div>
  {:else}
    <div class="no-user">
      <p>No user logged in</p>
    </div>
  {/if}
</div>

<style>
  .profile-panel {
    text-align: center;
  }

  .user-header {
    margin-bottom: 2rem;
  }

  .avatar {
    width: 80px;
    height: 80px;
    margin: 0 auto 1rem;
    border-radius: 50%;
    overflow: hidden;
    border: 3px solid #e2e8f0;
  }

  .avatar img {
    width: 100%;
    height: 100%;
    object-fit: cover;
  }

  .initials {
    width: 100%;
    height: 100%;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    display: flex;
    align-items: center;
    justify-content: center;
    color: white;
    font-size: 1.5rem;
    font-weight: 600;
  }

  .user-info {
    margin-bottom: 1rem;
  }

  .name {
    color: #2d3748;
    font-size: 1.5rem;
    font-weight: 600;
    margin-bottom: 0.5rem;
  }

  .email {
    color: #718096;
    font-size: 1rem;
    margin-bottom: 0.25rem;
  }

  .user-id {
    color: #a0aec0;
    font-size: 0.875rem;
    font-family: monospace;
  }

  .user-details {
    background: #f7fafc;
    border-radius: 8px;
    padding: 1.5rem;
    margin-bottom: 2rem;
    text-align: left;
  }

  .detail-item {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.5rem 0;
    border-bottom: 1px solid #e2e8f0;
  }

  .detail-item:last-child {
    border-bottom: none;
  }

  .label {
    color: #4a5568;
    font-weight: 500;
  }

  .value {
    color: #2d3748;
    font-weight: 400;
  }

  .value.authenticated {
    color: #38a169;
    font-weight: 500;
  }

  .actions {
    display: flex;
    justify-content: center;
    gap: 1rem;
  }

  .logout-btn {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.75rem 1.5rem;
    background-color: #e53e3e;
    color: white;
    border: none;
    border-radius: 6px;
    font-size: 0.875rem;
    font-weight: 500;
    cursor: pointer;
    transition: background-color 0.2s ease;
  }

  .logout-btn:hover:not(:disabled) {
    background-color: #c53030;
  }

  .logout-btn:disabled {
    opacity: 0.6;
    cursor: not-allowed;
  }

  .spinner {
    width: 14px;
    height: 14px;
    border: 2px solid transparent;
    border-top: 2px solid currentColor;
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }

  .no-user {
    text-align: center;
    color: #718096;
    font-style: italic;
  }

  @keyframes spin {
    from {
      transform: rotate(0deg);
    }
    to {
      transform: rotate(360deg);
    }
  }

  /* Dark mode support */
  :global(.dark-mode) .profile-panel {
    color: white;
  }

  :global(.dark-mode) .name {
    color: white;
  }

  :global(.dark-mode) .user-details {
    background-color: #4a5568;
  }

  :global(.dark-mode) .detail-item {
    border-bottom-color: #718096;
  }

  :global(.dark-mode) .label {
    color: #cbd5e0;
  }

  :global(.dark-mode) .value {
    color: white;
  }
</style>
