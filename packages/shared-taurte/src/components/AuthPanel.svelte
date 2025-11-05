<script lang="ts">
  import { invoke } from "@tauri-apps/api/core";
  import { authStore } from "../stores/authStore";

  let email = "";
  let password = "";
  let isLoading = false;
  let errorMessage = "";

  const handleLogin = async () => {
    if (!email || !password) {
      errorMessage = "Please enter both email and password";
      return;
    }

    isLoading = true;
    errorMessage = "";
    authStore.setLoading(true);

    try {
      const response = await invoke<{
        success: boolean;
        data?: any;
        error?: string;
      }>("authenticate_user", { email, password });

      if (response.success && response.data) {
        authStore.setUser(response.data);
        email = "";
        password = "";
      } else {
        errorMessage = response.error || "Authentication failed";
      }
    } catch (error) {
      errorMessage = `Authentication error: ${error}`;
      console.error("Authentication error:", error);
    } finally {
      isLoading = false;
      authStore.setLoading(false);
    }
  };

  const handleKeyPress = (event: KeyboardEvent) => {
    if (event.key === "Enter") {
      handleLogin();
    }
  };

  const handleDemoLogin = () => {
    authStore.setUser({
      id: "demo-user",
      name: "Demo User",
      email: "demo@example.com",
      avatar_url: "https://via.placeholder.com/150",
    });
  };
</script>

<div class="auth-panel">
  <div class="header">
    <h2>Authentication</h2>
    <p>Sign in to your account to access all features</p>
  </div>

  <form on:submit|preventDefault={handleLogin} class="form">
    {#if errorMessage}
      <div class="error-message">
        <span class="error-icon">⚠️</span>
        {errorMessage}
      </div>
    {/if}

    <div class="form-group">
      <label for="email">Email</label>
      <input
        type="email"
        id="email"
        bind:value={email}
        on:keypress={handleKeyPress}
        placeholder="Enter your email"
        disabled={isLoading}
        required
      />
    </div>

    <div class="form-group">
      <label for="password">Password</label>
      <input
        type="password"
        id="password"
        bind:value={password}
        on:keypress={handleKeyPress}
        placeholder="Enter your password"
        disabled={isLoading}
        required
      />
    </div>

    <button type="submit" class="login-btn" disabled={isLoading}>
      {#if isLoading}
        <span class="spinner"></span>
        Signing in...
      {:else}
        Sign In
      {/if}
    </button>

    <div class="demo-section">
      <p class="demo-text">Or try the demo mode:</p>
      <button
        type="button"
        class="demo-btn"
        on:click={handleDemoLogin}
        disabled={isLoading}
      >
        Demo Login
      </button>
    </div>
  </form>
</div>

<style>
  .auth-panel {
    width: 100%;
    max-width: 400px;
    margin: 0 auto;
    padding: 2rem;
    background: white;
    border-radius: 12px;
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.07);
  }

  .header {
    text-align: center;
    margin-bottom: 2rem;
  }

  .header h2 {
    color: #2d3748;
    font-size: 1.75rem;
    font-weight: 600;
    margin-bottom: 0.5rem;
  }

  .header p {
    color: #718096;
    font-size: 0.875rem;
  }

  .form {
    display: flex;
    flex-direction: column;
    gap: 1.5rem;
  }

  .error-message {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.75rem;
    background-color: #fed7d7;
    border: 1px solid #feb2b2;
    color: #c53030;
    border-radius: 6px;
    font-size: 0.875rem;
  }

  .error-icon {
    font-size: 1rem;
  }

  .form-group {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }

  .form-group label {
    color: #4a5568;
    font-weight: 500;
    font-size: 0.875rem;
  }

  .form-group input {
    padding: 0.75rem;
    border: 1px solid #e2e8f0;
    border-radius: 6px;
    font-size: 1rem;
    transition: border-color 0.2s ease, box-shadow 0.2s ease;
  }

  .form-group input:focus {
    outline: none;
    border-color: #3182ce;
    box-shadow: 0 0 0 3px rgba(49, 130, 206, 0.1);
  }

  .form-group input:disabled {
    background-color: #f7fafc;
    cursor: not-allowed;
  }

  .login-btn {
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 0.5rem;
    width: 100%;
    padding: 0.875rem;
    background-color: #3182ce;
    color: white;
    border: none;
    border-radius: 6px;
    font-size: 1rem;
    font-weight: 500;
    cursor: pointer;
    transition: background-color 0.2s ease;
  }

  .login-btn:hover:not(:disabled) {
    background-color: #2c5aa0;
  }

  .login-btn:disabled {
    opacity: 0.6;
    cursor: not-allowed;
  }

  .spinner {
    width: 16px;
    height: 16px;
    border: 2px solid transparent;
    border-top: 2px solid currentColor;
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }

  .demo-section {
    text-align: center;
    border-top: 1px solid #e2e8f0;
    padding-top: 1.5rem;
  }

  .demo-text {
    color: #718096;
    font-size: 0.875rem;
    margin-bottom: 0.75rem;
  }

  .demo-btn {
    padding: 0.5rem 1rem;
    background-color: #ed8936;
    color: white;
    border: none;
    border-radius: 6px;
    font-size: 0.875rem;
    font-weight: 500;
    cursor: pointer;
    transition: background-color 0.2s ease;
  }

  .demo-btn:hover:not(:disabled) {
    background-color: #dd6b20;
  }

  .demo-btn:disabled {
    opacity: 0.6;
    cursor: not-allowed;
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
  :global(.dark-mode) .auth-panel {
    background-color: #2d3748;
    color: white;
  }

  :global(.dark-mode) .header h2 {
    color: white;
  }

  :global(.dark-mode) .form-group input {
    background-color: #4a5568;
    border-color: #718096;
    color: white;
  }

  :global(.dark-mode) .form-group input:focus {
    border-color: #63b3ed;
    box-shadow: 0 0 0 3px rgba(99, 179, 237, 0.1);
  }
</style>
