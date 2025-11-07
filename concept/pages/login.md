# Login Page Template

## Structure
```tsx
export default function LoginPage() {
  return (
    <div className="auth-page">
      <div className="auth-container">
        <div className="auth-form">
          <h1>Welcome Back</h1>
          <form onSubmit={handleLogin}>
            <input type="email" placeholder="Email" required />
            <input type="password" placeholder="Password" required />
            <button type="submit">Log In</button>
          </form>
          
          <div className="divider">OR</div>
          
          <button onClick={loginWithGoogle}>Continue with Google</button>
          <button onClick={loginWithGitHub}>Continue with GitHub</button>
          
          <p>Don't have an account? <a href="/signup">Sign up</a></p>
        </div>
      </div>
    </div>
  );
}
```

## Features
- Email/password login
- OAuth providers (Google, GitHub)
- Password reset link
- Remember me option
- Error handling
