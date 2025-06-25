import React from 'react';
import { useAuth } from '@shared/provider';

export const AuthDemo: React.FC = () => {
  const { user, loading, signIn, signUp, signOut, signInWithGoogle, isPasskeySupported, signInWithPasskey } = useAuth();

  if (loading) {
    return <div>Loading...</div>;
  }

  if (user) {
    return (
      <div className="p-4">
        <h2 className="text-xl font-bold mb-4">Welcome, {user.name || user.email}!</h2>
        <div className="space-y-2">
          <p>User ID: {user.id}</p>
          <p>Email: {user.email}</p>
          {user.email_verified && <p>✅ Email verified</p>}
        </div>
        
        <button
          onClick={signOut}
          className="mt-4 px-4 py-2 bg-red-500 text-white rounded hover:bg-red-600"
        >
          Sign Out
        </button>
      </div>
    );
  }

  return (
    <div className="p-4">
      <h2 className="text-xl font-bold mb-4">Authentication Demo</h2>
      
      <div className="space-y-2">
        <h3 className="font-semibold">Sign In Options:</h3>
        
        <button
          onClick={() => signIn('test@example.com', 'password123')}
          className="block px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
        >
          Sign In with Email/Password
        </button>

        <button
          onClick={() => signInWithGoogle('signin')}
          className="block px-4 py-2 bg-red-500 text-white rounded hover:bg-red-600"
        >
          Sign In with Google
        </button>
        
        {isPasskeySupported() && (
          <button
            onClick={() => signInWithPasskey()}
            className="block px-4 py-2 bg-green-500 text-white rounded hover:bg-green-600"
          >
            Sign In with Passkey
          </button>
        )}
      </div>
      
      <div className="space-y-2 mt-4">
        <h3 className="font-semibold">Sign Up Options:</h3>
        
        <button
          onClick={() => signUp('newuser@example.com', 'password123', { 
            firstName: 'John',
            lastName: 'Doe'
          })}
          className="block px-4 py-2 bg-purple-500 text-white rounded hover:bg-purple-600"
        >
          Sign Up with Email/Password
        </button>
        
        <button
          onClick={() => signInWithGoogle('signup')}
          className="block px-4 py-2 bg-pink-500 text-white rounded hover:bg-pink-600"
        >
          Sign Up with Google
        </button>
      </div>
    </div>
  );
};
