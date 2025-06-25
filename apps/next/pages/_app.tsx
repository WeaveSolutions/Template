import '../styles/globals.css';
import React, { useState, useEffect } from 'react';
import { UserProvider } from '@auth0/nextjs-auth0/client';

// Client-only rendering to prevent SSR issues
function ClientOnly({ children }: { children: any }) {
  const [mounted, setMounted] = useState(false);

  useEffect(() => {
    setMounted(true);
  }, []);

  return mounted ? <>{children}</> : null;
}

// Check if Auth0 is properly configured
const isAuth0Configured = () => {
  if (typeof window === 'undefined') return false;
  
  const domain = process.env.NEXT_PUBLIC_AUTH0_DOMAIN;
  const clientId = process.env.NEXT_PUBLIC_AUTH0_CLIENT_ID;
  
  return domain && clientId && 
         domain !== 'your-tenant.auth0.com' && 
         clientId !== 'your-auth0-client-id';
};

export default function MyApp({ Component, pageProps }: any) {
  const [auth0Ready, setAuth0Ready] = useState(false);

  useEffect(() => {
    setAuth0Ready(isAuth0Configured());
  }, []);

  // Development mode without Auth0
  if (!auth0Ready) {
    return (
      <ClientOnly>
        <Component {...pageProps} />
      </ClientOnly>
    );
  }

  // Production mode with Auth0
  return (
    <UserProvider>
      <ClientOnly>
        <Component {...pageProps} />
      </ClientOnly>
    </UserProvider>
  );
}
