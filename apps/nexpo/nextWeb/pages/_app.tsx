import '../styles/globals.css';
import React, { useState, useEffect } from 'react';
import { Auth0Provider } from '@auth0/nextjs-auth0';
import Head from 'next/head';

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
      <>
        <Head>
          <title>Nexpo Web</title>
          <meta name="description" content="Nexpo Enterprise Web Platform - Cross-Platform Development Stack" />
          <link rel="icon" type="image/png" href="/WeaveLogo.png" />
        </Head>
        <ClientOnly>
          <Component {...pageProps} />
        </ClientOnly>
      </>
    );
  }

  // Production mode with Auth0
  return (
    <Auth0Provider>
      <Head>
        <title>Nexpo Web</title>
        <meta name="description" content="Nexpo Enterprise Web Platform - Cross-Platform Development Stack" />
        <link rel="icon" type="image/png" href="/WeaveLogo.png" />
      </Head>
      <ClientOnly>
        <Component {...pageProps} />
      </ClientOnly>
    </Auth0Provider>
  );
}
