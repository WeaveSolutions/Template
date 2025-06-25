import React from 'react';
import { AuthProvider } from './AuthProvider';

type ClientOnlyProps = {
  children: React.ReactNode;
};

// Client-only rendering to prevent SSR issues
function ClientOnly({ children }: ClientOnlyProps) {
  const [mounted, setMounted] = React.useState(false);

  React.useEffect(() => {
    setMounted(true);
  }, []);

  return mounted ? <>{children}</> : null;
}

type ClientAuth0ProviderProps = {
  children: React.ReactNode;
};

export function ClientAuth0Provider({ children }: ClientAuth0ProviderProps) {
  return (
    <AuthProvider>
      <ClientOnly>
        {children}
      </ClientOnly>
    </AuthProvider>
  );
}

export { useAuth } from './AuthProvider';
