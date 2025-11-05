import { NextResponse } from 'next/server';
import type { NextRequest } from 'next/server';

// NOTE: This middleware is part of the legacy Next.js app structure.
// The new authentication server is located at: microservices/api/api-typescript
// For production deployments, use the terraform microservices architecture.

// Simple middleware for Auth0 protection without SDK imports
export async function middleware(request: NextRequest) {
  // Check if the route should be protected
  const isProtectedRoute = 
    request.nextUrl.pathname.startsWith('/dashboard') ||
    request.nextUrl.pathname.startsWith('/profile');

  // If trying to access a protected route
  if (isProtectedRoute) {
    // Check for auth0 session cookie - Auth0 uses 'appSession' by default
    const sessionCookie = request.cookies.get('appSession');
    
    // If no session cookie exists, redirect to login
    if (!sessionCookie) {
      const redirectUrl = new URL('/api/auth/login', request.url);
      // Store the original URL to redirect back after login
      redirectUrl.searchParams.set('returnTo', request.nextUrl.pathname);
      return NextResponse.redirect(redirectUrl);
    }
  }

  return NextResponse.next();
}

// Configure which routes should be protected
export const config = {
  matcher: [
    '/dashboard/:path*',
    '/profile/:path*',
    // Add other protected routes here
  ],
};
