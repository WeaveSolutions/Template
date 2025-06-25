// Export the WebAuth class which intelligently selects between Auth0 and Dev auth
export { WebAuth } from './auth-web';

// Also export specific implementations if needed
export { Auth0WebAuth } from './auth-web-auth0';
export { DevAuth } from './auth-dev';
