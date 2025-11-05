// Simple test page without React Native dependencies
export default function TestPage() {
  return (
    <div style={{ padding: '2rem', fontFamily: 'system-ui' }}>
      <h1>✅ Next.js is Working!</h1>
      <p>This page proves the server is running correctly.</p>
      <p>Node version: {typeof window === 'undefined' ? 'Server' : 'Client'}</p>
    </div>
  );
}
