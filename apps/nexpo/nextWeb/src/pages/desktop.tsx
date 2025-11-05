import React from 'react';
import Head from 'next/head';
import { DesktopLayout, DesktopFeaturePanel } from '@/components/desktop';
import { isDesktopApp } from '@shared/utils';
import { useEffect, useState } from 'react';

export default function DesktopPage() {
  const [isDesktop, setIsDesktop] = useState(false);

  useEffect(() => {
    setIsDesktop(isDesktopApp());
  }, []);

  return (
    <>
      <Head>
        <title>Desktop Features - Nexpo</title>
        <meta name="description" content="Desktop-specific features for Nexpo" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <link rel="icon" href="/favicon.ico" />
      </Head>
      
      <DesktopLayout showTitleBar={isDesktop} showSystemInfo={isDesktop}>
        <main style={{ padding: '2rem', minHeight: '100vh' }}>
          <div style={{ maxWidth: '1200px', margin: '0 auto' }}>
            <header style={{ marginBottom: '2rem' }}>
              <h1 style={{ fontSize: '2.5rem', fontWeight: 'bold', marginBottom: '1rem' }}>
                🖥️ Desktop Features
              </h1>
              <p style={{ fontSize: '1.2rem', color: '#666', marginBottom: '2rem' }}>
                {isDesktop 
                  ? 'Welcome to the desktop version of Nexpo! Explore native desktop features below.' 
                  : 'This page is optimized for the desktop version of Nexpo. Please use the Tauri app to access these features.'
                }\n              </p>
            </header>

            {isDesktop ? (
              <div style={{ display: 'grid', gap: '2rem' }}>
                <section>
                  <h2 style={{ fontSize: '1.8rem', fontWeight: 'semibold', marginBottom: '1rem' }}>
                    🚀 Native Desktop Features
                  </h2>
                  <DesktopFeaturePanel />
                </section>

                <section>
                  <h2 style={{ fontSize: '1.8rem', fontWeight: 'semibold', marginBottom: '1rem' }}>
                    📱 App Information
                  </h2>
                  <div style={{ 
                    background: 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
                    color: 'white',
                    padding: '1.5rem',
                    borderRadius: '8px',
                    display: 'grid',
                    gridTemplateColumns: 'repeat(auto-fit, minmax(250px, 1fr))',
                    gap: '1rem'
                  }}>
                    <div>
                      <h3 style={{ fontSize: '1.2rem', fontWeight: 'semibold', marginBottom: '0.5rem' }}>
                        Platform
                      </h3>
                      <p>Tauri Desktop App</p>
                    </div>
                    <div>
                      <h3 style={{ fontSize: '1.2rem', fontWeight: 'semibold', marginBottom: '0.5rem' }}>
                        Frontend
                      </h3>
                      <p>Next.js + React</p>
                    </div>
                    <div>
                      <h3 style={{ fontSize: '1.2rem', fontWeight: 'semibold', marginBottom: '0.5rem' }}>
                        Backend
                      </h3>
                      <p>Rust + Tauri</p>
                    </div>
                  </div>
                </section>

                <section>
                  <h2 style={{ fontSize: '1.8rem', fontWeight: 'semibold', marginBottom: '1rem' }}>
                    🎯 Desktop-Specific Features
                  </h2>
                  <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))', gap: '1rem' }}>
                    <div style={{ 
                      background: 'rgba(255, 255, 255, 0.05)',
                      padding: '1.5rem',
                      borderRadius: '8px',
                      border: '1px solid rgba(255, 255, 255, 0.1)'
                    }}>
                      <h3 style={{ fontSize: '1.2rem', fontWeight: 'semibold', marginBottom: '0.5rem' }}>
                        🗂️ File System Access
                      </h3>
                      <p>Open and save files using native system dialogs</p>
                    </div>
                    <div style={{ 
                      background: 'rgba(255, 255, 255, 0.05)',
                      padding: '1.5rem',
                      borderRadius: '8px',
                      border: '1px solid rgba(255, 255, 255, 0.1)'
                    }}>
                      <h3 style={{ fontSize: '1.2rem', fontWeight: 'semibold', marginBottom: '0.5rem' }}>
                        🔔 System Notifications
                      </h3>
                      <p>Send native desktop notifications</p>
                    </div>
                    <div style={{ 
                      background: 'rgba(255, 255, 255, 0.05)',
                      padding: '1.5rem',
                      borderRadius: '8px',
                      border: '1px solid rgba(255, 255, 255, 0.1)'
                    }}>
                      <h3 style={{ fontSize: '1.2rem', fontWeight: 'semibold', marginBottom: '0.5rem' }}>
                        🪟 Window Management
                      </h3>
                      <p>Control window size, position, and behavior</p>
                    </div>
                    <div style={{ 
                      background: 'rgba(255, 255, 255, 0.05)',
                      padding: '1.5rem',
                      borderRadius: '8px',
                      border: '1px solid rgba(255, 255, 255, 0.1)'
                    }}>
                      <h3 style={{ fontSize: '1.2rem', fontWeight: 'semibold', marginBottom: '0.5rem' }}>
                        🔄 Auto Updates
                      </h3>
                      <p>Automatic application updates</p>
                    </div>
                    <div style={{ 
                      background: 'rgba(255, 255, 255, 0.05)',
                      padding: '1.5rem',
                      borderRadius: '8px',
                      border: '1px solid rgba(255, 255, 255, 0.1)'
                    }}>
                      <h3 style={{ fontSize: '1.2rem', fontWeight: 'semibold', marginBottom: '0.5rem' }}>
                        🔧 System Information
                      </h3>
                      <p>Access detailed system and hardware information</p>
                    </div>
                    <div style={{ 
                      background: 'rgba(255, 255, 255, 0.05)',
                      padding: '1.5rem',
                      borderRadius: '8px',
                      border: '1px solid rgba(255, 255, 255, 0.1)'
                    }}>
                      <h3 style={{ fontSize: '1.2rem', fontWeight: 'semibold', marginBottom: '0.5rem' }}>
                        🎨 Custom Title Bar
                      </h3>
                      <p>Native-looking custom title bar with window controls</p>
                    </div>
                  </div>
                </section>
              </div>
            ) : (
              <div style={{ textAlign: 'center', padding: '4rem' }}>
                <div style={{ fontSize: '4rem', marginBottom: '1rem' }}>🖥️</div>
                <h2 style={{ fontSize: '1.8rem', fontWeight: 'semibold', marginBottom: '1rem' }}>
                  Desktop Features Not Available
                </h2>
                <p style={{ color: '#666', marginBottom: '2rem' }}>
                  These features are only available in the Tauri desktop application.
                </p>
                <div style={{ 
                  background: 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
                  color: 'white',
                  padding: '1rem',
                  borderRadius: '8px',
                  display: 'inline-block'
                }}>
                  <p>
                    To access desktop features, please download and run the Tauri desktop version of Nexpo.
                  </p>
                </div>
              </div>
            )}
          </div>
        </main>
      </DesktopLayout>
    </>
  );
}
