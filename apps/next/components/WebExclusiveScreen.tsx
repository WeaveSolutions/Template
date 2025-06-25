import React, { useState, useEffect } from 'react';
import { useTheme, WebHeader, Footer } from '@shared/components';

export function WebExclusiveScreen() {
  const { theme } = useTheme();

  return (
    <div style={{ display: 'flex', flexDirection: 'column', minHeight: '100vh' }}>
      <WebHeader />
      <div
        style={{
          background: theme.colors.background,
          flex: 1,
          margin: 0,
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          boxSizing: 'border-box',
          position: 'relative',
          overflow: 'hidden',
          paddingTop: '120px', // Increased padding for fixed header
          paddingBottom: '60px',
          paddingLeft: '20px',
          paddingRight: '20px',
        }}
      >
        {/* Floating gradient orbs */}
        <div style={{
          position: 'absolute',
          top: '-20%',
          left: '-10%',
          width: '500px',
          height: '500px',
          background: 'radial-gradient(circle, rgba(102,126,234,0.2) 0%, transparent 70%)',
          borderRadius: '50%',
          filter: 'blur(60px)',
        }} />
        <div style={{
          position: 'absolute',
          bottom: '-20%',
          right: '-10%',
          width: '600px',
          height: '600px',
          background: 'radial-gradient(circle, rgba(240,147,251,0.2) 0%, transparent 70%)',
          borderRadius: '50%',
          filter: 'blur(60px)',
        }} />

        <h1 style={{
          margin: 0, 
          textAlign: 'center', 
          width: '100%', 
          fontSize: '56px',
          fontWeight: '900',
          color: theme.colors.text, // Use regular text color instead of gradient
          position: 'relative',
          zIndex: 1,
        }}>
          Web Exclusive Features
        </h1>
        <p style={{
          marginTop: '16px', 
          textAlign: 'center', 
          width: '100%', 
          fontSize: '20px',
          color: theme.colors.textSecondary,
          maxWidth: '600px',
          lineHeight: 1.6,
          position: 'relative',
          zIndex: 1,
        }}>
          This page showcases features that are only available on the web platform
        </p>

        <div style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(auto-fit, minmax(250px, 1fr))',
          gap: '24px',
          marginTop: '48px',
          width: '100%',
          maxWidth: '900px',
          padding: '0 20px',
          position: 'relative',
          zIndex: 1,
        }}>
          <div style={{
            background: theme.colors.cardBackground,
            border: `1px solid ${theme.colors.cardBorder}`,
            backdropFilter: 'blur(10px)',
            borderRadius: '16px',
            padding: '32px',
            textAlign: 'center',
          }}>
            <div style={{ fontSize: '36px', marginBottom: '16px' }}>🌐</div>
            <h3 style={{ color: theme.colors.text, marginBottom: '8px' }}>SEO Optimized</h3>
            <p style={{ color: theme.colors.textSecondary, fontSize: '14px' }}>Server-side rendering for better search visibility</p>
          </div>
          
          <div style={{
            background: theme.colors.cardBackground,
            border: `1px solid ${theme.colors.cardBorder}`,
            backdropFilter: 'blur(10px)',
            borderRadius: '16px',
            padding: '32px',
            textAlign: 'center',
          }}>
            <div style={{ fontSize: '36px', marginBottom: '16px' }}>⚡</div>
            <h3 style={{ color: theme.colors.text, marginBottom: '8px' }}>Web APIs</h3>
            <p style={{ color: theme.colors.textSecondary, fontSize: '14px' }}>Access to browser-specific APIs and features</p>
          </div>
          
          <div style={{
            background: theme.colors.cardBackground,
            border: `1px solid ${theme.colors.cardBorder}`,
            backdropFilter: 'blur(10px)',
            borderRadius: '16px',
            padding: '32px',
            textAlign: 'center',
          }}>
            <div style={{ fontSize: '36px', marginBottom: '16px' }}>🎨</div>
            <h3 style={{ color: theme.colors.text, marginBottom: '8px' }}>Advanced CSS</h3>
            <p style={{ color: theme.colors.textSecondary, fontSize: '14px' }}>Full CSS capabilities and animations</p>
          </div>
        </div>
      </div>
      <Footer />
    </div>
  );
}
