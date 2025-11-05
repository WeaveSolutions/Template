/**
 * Scene3D Component
 * 
 * Main React component for rendering EustressEngine 3D scenes in the browser.
 * Handles WASM initialization, canvas mounting, and lifecycle management.
 * 
 * @example
 * ```tsx
 * import { Scene3D } from '@weave/eustress-core';
 * 
 * function App() {
 *   return (
 *     <Scene3D
 *       scene="mindspace"
 *       onLoad={() => console.log('Scene loaded')}
 *       onError={(err) => console.error(err)}
 *     />
 *   );
 * }
 * ```
 */

'use client';

import React, { useEffect, useRef, useState } from 'react';

export interface Scene3DProps {
  /** Scene identifier (e.g., 'mindspace', 'timebelt') */
  scene?: string;
  /** Canvas ID for engine mounting */
  canvasId?: string;
  /** Custom canvas styles */
  style?: React.CSSProperties;
  /** Custom canvas className */
  className?: string;
  /** Authentication token for backend communication */
  authToken?: string;
  /** User context for multiplayer features */
  user?: {
    id: string;
    name: string;
    avatar?: string;
  };
  /** Callback when scene loads successfully */
  onLoad?: () => void;
  /** Callback on error */
  onError?: (error: Error) => void;
  /** Enable performance monitoring overlay */
  showPerformance?: boolean;
  /** Asset base URL */
  assetBaseUrl?: string;
}

export function Scene3D({
  scene = 'default',
  canvasId = 'eustress-canvas',
  style = {},
  className = '',
  authToken,
  user,
  onLoad,
  onError,
  showPerformance = false,
  assetBaseUrl = '/assets',
}: Scene3DProps) {
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [fps, setFps] = useState<number>(0);

  useEffect(() => {
    let mounted = true;
    let animationFrameId: number;

    async function initEngine() {
      try {
        setLoading(true);
        setError(null);

        // Dynamic import of WASM module
        const wasmModule = await import('../../wasm/eustress_engine.js');
        
        // Initialize WASM
        await wasmModule.default();
        
        if (!mounted) return;

        // Initialize engine with configuration
        if (wasmModule.init_web) {
          wasmModule.init_web();
        }

        // Pass configuration to engine
        if (typeof window !== 'undefined') {
          (window as any).__eustress_config = {
            scene,
            authToken,
            user,
            assetBaseUrl,
            showPerformance,
          };
        }

        setLoading(false);
        onLoad?.();

        // Performance monitoring loop
        if (showPerformance) {
          let lastTime = performance.now();
          let frames = 0;

          function updateFps() {
            frames++;
            const currentTime = performance.now();
            const delta = currentTime - lastTime;

            if (delta >= 1000) {
              setFps(Math.round((frames * 1000) / delta));
              frames = 0;
              lastTime = currentTime;
            }

            animationFrameId = requestAnimationFrame(updateFps);
          }

          updateFps();
        }
      } catch (err) {
        if (!mounted) return;
        
        const errorMessage = err instanceof Error ? err.message : 'Failed to load 3D engine';
        setError(errorMessage);
        setLoading(false);
        onError?.(err instanceof Error ? err : new Error(errorMessage));
        console.error('EustressEngine initialization error:', err);
      }
    }

    initEngine();

    return () => {
      mounted = false;
      if (animationFrameId) {
        cancelAnimationFrame(animationFrameId);
      }
    };
  }, [scene, authToken, user, onLoad, onError, showPerformance, assetBaseUrl]);

  const defaultStyle: React.CSSProperties = {
    width: '100%',
    height: '100%',
    display: 'block',
    ...style,
  };

  return (
    <div style={{ position: 'relative', width: '100%', height: '100%' }}>
      <canvas
        ref={canvasRef}
        id={canvasId}
        className={className}
        style={defaultStyle}
      />
      
      {loading && (
        <div style={{
          position: 'absolute',
          top: 0,
          left: 0,
          right: 0,
          bottom: 0,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          background: 'rgba(0, 0, 0, 0.8)',
          color: 'white',
          fontSize: '1.2rem',
        }}>
          <div>
            <div style={{ marginBottom: '1rem' }}>Loading 3D Engine...</div>
            <div style={{ 
              width: '200px', 
              height: '4px', 
              background: 'rgba(255,255,255,0.2)',
              borderRadius: '2px',
              overflow: 'hidden',
            }}>
              <div style={{
                width: '100%',
                height: '100%',
                background: 'linear-gradient(90deg, #3b82f6, #8b5cf6)',
                animation: 'loading 1.5s ease-in-out infinite',
              }} />
            </div>
          </div>
        </div>
      )}

      {error && (
        <div style={{
          position: 'absolute',
          top: 0,
          left: 0,
          right: 0,
          bottom: 0,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          background: 'rgba(220, 38, 38, 0.1)',
          color: '#dc2626',
          padding: '2rem',
          textAlign: 'center',
        }}>
          <div>
            <div style={{ fontSize: '1.5rem', marginBottom: '1rem' }}>⚠️</div>
            <div style={{ fontWeight: 'bold', marginBottom: '0.5rem' }}>
              Failed to Load 3D Engine
            </div>
            <div style={{ fontSize: '0.9rem', opacity: 0.8 }}>{error}</div>
          </div>
        </div>
      )}

      {showPerformance && !loading && !error && (
        <div style={{
          position: 'absolute',
          top: '10px',
          right: '10px',
          background: 'rgba(0, 0, 0, 0.7)',
          color: 'white',
          padding: '8px 12px',
          borderRadius: '4px',
          fontSize: '14px',
          fontFamily: 'monospace',
        }}>
          {fps} FPS
        </div>
      )}

      <style>{`
        @keyframes loading {
          0% { transform: translateX(-100%); }
          100% { transform: translateX(100%); }
        }
      `}</style>
    </div>
  );
}
