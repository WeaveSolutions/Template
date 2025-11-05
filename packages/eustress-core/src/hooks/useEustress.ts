/**
 * useEustress Hook
 * 
 * Main hook for interacting with the EustressEngine from React components.
 * Provides engine state, controls, and event handlers.
 */

'use client';

import { useState, useEffect, useCallback } from 'react';

export interface EustressEngineState {
  /** Whether the engine is initialized */
  isInitialized: boolean;
  /** Current loading state */
  isLoading: boolean;
  /** Error message if any */
  error: string | null;
  /** Performance metrics */
  metrics: {
    fps: number;
    frameTime: number;
    entityCount: number;
  };
}

export interface EustressEngineControls {
  /** Load a new scene */
  loadScene: (sceneId: string) => Promise<void>;
  /** Send event to engine */
  sendEvent: (eventType: string, data: any) => void;
  /** Take screenshot */
  takeScreenshot: () => Promise<Blob | null>;
  /** Toggle performance overlay */
  togglePerformance: () => void;
}

/**
 * Hook to interact with EustressEngine
 * 
 * @example
 * ```tsx
 * function MyComponent() {
 *   const { state, controls } = useEustress();
 *   
 *   return (
 *     <div>
 *       <p>FPS: {state.metrics.fps}</p>
 *       <button onClick={() => controls.loadScene('new-scene')}>
 *         Load Scene
 *       </button>
 *     </div>
 *   );
 * }
 * ```
 */
export function useEustress() {
  const [state, setState] = useState<EustressEngineState>({
    isInitialized: false,
    isLoading: false,
    error: null,
    metrics: {
      fps: 0,
      frameTime: 0,
      entityCount: 0,
    },
  });

  // Listen for engine events
  useEffect(() => {
    const handleEngineEvent = (event: CustomEvent) => {
      const { type, data } = event.detail;

      switch (type) {
        case 'initialized':
          setState(prev => ({ ...prev, isInitialized: true, isLoading: false }));
          break;
        case 'loading':
          setState(prev => ({ ...prev, isLoading: true }));
          break;
        case 'error':
          setState(prev => ({ ...prev, error: data.message, isLoading: false }));
          break;
        case 'metrics':
          setState(prev => ({ ...prev, metrics: data }));
          break;
      }
    };

    window.addEventListener('eustress-event' as any, handleEngineEvent);
    return () => {
      window.removeEventListener('eustress-event' as any, handleEngineEvent);
    };
  }, []);

  const loadScene = useCallback(async (sceneId: string) => {
    setState(prev => ({ ...prev, isLoading: true, error: null }));
    
    try {
      // Send load scene command to engine
      if (typeof window !== 'undefined' && (window as any).__eustress_load_scene) {
        await (window as any).__eustress_load_scene(sceneId);
      }
      setState(prev => ({ ...prev, isLoading: false }));
    } catch (error) {
      setState(prev => ({
        ...prev,
        isLoading: false,
        error: error instanceof Error ? error.message : 'Failed to load scene',
      }));
    }
  }, []);

  const sendEvent = useCallback((eventType: string, data: any) => {
    if (typeof window !== 'undefined' && (window as any).__eustress_send_event) {
      (window as any).__eustress_send_event(eventType, data);
    }
  }, []);

  const takeScreenshot = useCallback(async (): Promise<Blob | null> => {
    const canvas = document.getElementById('eustress-canvas') as HTMLCanvasElement;
    if (!canvas) return null;

    return new Promise((resolve) => {
      canvas.toBlob((blob) => resolve(blob));
    });
  }, []);

  const togglePerformance = useCallback(() => {
    if (typeof window !== 'undefined' && (window as any).__eustress_toggle_performance) {
      (window as any).__eustress_toggle_performance();
    }
  }, []);

  const controls: EustressEngineControls = {
    loadScene,
    sendEvent,
    takeScreenshot,
    togglePerformance,
  };

  return { state, controls };
}
