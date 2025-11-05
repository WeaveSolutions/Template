/**
 * @weave/eustress-core
 * 
 * EustressEngine integration for Weave Template
 * React/Next.js components and hooks for 3D rendering
 */

// Components
export { Scene3D } from './components/Scene3D';
export type { Scene3DProps } from './components/Scene3D';

// Hooks
export { useEustress } from './hooks/useEustress';
export type { EustressEngineState, EustressEngineControls } from './hooks/useEustress';

// Version
export const VERSION = '0.1.0';
