# @weave/eustress-core

**EustressEngine integration for Weave Template**

React/Next.js components and hooks for seamless 3D rendering in web applications.

---

## Installation

Already included in Weave Template workspace. No separate installation needed.

```bash
# Build WASM from EustressEngine
pnpm run build:wasm --filter @weave/eustress-core
```

---

## Quick Start

### Basic Usage

```tsx
import { Scene3D } from '@weave/eustress-core';

export default function MyApp() {
  return (
    <div style={{ width: '100vw', height: '100vh' }}>
      <Scene3D
        scene="mindspace"
        onLoad={() => console.log('3D scene loaded!')}
        onError={(err) => console.error('Error:', err)}
      />
    </div>
  );
}
```

### With Auth0 Integration

```tsx
import { Scene3D } from '@weave/eustress-core';
import { useAuth } from '@auth0/auth0-react';

export default function SecureScene() {
  const { user, getAccessTokenSilently } = useAuth();
  const [token, setToken] = useState<string>();

  useEffect(() => {
    getAccessTokenSilently().then(setToken);
  }, []);

  return (
    <Scene3D
      scene="mindspace"
      authToken={token}
      user={{
        id: user?.sub || '',
        name: user?.name || '',
        avatar: user?.picture,
      }}
    />
  );
}
```

### With Performance Monitoring

```tsx
import { Scene3D } from '@weave/eustress-core';

export default function PerformanceScene() {
  return (
    <Scene3D
      scene="timebelt"
      showPerformance={true}
      onLoad={() => console.log('Loaded')}
    />
  );
}
```

---

## API Reference

### `<Scene3D />` Component

Main component for rendering 3D scenes.

#### Props

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `scene` | `string` | `'default'` | Scene identifier to load |
| `canvasId` | `string` | `'eustress-canvas'` | Canvas element ID |
| `style` | `React.CSSProperties` | `{}` | Custom canvas styles |
| `className` | `string` | `''` | Custom canvas class |
| `authToken` | `string` | - | Auth token for backend calls |
| `user` | `User` | - | User context object |
| `onLoad` | `() => void` | - | Callback when loaded |
| `onError` | `(error: Error) => void` | - | Error callback |
| `showPerformance` | `boolean` | `false` | Show FPS overlay |
| `assetBaseUrl` | `string` | `'/assets'` | Asset CDN URL |

#### User Object

```typescript
interface User {
  id: string;
  name: string;
  avatar?: string;
}
```

---

### `useEustress()` Hook

Hook for controlling the 3D engine from React components.

#### Returns

```typescript
{
  state: EustressEngineState;
  controls: EustressEngineControls;
}
```

#### State

```typescript
interface EustressEngineState {
  isInitialized: boolean;
  isLoading: boolean;
  error: string | null;
  metrics: {
    fps: number;
    frameTime: number;
    entityCount: number;
  };
}
```

#### Controls

```typescript
interface EustressEngineControls {
  loadScene: (sceneId: string) => Promise<void>;
  sendEvent: (eventType: string, data: any) => void;
  takeScreenshot: () => Promise<Blob | null>;
  togglePerformance: () => void;
}
```

#### Example

```tsx
import { useEustress } from '@weave/eustress-core';

function Controls() {
  const { state, controls } = useEustress();

  return (
    <div>
      <p>FPS: {state.metrics.fps}</p>
      <button onClick={() => controls.loadScene('new-scene')}>
        Load Scene
      </button>
      <button onClick={controls.togglePerformance}>
        Toggle Stats
      </button>
      <button onClick={async () => {
        const blob = await controls.takeScreenshot();
        // Download screenshot
      }}>
        Screenshot
      </button>
    </div>
  );
}
```

---

## Integration Examples

### Next.js App Router

```tsx
// app/3d/page.tsx
'use client';

import { Scene3D } from '@weave/eustress-core';

export default function ThreeDPage() {
  return (
    <main style={{ width: '100vw', height: '100vh' }}>
      <Scene3D scene="mindspace" showPerformance />
    </main>
  );
}
```

### Next.js with Layout

```tsx
// app/mindspace/layout.tsx
export default function MindSpaceLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <div className="relative w-full h-screen">
      <Scene3D scene="mindspace" />
      <div className="absolute inset-0 pointer-events-none">
        {children}
      </div>
    </div>
  );
}
```

### With Supabase Real-time

```tsx
import { Scene3D, useEustress } from '@weave/eustress-core';
import { useSupabase } from '@weave/supabase';

export default function CollaborativeScene() {
  const { controls } = useEustress();
  const supabase = useSupabase();

  useEffect(() => {
    // Listen for real-time updates
    const channel = supabase
      .channel('scene-updates')
      .on('broadcast', { event: 'object-moved' }, (payload) => {
        controls.sendEvent('sync-object', payload);
      })
      .subscribe();

    return () => {
      supabase.removeChannel(channel);
    };
  }, []);

  return <Scene3D scene="collaborative" />;
}
```

---

## Available Scenes

| Scene ID | Description | Use Case |
|----------|-------------|----------|
| `mindspace` | Collaborative workspace | Document sharing, 3D meetings |
| `timebelt` | Timeline visualization | Data visualization, analytics |
| `basic_3d` | Simple rotating cube | Testing, prototyping |

---

## Building WASM

### Manual Build

```bash
# From EustressEngine directory
cd ../../EustressEngine/eustress-engine-mvp
wasm-pack build --target web --out-dir ../../Templates/Weave/packages/eustress-core/wasm --release
```

### Automatic Build (Workspace)

```bash
# From Weave root
pnpm run build:wasm --filter @weave/eustress-core
```

---

## File Structure

```
packages/eustress-core/
├── src/
│   ├── components/
│   │   ├── Scene3D.tsx       # Main 3D scene component
│   │   └── index.ts
│   ├── hooks/
│   │   ├── useEustress.ts    # Engine control hook
│   │   └── index.ts
│   └── index.ts              # Package exports
├── wasm/                     # WASM build output (generated)
│   ├── eustress_engine.js
│   ├── eustress_engine_bg.wasm
│   └── package.json
├── package.json
├── tsconfig.json
└── README.md
```

---

## Performance

### Bundle Sizes
- **WASM**: ~2.5MB uncompressed, ~600KB gzipped
- **JS Bindings**: ~50KB
- **React Package**: ~15KB

### Runtime Performance
- **FPS**: 60 (browser-capped)
- **Memory**: ~50-100MB
- **Load Time**: 2-3 seconds on broadband

---

## Browser Support

| Browser | Version | Support |
|---------|---------|---------|
| Chrome | 79+ | ✅ Full |
| Firefox | 79+ | ✅ Full |
| Safari | 15.4+ | ✅ Full |
| Edge | 79+ | ✅ Full |

**Requirements:**
- WebGL2 or WebGPU
- WebAssembly support
- ES2022+ JavaScript

---

## Troubleshooting

### WASM Not Loading

```tsx
// Check browser console for errors
// Ensure WASM files are accessible
// Verify Next.js config allows WASM

// next.config.js
module.exports = {
  webpack: (config) => {
    config.experiments = {
      ...config.experiments,
      asyncWebAssembly: true,
    };
    return config;
  },
};
```

### Performance Issues

```tsx
// Enable performance monitoring
<Scene3D showPerformance={true} />

// Check FPS in overlay
// Reduce scene complexity if needed
// Ensure GPU acceleration is enabled
```

### Canvas Not Visible

```tsx
// Ensure parent has dimensions
<div style={{ width: '100vw', height: '100vh' }}>
  <Scene3D scene="mindspace" />
</div>

// Or use CSS classes
<div className="w-full h-screen">
  <Scene3D scene="mindspace" />
</div>
```

---

## Development

### Local Testing

```bash
# Start Weave dev server
pnpm dev

# Navigate to page with Scene3D component
# Watch for console logs
```

### Debug Mode

```tsx
<Scene3D
  scene="mindspace"
  showPerformance={true}
  onLoad={() => console.log('Loaded!')}
  onError={(err) => console.error('Error:', err)}
/>
```

---

## License

MIT OR Apache-2.0 (same as Bevy/EustressEngine)

---

## Support

**Author**: WeaveSolutions  
**Email**: admin@weave.solutions  
**Website**: https://mckaleolson.com
