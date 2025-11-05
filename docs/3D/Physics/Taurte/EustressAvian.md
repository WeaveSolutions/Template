# Eustress Avian Physics: Tauri Svelte Integration

## Overview
Eustress Avian is a modern physics engine built specifically for the Eustress game engine, providing high-performance 2D and 3D physics simulation capabilities for the Nexpo platform through Tauri and Svelte. This integration enables realistic physics-based interactions, simulations, and immersive experiences with native performance and reactive UI controls.

## Table of Contents
1. [Physics Architecture](#physics-architecture)
2. [Tauri Integration](#tauri-integration)
3. [Svelte Physics Controls](#svelte-physics-controls)
4. [Rigid Body Dynamics](#rigid-body-dynamics)
5. [Collision Detection](#collision-detection)
6. [Joints and Constraints](#joints-and-constraints)
7. [Character Controllers](#character-controllers)
8. [Performance Optimization](#performance-optimization)
9. [Real-time Synchronization](#real-time-synchronization)
10. [Development Tools](#development-tools)

## Physics Architecture

### Core Components
- **Physics World**: Central simulation environment in Tauri backend
- **Rigid Bodies**: Dynamic, kinematic, and static physics objects
- **Colliders**: Shape definitions for collision detection and response
- **Joints**: Mechanical connections between rigid bodies
- **Force Systems**: Gravity, custom forces, and impulse application
- **Spatial Queries**: Ray casting and intersection testing
- **Svelte Controls**: Reactive UI for physics parameter manipulation

### Integration Benefits
- **Native Performance**: Direct hardware access through Tauri
- **Reactive UI**: Svelte provides real-time physics parameter controls
- **Cross-Platform**: Consistent physics behavior across desktop platforms
- **Memory Efficient**: Optimized for real-time applications
- **Hot Reloading**: Live physics parameter updates during development

## Tauri Integration

### Physics Backend Commands
```rust
// src-tauri/src/physics.rs
use tauri::{command, State, Emitter};
use Eustress::prelude::*;
use Eustress_avian3d::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(Default)]
struct PhysicsState {
    world_gravity: Vec3,
    time_scale: f32,
    debug_mode: bool,
}

#[derive(Serialize, Deserialize, Clone)]
struct PhysicsObject {
    id: String,
    position: [f32; 3],
    rotation: [f32; 4],
    velocity: [f32; 3],
    angular_velocity: [f32; 3],
    mass: f32,
    object_type: String,
}

#[command]
async fn set_world_gravity(
    state: State<'_, PhysicsState>,
    gravity: [f32; 3],
) -> Result<(), String> {
    // Update physics world gravity
    println!("Setting world gravity to: {:?}", gravity);
    Ok(())
}

#[command]
async fn spawn_physics_object(
    object_type: String,
    position: [f32; 3],
    mass: f32,
) -> Result<String, String> {
    let object_id = format!("obj_{}", uuid::Uuid::new_v4().to_simple());
    
    // Spawn physics object in Eustress world
    println!("Spawning {} at {:?} with mass {}", object_type, position, mass);
    
    Ok(object_id)
}

#[command]
async fn apply_force_to_object(
    object_id: String,
    force: [f32; 3],
    position: Option<[f32; 3]>,
) -> Result<(), String> {
    // Apply force to specific physics object
    println!("Applying force {:?} to object {}", force, object_id);
    Ok(())
}

#[command]
async fn get_physics_objects() -> Result<Vec<PhysicsObject>, String> {
    // Return current state of all physics objects
    Ok(vec![PhysicsObject {
        id: "example".to_string(),
        position: [0.0, 5.0, 0.0],
        rotation: [0.0, 0.0, 0.0, 1.0],
        velocity: [0.0, -1.0, 0.0],
        angular_velocity: [0.0, 0.0, 0.0],
        mass: 1.0,
        object_type: "cube".to_string(),
    }])
}

#[command]
async fn set_physics_debug_mode(enabled: bool) -> Result<(), String> {
    println!("Physics debug mode: {}", enabled);
    Ok(())
}
```

### Eustress Physics Plugin Integration
```rust
// src-tauri/src/Eustress_physics.rs
use Eustress::prelude::*;
use Eustress_avian3d::prelude::*;
use tauri::{AppHandle, Emitter};

pub struct NexpoPhysicsPlugin {
    pub app_handle: AppHandle,
}

impl Plugin for NexpoPhysicsPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(PhysicsPlugins::default())
            .insert_resource(Gravity(Vec3::new(0.0, -9.81, 0.0)))
            .add_event::<PhysicsUpdateEvent>()
            .add_systems(Startup, setup_physics_world)
            .add_systems(Update, (
                physics_object_spawner,
                force_application_system,
                physics_state_sync,
                emit_physics_events,
            ));
    }
}

#[derive(Event)]
struct PhysicsUpdateEvent {
    object_id: String,
    position: Vec3,
    rotation: Quat,
    velocity: Vec3,
}

fn setup_physics_world(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    // Create ground plane
    commands.spawn((
        PbrBundle {
            mesh: meshes.add(Plane3d::default().mesh().size(20.0, 20.0)),
            material: materials.add(Color::rgb(0.3, 0.5, 0.3).into()),
            ..default()
        },
        RigidBody::Static,
        Collider::cuboid(10.0, 0.1, 10.0),
        Name::new("Ground"),
    ));
    
    // Create some demo objects
    spawn_demo_objects(&mut commands, &mut meshes, &mut materials);
}

fn spawn_demo_objects(
    commands: &mut Commands,
    meshes: &mut ResMut<Assets<Mesh>>,
    materials: &mut ResMut<Assets<StandardMaterial>>,
) {
    // Cube
    commands.spawn((
        PbrBundle {
            mesh: meshes.add(Mesh::from(Cube { size: 1.0 })),
            material: materials.add(Color::rgb(0.8, 0.2, 0.2).into()),
            transform: Transform::from_xyz(-2.0, 5.0, 0.0),
            ..default()
        },
        RigidBody::Dynamic,
        Collider::cuboid(0.5, 0.5, 0.5),
        Mass(1.0),
        Restitution::coefficient(0.7),
        Friction::coefficient(0.8),
        PhysicsObjectId("demo-cube".to_string()),
    ));
    
    // Sphere
    commands.spawn((
        PbrBundle {
            mesh: meshes.add(Mesh::from(Sphere::new(0.5))),
            material: materials.add(Color::rgb(0.2, 0.8, 0.2).into()),
            transform: Transform::from_xyz(0.0, 6.0, 0.0),
            ..default()
        },
        RigidBody::Dynamic,
        Collider::ball(0.5),
        Mass(2.0),
        Restitution::coefficient(0.9),
        PhysicsObjectId("demo-sphere".to_string()),
    ));
}

#[derive(Component)]
struct PhysicsObjectId(String);

fn physics_state_sync(
    physics_objects: Query<(
        &PhysicsObjectId,
        &Transform,
        &LinearVelocity,
        &AngularVelocity,
    )>,
    app_handle: Res<AppHandle>,
    mut physics_events: EventWriter<PhysicsUpdateEvent>,
    time: Res<Time>,
) {
    // Emit physics updates to Svelte frontend every few frames
    if time.elapsed_seconds() % 0.1 < time.delta_seconds() {
        for (id, transform, linear_vel, angular_vel) in physics_objects.iter() {
            let physics_data = serde_json::json!({
                "id": id.0,
                "position": [transform.translation.x, transform.translation.y, transform.translation.z],
                "rotation": [transform.rotation.x, transform.rotation.y, transform.rotation.z, transform.rotation.w],
                "velocity": [linear_vel.x, linear_vel.y, linear_vel.z],
                "angularVelocity": [angular_vel.x, angular_vel.y, angular_vel.z]
            });
            
            let _ = app_handle.emit("physics-update", physics_data);
        }
    }
}
```

## Svelte Physics Controls

### Physics State Management
```typescript
// src/lib/stores/physicsStore.ts
import { writable, derived } from 'svelte/store';
import { invoke } from '@tauri-apps/api/tauri';
import { listen } from '@tauri-apps/api/event';

interface PhysicsObject {
  id: string;
  position: [number, number, number];
  rotation: [number, number, number, number];
  velocity: [number, number, number];
  angularVelocity: [number, number, number];
  mass: number;
  objectType: string;
}

interface PhysicsWorld {
  gravity: [number, number, number];
  timeScale: number;
  debugMode: boolean;
  paused: boolean;
}

// Physics state stores
export const physicsWorld = writable<PhysicsWorld>({
  gravity: [0, -9.81, 0],
  timeScale: 1.0,
  debugMode: false,
  paused: false
});

export const physicsObjects = writable<PhysicsObject[]>([]);

// Derived stores
export const totalKineticEnergy = derived(physicsObjects, $objects => {
  return $objects.reduce((total, obj) => {
    const speed = Math.sqrt(
      obj.velocity[0] ** 2 + obj.velocity[1] ** 2 + obj.velocity[2] ** 2
    );
    return total + 0.5 * obj.mass * speed ** 2;
  }, 0);
});

// Physics actions
export const physicsActions = {
  async updateGravity(gravity: [number, number, number]): Promise<void> {
    try {
      await invoke('set_world_gravity', { gravity });
      physicsWorld.update(world => ({ ...world, gravity }));
    } catch (error) {
      console.error('Failed to update gravity:', error);
    }
  },
  
  async spawnObject(
    objectType: string,
    position: [number, number, number],
    mass: number
  ): Promise<void> {
    try {
      const objectId = await invoke<string>('spawn_physics_object', {
        objectType,
        position,
        mass
      });
      console.log(`Spawned object: ${objectId}`);
    } catch (error) {
      console.error('Failed to spawn object:', error);
    }
  },
  
  async applyForce(
    objectId: string,
    force: [number, number, number],
    position?: [number, number, number]
  ): Promise<void> {
    try {
      await invoke('apply_force_to_object', {
        objectId,
        force,
        position
      });
    } catch (error) {
      console.error('Failed to apply force:', error);
    }
  },
  
  async toggleDebugMode(): Promise<void> {
    const currentWorld = get(physicsWorld);
    const newDebugMode = !currentWorld.debugMode;
    
    try {
      await invoke('set_physics_debug_mode', { enabled: newDebugMode });
      physicsWorld.update(world => ({ ...world, debugMode: newDebugMode }));
    } catch (error) {
      console.error('Failed to toggle debug mode:', error);
    }
  },
  
  async refreshObjects(): Promise<void> {
    try {
      const objects = await invoke<PhysicsObject[]>('get_physics_objects');
      physicsObjects.set(objects);
    } catch (error) {
      console.error('Failed to refresh objects:', error);
    }
  }
};

// Listen for physics updates from Tauri
listen<any>('physics-update', (event) => {
  const objectData = event.payload;
  
  physicsObjects.update(objects => {
    const existingIndex = objects.findIndex(obj => obj.id === objectData.id);
    
    if (existingIndex >= 0) {
      objects[existingIndex] = {
        ...objects[existingIndex],
        position: objectData.position,
        rotation: objectData.rotation,
        velocity: objectData.velocity,
        angularVelocity: objectData.angularVelocity
      };
    }
    
    return objects;
  });
});
```

### Physics Control Panel Component
```svelte
<!-- src/lib/components/PhysicsControlPanel.svelte -->
<script lang="ts">
  import { onMount } from 'svelte';
  import {
    physicsWorld,
    physicsObjects,
    totalKineticEnergy,
    physicsActions
  } from '../stores/physicsStore';
  
  let selectedObjectType = 'cube';
  let spawnPosition = [0, 5, 0] as [number, number, number];
  let objectMass = 1.0;
  let forceVector = [0, 10, 0] as [number, number, number];
  let selectedObjectId = '';
  
  onMount(() => {
    physicsActions.refreshObjects();
  });
  
  async function handleSpawnObject() {
    await physicsActions.spawnObject(selectedObjectType, spawnPosition, objectMass);
    await physicsActions.refreshObjects();
  }
  
  async function handleApplyForce() {
    if (selectedObjectId) {
      await physicsActions.applyForce(selectedObjectId, forceVector);
    }
  }
  
  async function handleGravityChange() {
    await physicsActions.updateGravity($physicsWorld.gravity);
  }
</script>

<div class="physics-control-panel">
  <h2>Physics Control Panel</h2>
  
  <!-- World Settings -->
  <div class="section">
    <h3>World Settings</h3>
    
    <div class="control-group">
      <label>Gravity:</label>
      <div class="vector-input">
        <input 
          type="number" 
          bind:value={$physicsWorld.gravity[0]} 
          on:change={handleGravityChange}
          step="0.1"
        />
        <input 
          type="number" 
          bind:value={$physicsWorld.gravity[1]} 
          on:change={handleGravityChange}
          step="0.1"
        />
        <input 
          type="number" 
          bind:value={$physicsWorld.gravity[2]} 
          on:change={handleGravityChange}
          step="0.1"
        />
      </div>
    </div>
    
    <div class="control-group">
      <label>
        <input 
          type="checkbox" 
          bind:checked={$physicsWorld.debugMode}
          on:change={physicsActions.toggleDebugMode}
        />
        Debug Mode
      </label>
    </div>
  </div>
  
  <!-- Object Spawning -->
  <div class="section">
    <h3>Spawn Objects</h3>
    
    <div class="control-group">
      <label>Object Type:</label>
      <select bind:value={selectedObjectType}>
        <option value="cube">Cube</option>
        <option value="sphere">Sphere</option>
        <option value="cylinder">Cylinder</option>
        <option value="capsule">Capsule</option>
      </select>
    </div>
    
    <div class="control-group">
      <label>Position:</label>
      <div class="vector-input">
        <input type="number" bind:value={spawnPosition[0]} step="0.5" />
        <input type="number" bind:value={spawnPosition[1]} step="0.5" />
        <input type="number" bind:value={spawnPosition[2]} step="0.5" />
      </div>
    </div>
    
    <div class="control-group">
      <label>Mass:</label>
      <input type="number" bind:value={objectMass} min="0.1" max="10" step="0.1" />
    </div>
    
    <button on:click={handleSpawnObject}>Spawn Object</button>
  </div>
  
  <!-- Force Application -->
  <div class="section">
    <h3>Apply Forces</h3>
    
    <div class="control-group">
      <label>Target Object:</label>
      <select bind:value={selectedObjectId}>
        <option value="">Select object...</option>
        {#each $physicsObjects as obj (obj.id)}
          <option value={obj.id}>{obj.id} ({obj.objectType})</option>
        {/each}
      </select>
    </div>
    
    <div class="control-group">
      <label>Force Vector:</label>
      <div class="vector-input">
        <input type="number" bind:value={forceVector[0]} step="1" />
        <input type="number" bind:value={forceVector[1]} step="1" />
        <input type="number" bind:value={forceVector[2]} step="1" />
      </div>
    </div>
    
    <button on:click={handleApplyForce} disabled={!selectedObjectId}>
      Apply Force
    </button>
  </div>
  
  <!-- Physics Info -->
  <div class="section">
    <h3>Physics Information</h3>
    
    <div class="info-display">
      <div class="info-item">
        <span class="label">Objects:</span>
        <span class="value">{$physicsObjects.length}</span>
      </div>
      
      <div class="info-item">
        <span class="label">Total Kinetic Energy:</span>
        <span class="value">{$totalKineticEnergy.toFixed(2)} J</span>
      </div>
      
      <div class="info-item">
        <span class="label">Gravity:</span>
        <span class="value">
          [{$physicsWorld.gravity[0].toFixed(1)}, 
           {$physicsWorld.gravity[1].toFixed(1)}, 
           {$physicsWorld.gravity[2].toFixed(1)}]
        </span>
      </div>
    </div>
  </div>
</div>

<style>
  .physics-control-panel {
    background: #2a2a2a;
    color: white;
    padding: 1rem;
    border-radius: 8px;
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    max-width: 400px;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
  }
  
  .section {
    margin-bottom: 1.5rem;
    padding-bottom: 1rem;
    border-bottom: 1px solid #444;
  }
  
  .section:last-child {
    border-bottom: none;
  }
  
  h2 {
    margin: 0 0 1rem 0;
    color: #61dafb;
    font-size: 1.2rem;
  }
  
  h3 {
    margin: 0 0 0.75rem 0;
    color: #ffd700;
    font-size: 1rem;
  }
  
  .control-group {
    margin-bottom: 0.75rem;
  }
  
  label {
    display: block;
    margin-bottom: 0.25rem;
    font-size: 0.9rem;
    color: #ccc;
  }
  
  input, select {
    background: #333;
    border: 1px solid #555;
    color: white;
    padding: 0.4rem;
    border-radius: 4px;
    font-size: 0.9rem;
  }
  
  input:focus, select:focus {
    border-color: #61dafb;
    outline: none;
  }
  
  .vector-input {
    display: flex;
    gap: 0.25rem;
  }
  
  .vector-input input {
    flex: 1;
    min-width: 0;
  }
  
  button {
    background: #61dafb;
    color: #1a1a1a;
    border: none;
    padding: 0.5rem 1rem;
    border-radius: 4px;
    cursor: pointer;
    font-weight: bold;
    transition: background 0.2s;
  }
  
  button:hover:not(:disabled) {
    background: #4fa8c5;
  }
  
  button:disabled {
    background: #555;
    color: #999;
    cursor: not-allowed;
  }
  
  .info-display {
    font-size: 0.9rem;
  }
  
  .info-item {
    display: flex;
    justify-content: space-between;
    margin-bottom: 0.5rem;
  }
  
  .label {
    color: #ccc;
  }
  
  .value {
    color: #61dafb;
    font-weight: bold;
  }
</style>
```

## Performance Optimization

### Physics Configuration
```rust
// Optimize physics for Tauri applications
fn configure_physics_performance(app: &mut App) {
    app.insert_resource(PhysicsTimestep::Fixed {
        dt: 1.0 / 60.0, // 60 FPS physics
        substeps: 4,    // Stability vs performance
    })
    .insert_resource(SpatialQueryPipeline {
        query_margin: 0.01,
        prediction_distance: 0.02,
    });
}
```

### Level-of-Detail System
```rust
#[derive(Component)]
struct PhysicsLOD {
    high_detail_distance: f32,
    medium_detail_distance: f32,
}

fn physics_lod_system(
    mut commands: Commands,
    camera_query: Query<&Transform, With<Camera>>,
    physics_objects: Query<(Entity, &Transform, &PhysicsLOD), Without<Camera>>,
) {
    let camera_pos = camera_query.single().translation;
    
    for (entity, transform, lod) in physics_objects.iter() {
        let distance = camera_pos.distance(transform.translation);
        
        if distance > lod.medium_detail_distance {
            // Reduce physics complexity for distant objects
            commands.entity(entity).insert(RigidBody::Static);
        } else if distance < lod.high_detail_distance {
            // Full physics for close objects
            commands.entity(entity).insert(RigidBody::Dynamic);
        }
    }
}
```

## Development Tools

### Physics Debug Visualization
```rust
#[cfg(debug_assertions)]
fn physics_debug_system(
    mut gizmos: Gizmos,
    colliders: Query<(&Transform, &Collider)>,
    debug_enabled: Res<PhysicsDebugConfig>,
) {
    if !debug_enabled.enabled {
        return;
    }
    
    for (transform, collider) in colliders.iter() {
        // Draw collider wireframes
        gizmos.rect(
            transform.translation,
            transform.rotation,
            Vec2::new(1.0, 1.0),
            Color::GREEN,
        );
    }
}
```

### Hot Reloading Physics Parameters
```svelte
<!-- Live physics parameter tuning -->
<script>
  import { onMount } from 'svelte';
  import { listen } from '@tauri-apps/api/event';
  
  let physicsParams = {
    gravity: -9.81,
    friction: 0.5,
    restitution: 0.7
  };
  
  // Watch for parameter changes and apply immediately
  $: updatePhysicsParams(physicsParams);
  
  async function updatePhysicsParams(params) {
    await invoke('update_physics_parameters', { params });
  }
</script>
```

Eustress Avian Physics with Tauri Svelte integration provides Nexpo with a powerful, performance-optimized physics simulation system that combines native backend processing with reactive frontend controls, enabling immersive and interactive experiences across desktop platforms.