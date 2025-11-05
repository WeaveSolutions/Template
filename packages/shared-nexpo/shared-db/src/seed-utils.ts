import type { PrismaClient } from '@prisma/client';
import { hash } from 'bcryptjs';
import { v4 as uuidv4 } from 'uuid';
import { db } from './client';

// Type definitions for improved type safety
type PrismaTransaction = Omit<PrismaClient, '$connect' | '$disconnect' | '$on' | '$transaction' | '$use' | '$extends'>;
interface User {
  id: string;
  email: string;
  name: string;
  role: string;
  password: string;
  emailVerified: Date;
}

/**
 * Seed the database with initial data
 */
export async function seedDatabase() {
  console.log('🌱 Seeding database...');
  
  // Create a transaction to ensure all operations succeed or fail together
  // @ts-ignore - db.prisma exists at runtime but TypeScript doesn't recognize it
  await db.prisma.$transaction(async (tx: PrismaTransaction) => {
    // Clear existing data (be careful with this in production!)
    if (process.env.NODE_ENV !== 'production') {
      const models = Object.keys(tx).filter(
        (key) => !['_', '$'].includes(key[0])
      );
      
      for (const model of models) {
        try {
          // Skip if the model doesn't have deleteMany
          if (typeof tx[model].deleteMany === 'function') {
            await tx[model].deleteMany({});
          }
        } catch (error) {
          console.warn(`Failed to clear ${model}:`, error);
        }
      }
    }

    // Create admin user
    const adminUser = await tx.user.upsert({
      where: { email: 'admin@example.com' },
      update: {},
      create: {
        id: uuidv4(),
        email: 'admin@example.com',
        name: 'Admin User',
        role: 'ADMIN',
        password: await hash('admin123', 12),
        emailVerified: new Date(),
      },
    });

    console.log(`✅ Created admin user: ${adminUser.email}`);

    // Create sample posts
    const posts = await Promise.all(
      Array(10)
        .fill(0)
        .map((_, i) =>
          tx.post.create({
            data: {
              title: `Sample Post ${i + 1}`,
              content: `This is a sample post #${i + 1}`,
              published: true,
              authorId: adminUser.id,
            },
          })
        )
    );

    console.log(`✅ Created ${posts.length} sample posts`);
  });
  
  console.log('🌱 Database seeded successfully!');
}

/**
 * Seed the database if this module is run directly
 */
if (require.main === module) {
  seedDatabase()
    .catch((error) => {
      console.error('❌ Error seeding database:', error);
      // Using a safer way to terminate the process
      console.error('Process will terminate with error');
      // Give time for logs to flush before exiting
      setTimeout(() => {
        // Use the correct type for process.exit
        (process as NodeJS.Process).exit(1);
      }, 100);
    })
    .finally(async () => {
      await db.prisma.$disconnect();
    });
}

// Export types for the seed data
export interface SeedData {
  users: any[];
  posts: any[];
  // Add more collections as needed
}

/**
 * Helper to generate mock data for testing
 */
export function generateMockData(): SeedData {
  const users = Array(10)
    .fill(0)
    .map((_, i) => ({
      id: uuidv4(),
      email: `user${i + 1}@example.com`,
      name: `User ${i + 1}`,
      role: i === 0 ? 'ADMIN' : 'USER',
      password: 'password', // Should be hashed in a real scenario
      emailVerified: new Date(),
    }));

  const posts = users.flatMap((user: User) =>
    Array(3).fill(0).map((_, i) => ({
      title: `Post by ${user.name} #${i + 1}`,
      content: `This is post #${i + 1} by ${user.name}`,
      published: true,
      authorId: user.id,
    }))
  );

  return { users, posts };
}

/**
 * Load seed data into the database
 */
export async function loadSeedData(data: SeedData) {
  console.log('🌱 Loading seed data...');
  
  await db.prisma.$transaction([
    // Clear existing data
    db.prisma.post.deleteMany({}),
    db.prisma.user.deleteMany({}),
    
    // Insert seed data
    db.prisma.user.createMany({
      data: data.users.map(user => ({
        ...user,
        password: 'hashed_password_here', // Make sure to hash passwords in a real scenario
      })),
    }),
    db.prisma.post.createMany({ data: data.posts }),
  ]);
  
  console.log('✅ Seed data loaded successfully!');
}
