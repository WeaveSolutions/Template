import { PrismaClient } from '@prisma/client';
import { hash } from 'bcryptjs';
import { v4 as uuidv4 } from 'uuid';
import { generateMockData, loadSeedData } from '../src/seed-utils';

const prisma = new PrismaClient();

async function main() {
  console.log('🌱 Starting database seeding...');
  
  // Generate mock data
  const mockData = generateMockData();
  
  // Hash passwords for users
  const hashedUsers = await Promise.all(
    mockData.users.map(async (user) => ({
      ...user,
      password: await hash(user.password, 12),
      emailVerified: new Date(),
    }))
  );
  
  // Load the seed data
  await loadSeedData({
    ...mockData,
    users: hashedUsers,
  });
  
  console.log('✅ Database seeded successfully!');
}

main()
  .catch((e) => {
    console.error('❌ Error seeding database:', e);
    process.exit(1);
  })
  .finally(async () => {
    await prisma.$disconnect();
  });
