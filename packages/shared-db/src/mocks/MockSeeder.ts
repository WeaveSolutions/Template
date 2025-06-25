import { MockDataGenerator } from './MockDataGenerator';
import { DatabaseClient } from '../DatabaseClient';
import { getActiveProvider, getEnabledProviders } from '../utils/providers';

/**
 * Central mock data seeder for all Prisma providers
 * Handles seeding and clearing test data across multiple database providers
 */
export class MockSeeder {
  /**
   * Seed users for a specific provider
   */
  static async seedUsers(provider: string, count: number = 50) {
    try {
      const db = await DatabaseClient.getInstance(provider);
      const users = MockDataGenerator.generateUsers(count);
      
      console.log(`🌱 Seeding ${count} users for ${provider}...`);
      
      if (provider === 'mongodb' || provider === 'cosmosdb') {
        // MongoDB/CosmosDB requires different handling for _id
        const mongoUsers = users.map(user => ({
          ...user,
          _id: user.id,
        }));
        
        await db.user.createMany({
          data: mongoUsers,
          skipDuplicates: true,
        });
      } else {
        await db.user.createMany({
          data: users,
          skipDuplicates: true,
        });
      }
      
      console.log(`✅ Seeded ${count} users for ${provider}`);
    } catch (error) {
      console.error(`❌ Error seeding users for ${provider}:`, error);
      throw error;
    }
  }

  /**
   * Seed accounts for a specific provider
   */
  static async seedAccounts(provider: string, count: number = 100) {
    try {
      const db = await DatabaseClient.getInstance(provider);
      
      // First, get some existing users to link accounts to
      const existingUsers = await db.user.findMany({ take: 10 });
      
      if (existingUsers.length === 0) {
        console.log(`⚠️  No users found for ${provider}, seeding users first...`);
        await this.seedUsers(provider, 10);
        const newUsers = await db.user.findMany({ take: 10 });
        return this.seedAccountsForUsers(provider, newUsers, count);
      }
      
      return this.seedAccountsForUsers(provider, existingUsers, count);
    } catch (error) {
      console.error(`❌ Error seeding accounts for ${provider}:`, error);
      throw error;
    }
  }

  /**
   * Seed accounts for specific users
   */
  private static async seedAccountsForUsers(provider: string, users: any[], totalCount: number) {
    const db = await DatabaseClient.getInstance(provider);
    const accounts = [];
    
    console.log(`🌱 Seeding ${totalCount} accounts for ${provider}...`);
    
    for (let i = 0; i < totalCount; i++) {
      const randomUser = users[Math.floor(Math.random() * users.length)];
      const account = MockDataGenerator.generateAccount({
        userId: randomUser.id,
      });
      accounts.push(account);
    }
    
    if (provider === 'mongodb' || provider === 'cosmosdb') {
      // MongoDB/CosmosDB requires different handling for _id
      const mongoAccounts = accounts.map(account => ({
        ...account,
        _id: account.id,
      }));
      
      await db.account.createMany({
        data: mongoAccounts,
        skipDuplicates: true,
      });
    } else {
      await db.account.createMany({
        data: accounts,
        skipDuplicates: true,
      });
    }
    
    console.log(`✅ Seeded ${totalCount} accounts for ${provider}`);
  }

  /**
   * Seed complete test scenarios for a provider
   */
  static async seedTestScenarios(provider: string) {
    console.log(`🎭 Seeding test scenarios for ${provider}...`);
    
    const scenarios = MockDataGenerator.generateTestScenarios();
    const db = await DatabaseClient.getInstance(provider);
    
    try {
      // Seed different user types
      console.log(`👑 Seeding ${scenarios.admins.length} admin users...`);
      await this.seedUsersData(db, provider, scenarios.admins);
      
      console.log(`👤 Seeding ${scenarios.regularUsers.length} regular users...`);
      await this.seedUsersData(db, provider, scenarios.regularUsers);
      
      console.log(`🆕 Seeding ${scenarios.newUsers.length} new users...`);
      await this.seedUsersData(db, provider, scenarios.newUsers);
      
      console.log(`🗑️ Seeding ${scenarios.deletedUsers.length} deleted users...`);
      await this.seedUsersData(db, provider, scenarios.deletedUsers);
      
      // Seed federated users with accounts
      console.log(`🔗 Seeding ${scenarios.federatedUsers.length} federated users...`);
      for (const { user, accounts } of scenarios.federatedUsers) {
        await this.seedUsersData(db, provider, [user]);
        await this.seedAccountsData(db, provider, accounts);
      }
      
      console.log(`🎉 Test scenarios seeded successfully for ${provider}`);
    } catch (error) {
      console.error(`❌ Error seeding test scenarios for ${provider}:`, error);
      throw error;
    }
  }

  /**
   * Helper method to seed user data with provider-specific handling
   */
  private static async seedUsersData(db: any, provider: string, users: any[]) {
    if (provider === 'mongodb' || provider === 'cosmosdb') {
      const mongoUsers = users.map(user => ({
        ...user,
        _id: user.id,
      }));
      
      await db.user.createMany({
        data: mongoUsers,
        skipDuplicates: true,
      });
    } else {
      await db.user.createMany({
        data: users,
        skipDuplicates: true,
      });
    }
  }

  /**
   * Helper method to seed account data with provider-specific handling
   */
  private static async seedAccountsData(db: any, provider: string, accounts: any[]) {
    if (provider === 'mongodb' || provider === 'cosmosdb') {
      const mongoAccounts = accounts.map(account => ({
        ...account,
        _id: account.id,
      }));
      
      await db.account.createMany({
        data: mongoAccounts,
        skipDuplicates: true,
      });
    } else {
      await db.account.createMany({
        data: accounts,
        skipDuplicates: true,
      });
    }
  }

  /**
   * Seed all data for a provider (users + accounts + scenarios)
   */
  static async seedAll(provider: string, options: {
    userCount?: number;
    accountCount?: number;
    includeScenarios?: boolean;
  } = {}) {
    const {
      userCount = 50,
      accountCount = 100,
      includeScenarios = true,
    } = options;
    
    console.log(`🌱 Starting complete data seeding for ${provider}...`);
    
    try {
      // Clear existing data first
      await this.clearAll(provider);
      
      // Seed basic data
      await this.seedUsers(provider, userCount);
      await this.seedAccounts(provider, accountCount);
      
      // Seed test scenarios if requested
      if (includeScenarios) {
        await this.seedTestScenarios(provider);
      }
      
      console.log(`🎉 Complete data seeding finished for ${provider}`);
    } catch (error) {
      console.error(`❌ Error in complete seeding for ${provider}:`, error);
      throw error;
    }
  }

  /**
   * Seed data for all enabled providers
   */
  static async seedAllProviders(options: {
    userCount?: number;
    accountCount?: number;
    includeScenarios?: boolean;
  } = {}) {
    const enabledProviders = getEnabledProviders();
    
    console.log(`🌍 Seeding data for all enabled providers: ${enabledProviders.join(', ')}`);
    
    for (const provider of enabledProviders) {
      try {
        await this.seedAll(provider, options);
      } catch (error) {
        console.error(`❌ Failed to seed ${provider}, continuing with next provider...`);
      }
    }
    
    console.log(`🎊 Finished seeding all enabled providers!`);
  }

  /**
   * Clear all data for a specific provider
   */
  static async clearAll(provider: string) {
    try {
      const db = await DatabaseClient.getInstance(provider);
      
      console.log(`🧹 Clearing all data for ${provider}...`);
      
      // Delete in correct order (accounts first due to foreign key constraints)
      await db.account.deleteMany({});
      await db.user.deleteMany({});
      
      console.log(`✅ Data cleared for ${provider}`);
    } catch (error) {
      console.error(`❌ Error clearing data for ${provider}:`, error);
      throw error;
    }
  }

  /**
   * Clear data for all enabled providers
   */
  static async clearAllProviders() {
    const enabledProviders = getEnabledProviders();
    
    console.log(`🧹 Clearing data for all enabled providers: ${enabledProviders.join(', ')}`);
    
    for (const provider of enabledProviders) {
      try {
        await this.clearAll(provider);
      } catch (error) {
        console.error(`❌ Failed to clear ${provider}, continuing with next provider...`);
      }
    }
    
    console.log(`✨ Finished clearing all enabled providers!`);
  }

  /**
   * Get data statistics for a provider
   */
  static async getStats(provider: string) {
    try {
      const db = await DatabaseClient.getInstance(provider);
      
      const userCount = await db.user.count();
      const accountCount = await db.account.count();
      const deletedUserCount = await db.user.count({
        where: { deletedAt: { not: null } }
      });
      
      return {
        provider,
        users: userCount,
        accounts: accountCount,
        deletedUsers: deletedUserCount,
        activeUsers: userCount - deletedUserCount,
      };
    } catch (error) {
      console.error(`❌ Error getting stats for ${provider}:`, error);
      return {
        provider,
        users: 0,
        accounts: 0,
        deletedUsers: 0,
        activeUsers: 0,
        error: error.message,
      };
    }
  }

  /**
   * Get statistics for all enabled providers
   */
  static async getAllStats() {
    const enabledProviders = getEnabledProviders();
    const stats = [];
    
    for (const provider of enabledProviders) {
      const providerStats = await this.getStats(provider);
      stats.push(providerStats);
    }
    
    return stats;
  }

  /**
   * Validate data consistency across providers
   */
  static async validateDataConsistency() {
    const stats = await this.getAllStats();
    
    console.log('\n📊 Data Consistency Report:');
    console.log('═'.repeat(50));
    
    stats.forEach(stat => {
      if (stat.error) {
        console.log(`❌ ${stat.provider}: ERROR - ${stat.error}`);
      } else {
        console.log(`✅ ${stat.provider}: ${stat.users} users, ${stat.accounts} accounts`);
      }
    });
    
    console.log('═'.repeat(50));
    
    return stats;
  }
}

export default MockSeeder;
