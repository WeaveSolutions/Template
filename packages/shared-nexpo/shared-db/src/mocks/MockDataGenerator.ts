import { faker } from '@faker-js/faker';

/**
 * Central mock data generator for consistent test data across all Prisma providers
 * Uses Faker.js to generate realistic test data
 */
export class MockDataGenerator {
  /**
   * Generate a single user with realistic data
   */
  static generateUser(overrides: Partial<any> = {}) {
    return {
      id: faker.string.uuid(),
      email: faker.internet.email(),
      displayName: faker.person.fullName(),
      rank: faker.number.int({ min: 1, max: 100 }),
      createdAt: faker.date.past(),
      updatedAt: new Date(),
      deletedAt: null,
      ...overrides,
    };
  }

  /**
   * Generate a single account connection with realistic provider data
   */
  static generateAccount(overrides: Partial<any> = {}) {
    const providers = ['discord', 'github', 'slack', 'google', 'microsoft'];
    const provider = faker.helpers.arrayElement(providers);
    
    return {
      id: faker.string.uuid(),
      userId: faker.string.uuid(),
      provider,
      providerAccountId: faker.string.alphanumeric(10),
      accessToken: faker.string.alphanumeric(32),
      refreshToken: faker.string.alphanumeric(32),
      expiresAt: faker.date.future(),
      scope: this.generateScopeForProvider(provider),
      tokenType: 'Bearer',
      createdAt: faker.date.past(),
      updatedAt: new Date(),
      deletedAt: null,
      ...overrides,
    };
  }

  /**
   * Generate realistic OAuth scopes based on provider
   */
  private static generateScopeForProvider(provider: string): string {
    const scopes = {
      discord: 'identify email guilds',
      github: 'user:email read:org',
      slack: 'users:read channels:read',
      google: 'openid email profile',
      microsoft: 'openid email profile User.Read',
    };
    
    return scopes[provider as keyof typeof scopes] || 'openid email profile';
  }

  /**
   * Generate multiple users
   */
  static generateUsers(count: number = 10, overrides: Partial<any> = {}) {
    return Array.from({ length: count }, () => this.generateUser(overrides));
  }

  /**
   * Generate multiple accounts
   */
  static generateAccounts(count: number = 20, overrides: Partial<any> = {}) {
    return Array.from({ length: count }, () => this.generateAccount(overrides));
  }

  /**
   * Generate accounts for a specific user
   */
  static generateAccountsForUser(userId: string, count: number = 3) {
    return Array.from({ length: count }, () => 
      this.generateAccount({ userId })
    );
  }

  /**
   * Generate a complete user with linked accounts
   */
  static generateUserWithAccounts(accountCount: number = 2) {
    const user = this.generateUser();
    const accounts = this.generateAccountsForUser(user.id, accountCount);
    
    return {
      user,
      accounts,
    };
  }

  /**
   * Generate users with a specific rank range
   */
  static generateUsersWithRankRange(count: number, minRank: number, maxRank: number) {
    return Array.from({ length: count }, () => 
      this.generateUser({
        rank: faker.number.int({ min: minRank, max: maxRank })
      })
    );
  }

  /**
   * Generate test data for specific scenarios
   */
  static generateTestScenarios() {
    return {
      // High-rank users for admin testing
      admins: this.generateUsersWithRankRange(5, 90, 100),
      
      // Mid-rank users for regular user testing
      regularUsers: this.generateUsersWithRankRange(20, 20, 80),
      
      // New users for onboarding testing
      newUsers: this.generateUsersWithRankRange(10, 1, 10),
      
      // Users with multiple accounts for federated testing
      federatedUsers: Array.from({ length: 5 }, () => 
        this.generateUserWithAccounts(3)
      ),
      
      // Deleted users for soft delete testing
      deletedUsers: this.generateUsers(5, { 
        deletedAt: faker.date.past() 
      }),
    };
  }

  /**
   * Generate provider-specific test data
   */
  static generateProviderSpecificData(provider: string) {
    switch (provider.toLowerCase()) {
      case 'mongodb':
      case 'cosmosdb':
        return {
          users: this.generateUsers(10).map(user => ({
            ...user,
            _id: user.id, // MongoDB uses _id
          })),
          accounts: this.generateAccounts(20).map(account => ({
            ...account,
            _id: account.id,
          })),
        };
      
      case 'postgres':
      case 'sqlserver':
      case 'ibmcloud':
      default:
        return {
          users: this.generateUsers(10),
          accounts: this.generateAccounts(20),
        };
    }
  }

  /**
   * Clear all generated data (for testing cleanup)
   */
  static clearGeneratedData() {
    // Reset faker seed for consistent results in tests
    faker.seed(12345);
  }

  /**
   * Generate deterministic data for testing (with fixed seed)
   */
  static generateDeterministicData(seed: number = 12345) {
    faker.seed(seed);
    
    return {
      users: this.generateUsers(5),
      accounts: this.generateAccounts(10),
    };
  }
}

export default MockDataGenerator;
