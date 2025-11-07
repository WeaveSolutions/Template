# Database Schema Template

## User Schema

```typescript
// Prisma Example
model User {
  id            String    @id @default(uuid())
  email         String    @unique
  name          String?
  passwordHash  String?
  role          Role      @default(USER)
  
  // OAuth
  oauthProvider String?
  oauthId       String?
  
  // Timestamps
  createdAt     DateTime  @default(now())
  updatedAt     DateTime  @updatedAt
  lastLoginAt   DateTime?
  
  // Relations
  profile       Profile?
  posts         Post[]
  
  @@index([email])
}

model Profile {
  id         String   @id @default(uuid())
  bio        String?
  avatar     String?
  website    String?
  
  userId     String   @unique
  user       User     @relation(fields: [userId], references: [id], onDelete: Cascade)
  
  createdAt  DateTime @default(now())
  updatedAt  DateTime @updatedAt
}

model Post {
  id          String   @id @default(uuid())
  title       String
  content     String
  published   Boolean  @default(false)
  
  authorId    String
  author      User     @relation(fields: [authorId], references: [id], onDelete: Cascade)
  
  createdAt   DateTime @default(now())
  updatedAt   DateTime @updatedAt
  
  @@index([authorId])
  @@index([published])
}

enum Role {
  USER
  ADMIN
  MODERATOR
}
```

## SQL Migration Example

```sql
CREATE TABLE users (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  email VARCHAR(255) UNIQUE NOT NULL,
  name VARCHAR(255),
  password_hash VARCHAR(255),
  role VARCHAR(50) DEFAULT 'USER',
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX idx_users_email ON users(email);

CREATE TABLE profiles (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id UUID UNIQUE REFERENCES users(id) ON DELETE CASCADE,
  bio TEXT,
  avatar VARCHAR(500),
  created_at TIMESTAMP DEFAULT NOW()
);
```

## Best Practices
1. Use UUIDs for primary keys
2. Add indexes on foreign keys
3. Use CASCADE for related deletions
4. Add timestamps (createdAt, updatedAt)
5. Normalize data appropriately
6. Use constraints and validations
7. Plan for soft deletes if needed
