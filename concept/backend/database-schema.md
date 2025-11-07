# Database Schema Template

## User Schema

Core user model should include:

**User Table:**
- `id` (UUID, primary key)
- `email` (string, unique, indexed)
- `name` (string, optional)
- `passwordHash` (string, optional for OAuth users)
- `role` (enum: USER, ADMIN, MODERATOR)
- `oauthProvider` (string, optional)
- `oauthId` (string, optional)
- `createdAt`, `updatedAt`, `lastLoginAt` (timestamps)

**Profile Table (1:1 with User):**
- `id` (UUID, primary key)
- `bio`, `avatar`, `website` (optional strings)
- `userId` (foreign key, unique)
- `createdAt`, `updatedAt` (timestamps)

**Post Table (Many:1 with User):**
- `id` (UUID, primary key)
- `title`, `content` (strings)
- `published` (boolean, default false)
- `authorId` (foreign key, indexed)
- `createdAt`, `updatedAt` (timestamps)

## SQL Migration Guidelines

**Create User Table:**
- Use UUID for primary keys
- Set email as UNIQUE and NOT NULL
- Add default values where appropriate
- Include created_at and updated_at timestamps
- Create index on email for faster lookups

**Create Profile Table:**
- Link to users via user_id foreign key
- Set user_id as UNIQUE for 1:1 relationship
- Use CASCADE on delete to remove orphaned profiles
- Store optional bio, avatar URL, and website

**Create Post Table:**
- Link to users via author_id foreign key
- Add indexes on author_id and published status
- Use CASCADE delete to remove user's posts

## Best Practices
1. Use UUIDs for primary keys
2. Add indexes on foreign keys
3. Use CASCADE for related deletions
4. Add timestamps (createdAt, updatedAt)
5. Normalize data appropriately
6. Use constraints and validations
7. Plan for soft deletes if needed
