-- Seed data for users table
INSERT INTO users (email, full_name, created_at, updated_at)
VALUES 
  ('user1@example.com', 'User One', NOW(), NOW()),
  ('user2@example.com', 'User Two', NOW(), NOW()),
  ('user3@example.com', 'User Three', NOW(), NOW())
ON CONFLICT (email) DO NOTHING;

-- Seed data for profiles table
INSERT INTO profiles (id, bio, avatar_url, created_at, updated_at)
SELECT id, 'I am user one.', 'https://example.com/avatar1.jpg', NOW(), NOW() FROM users WHERE email = 'user1@example.com'
ON CONFLICT (id) DO NOTHING;

INSERT INTO profiles (id, bio, avatar_url, created_at, updated_at)
SELECT id, 'I am user two.', 'https://example.com/avatar2.jpg', NOW(), NOW() FROM users WHERE email = 'user2@example.com'
ON CONFLICT (id) DO NOTHING;

INSERT INTO profiles (id, bio, avatar_url, created_at, updated_at)
SELECT id, 'I am user three.', 'https://example.com/avatar3.jpg', NOW(), NOW() FROM users WHERE email = 'user3@example.com'
ON CONFLICT (id) DO NOTHING;

-- Seed data for settings table
INSERT INTO settings (id, language, theme, notifications, created_at, updated_at)
SELECT id, 'en', 'light', true, NOW(), NOW() FROM users WHERE email = 'user1@example.com'
ON CONFLICT (id) DO NOTHING;

INSERT INTO settings (id, language, theme, notifications, created_at, updated_at)
SELECT id, 'es', 'dark', false, NOW(), NOW() FROM users WHERE email = 'user2@example.com'
ON CONFLICT (id) DO NOTHING;

INSERT INTO settings (id, language, theme, notifications, created_at, updated_at)
SELECT id, 'fr', 'light', true, NOW(), NOW() FROM users WHERE email = 'user3@example.com'
ON CONFLICT (id) DO NOTHING;
