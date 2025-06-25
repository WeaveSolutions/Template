-- Create users table
CREATE TABLE IF NOT EXISTS users (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  email TEXT UNIQUE NOT NULL,
  full_name TEXT,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Create profiles table
CREATE TABLE IF NOT EXISTS profiles (
  id UUID PRIMARY KEY REFERENCES users(id) ON DELETE CASCADE,
  bio TEXT,
  avatar_url TEXT,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Create settings table
CREATE TABLE IF NOT EXISTS settings (
  id UUID PRIMARY KEY REFERENCES users(id) ON DELETE CASCADE,
  language TEXT DEFAULT 'en',
  theme TEXT DEFAULT 'light',
  notifications BOOLEAN DEFAULT true,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
