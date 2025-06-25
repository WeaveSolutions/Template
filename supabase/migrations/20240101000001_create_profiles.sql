-- Create profiles table
CREATE TABLE IF NOT EXISTS public.profiles (
  id UUID REFERENCES auth.users(id) ON DELETE CASCADE PRIMARY KEY,
  updated_at TIMESTAMPTZ,
  username TEXT UNIQUE,
  full_name TEXT,
  avatar_url TEXT,
  email TEXT,
  
  CONSTRAINT username_length CHECK (char_length(username) >= 3)
);

-- Enable RLS (Row Level Security)
ALTER TABLE public.profiles ENABLE ROW LEVEL SECURITY;

-- Create policies
CREATE POLICY "Public profiles are viewable by everyone." 
  ON public.profiles FOR SELECT 
  USING (true);

CREATE POLICY "Users can update own profile." 
  ON public.profiles FOR UPDATE 
  USING (auth.uid() = id);

CREATE POLICY "Users can insert own profile." 
  ON public.profiles FOR INSERT 
  WITH CHECK (auth.uid() = id);

-- Create a trigger to create a profile on user signup
CREATE OR REPLACE FUNCTION public.handle_new_user()
RETURNS trigger AS $$
BEGIN
  INSERT INTO public.profiles (id, email, updated_at)
  VALUES (new.id, new.email, now());
  RETURN new;
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;

-- Trigger the function every time a user is created
CREATE OR REPLACE TRIGGER on_auth_user_created
  AFTER INSERT ON auth.users
  FOR EACH ROW EXECUTE FUNCTION public.handle_new_user();
