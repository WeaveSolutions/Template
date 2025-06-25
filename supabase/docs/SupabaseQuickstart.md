# Supabase Quick Start Guide for Nexpo

This guide provides instructions for setting up and using Supabase with the Nexpo template.

## Prerequisites

- Node.js and pnpm installed
- A Supabase account ([https://app.supabase.com/](https://app.supabase.com/))
- Supabase CLI installed (optional for local development)

## Setting Up Supabase

### Option 1: Remote Supabase Project (Recommended)

1. **Create a Supabase Project**: 
   - Log in to [Supabase Dashboard](https://app.supabase.com/).
   - Click on "New Project" and follow the prompts to create a new project.
   - Once created, navigate to "Settings" > "API" to find your Project URL and Anonymous Key.

2. **Update Environment Variables**:
   - Open the `.env` file in the root of your Nexpo project.
   - Replace the placeholder values with your Supabase credentials:
     ```
     EXPO_PUBLIC_SUPABASE_URL=your_supabase_project_url
     EXPO_PUBLIC_SUPABASE_ANON_KEY=your_supabase_anon_key
     NEXT_PUBLIC_SUPABASE_URL=your_supabase_project_url
     NEXT_PUBLIC_SUPABASE_ANON_KEY=your_supabase_anon_key
     ```

### Option 2: Local Supabase Development (Advanced)

1. **Install Docker Desktop**: 
   - Download and install Docker Desktop from [https://www.docker.com/products/docker-desktop](https://www.docker.com/products/docker-desktop).
   - Ensure Docker Desktop is running.

2. **Install Supabase CLI**:
   - If not already installed, use Scoop (Windows) to install the Supabase CLI:
     ```powershell
     scoop install supabase
     ```

3. **Initialize Supabase Project**:
   - Navigate to your project root directory and run:
     ```bash
     supabase init
     ```

4. **Start Local Supabase Environment**:
   - Run the following command to start the Supabase services:
     ```bash
     supabase start
     ```
   - If successful, this will output connection details for your local Supabase instance.

## Integrating Supabase Authentication

Supabase authentication is already set up in this project through the `AuthContext` in the `shared-ui` package.

1. **Wrap Your App with AuthProvider**:
   - In both `apps/expo/App.tsx` and `apps/next/pages/_app.tsx`, ensure your app is wrapped with the `AuthProvider`:
     ```typescript
     import { AuthProvider } from 'shared-ui';

     // Wrap your app component
     <AuthProvider>
       <App />
     </AuthProvider>
     ```

2. **Use Authentication in Components**:
   - Use the `useAuth` hook to access authentication methods:
     ```typescript
     import { useAuth } from 'shared-ui';

     const { session, signIn, signUp, signOut } = useAuth();
     ```

## Creating a Database Table

1. **Using Supabase Dashboard**:
   - Go to your Supabase project in the dashboard.
   - Navigate to "SQL" or "Table" section to create and manage database tables.

2. **Using Supabase CLI (Local)**:
   - Modify the schema in `supabase/migrations` folder.
   - Apply changes with:
     ```bash
     supabase migrations up
     ```

## Querying Data from Supabase

Use the Supabase client to interact with your database:

```typescript
import { supabase } from 'shared-ui';

// Fetch data
const { data, error } = await supabase
  .from('your_table')
  .select('*');

if (error) {
  console.error('Error fetching data:', error);
} else {
  console.log('Data:', data);
}
```

## Troubleshooting

- **Local Environment Issues**: If you encounter errors with `supabase start`, ensure Docker Desktop is running. You can use `supabase start --debug` for detailed error messages.
- **Authentication Errors**: Verify that your Supabase URL and Anon Key are correctly set in your `.env` file.
- **Connection Issues**: Check your internet connection or Supabase status if you face issues connecting to a remote project.

For more detailed documentation, refer to the official Supabase docs at [https://supabase.com/docs](https://supabase.com/docs).
