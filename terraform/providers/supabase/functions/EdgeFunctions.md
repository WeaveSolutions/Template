# Supabase Edge Functions

This directory contains Deno-based Edge Functions for Supabase.

## 📁 Structure

```
functions/
├── types.d.ts              # TypeScript declarations for Deno
├── deno.json              # Deno configuration
├── hello-world/           # Demo function
│   ├── index.ts
│   └── import_map.json
└── send-email/            # Authenticated email function
    ├── index.ts
    └── import_map.json
```

## 🔧 TypeScript Setup

These functions are written for **Deno**, not Node.js. The IDE may show TypeScript errors because it's expecting Node.js modules. This is normal and expected.

### Type Declarations

- `types.d.ts` contains Deno-specific type declarations
- Each function includes `/// <reference path="../types.d.ts" />` to reference these types
- `deno.json` configures the Deno environment

### Running Functions Locally

To test functions locally with Deno:

```bash
# Install Deno
winget install deno

# Run hello-world function
cd hello-world
deno run --allow-net --allow-env index.ts

# Run send-email function  
cd send-email
deno run --allow-net --allow-env index.ts
```

## 🚀 Functions

### hello-world
- **Purpose**: Simple demo function
- **Authentication**: None required
- **Method**: GET, POST
- **Response**: JSON greeting

```typescript
// Usage
fetch('https://your-project.functions.supabase.co/hello-world', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({ name: 'World' })
})
```

### send-email
- **Purpose**: Authenticated email sending
- **Authentication**: JWT required
- **Method**: POST
- **Dependencies**: Supabase client

```typescript
// Usage (requires auth)
const { data, error } = await supabase.functions.invoke('send-email', {
  body: {
    to: 'user@example.com',
    subject: 'Hello',
    message: 'Hello from Supabase!'
  }
})
```

## 🔐 Environment Variables

Functions access environment variables through `Deno.env.get()`:

- `SUPABASE_URL` - Your Supabase project URL
- `SUPABASE_ANON_KEY` - Supabase anonymous key
- `SUPABASE_SERVICE_ROLE_KEY` - Service role key (server-side)

## 📝 Development Notes

### IDE Support
- TypeScript errors about missing modules are expected (Deno vs Node.js)
- Functions will work correctly when deployed to Supabase
- Use `deno check` to validate TypeScript in Deno environment

### Best Practices
- Always handle CORS for browser requests
- Use proper TypeScript types for request/response objects
- Implement proper error handling with try/catch
- Validate request data before processing
- Use JWT verification for protected functions

### Adding New Functions

1. Create new directory: `mkdir my-function`
2. Add `index.ts` with function code
3. Add `import_map.json` for dependencies
4. Reference in Terraform configuration:

```hcl
edge_functions = {
  my_function = {
    name        = "my-function"
    source_code = file("${path.module}/functions/my-function/index.ts")
    import_map  = file("${path.module}/functions/my-function/import_map.json")
    verify_jwt  = true
  }
}
```

## 🐛 Troubleshooting

### TypeScript Errors
- **Module not found**: Normal for Deno URL imports in Node.js IDE
- **Deno not found**: Add `/// <reference path="../types.d.ts" />`
- **Type errors**: Check function signatures match Deno expectations

### Runtime Errors
- **CORS issues**: Add proper CORS headers
- **Auth errors**: Verify JWT handling and Supabase client setup
- **Environment variables**: Ensure variables are set in Supabase dashboard

### Testing
```bash
# Test locally with curl
curl -X POST http://localhost:54321/functions/v1/hello-world \
  -H "Content-Type: application/json" \
  -d '{"name":"Test"}'

# Test with authentication
curl -X POST http://localhost:54321/functions/v1/send-email \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_JWT_TOKEN" \
  -d '{"to":"test@example.com","subject":"Test","message":"Hello"}'
```
