# API Endpoints Template

## REST API Structure

```typescript
// Express.js Example
import express from 'express';
const router = express.Router();

// GET /api/users
router.get('/users', async (req, res) => {
  try {
    const users = await db.user.findMany();
    res.json({ success: true, data: users });
  } catch (error) {
    res.status(500).json({ success: false, error: error.message });
  }
});

// POST /api/users
router.post('/users', async (req, res) => {
  try {
    const user = await db.user.create({ data: req.body });
    res.status(201).json({ success: true, data: user });
  } catch (error) {
    res.status(400).json({ success: false, error: error.message });
  }
});

// GET /api/users/:id
router.get('/users/:id', async (req, res) => {
  const user = await db.user.findUnique({ where: { id: req.params.id } });
  if (!user) return res.status(404).json({ success: false, error: 'Not found' });
  res.json({ success: true, data: user });
});

// PUT /api/users/:id
router.put('/users/:id', async (req, res) => {
  const user = await db.user.update({
    where: { id: req.params.id },
    data: req.body
  });
  res.json({ success: true, data: user });
});

// DELETE /api/users/:id
router.delete('/users/:id', async (req, res) => {
  await db.user.delete({ where: { id: req.params.id } });
  res.json({ success: true });
});

export default router;
```

## Response Format

```typescript
interface APIResponse<T> {
  success: boolean;
  data?: T;
  error?: string;
  pagination?: {
    page: number;
    limit: number;
    total: number;
  };
}
```

## Error Handling

```typescript
class APIError extends Error {
  statusCode: number;
  constructor(message: string, statusCode: number) {
    super(message);
    this.statusCode = statusCode;
  }
}

// Error middleware
app.use((err, req, res, next) => {
  const status = err.statusCode || 500;
  res.status(status).json({
    success: false,
    error: err.message
  });
});
```

## Authentication

```typescript
// Middleware
const authenticate = async (req, res, next) => {
  const token = req.headers.authorization?.split(' ')[1];
  if (!token) return res.status(401).json({ error: 'Unauthorized' });
  
  try {
    const user = await verifyToken(token);
    req.user = user;
    next();
  } catch (error) {
    res.status(401).json({ error: 'Invalid token' });
  }
};

// Protected route
router.get('/protected', authenticate, async (req, res) => {
  res.json({ user: req.user });
});
```

## Best Practices
1. Use versioning: `/api/v1/`
2. Implement rate limiting
3. Validate input
4. Log requests
5. Use CORS properly
6. Return consistent responses
7. Document with OpenAPI/Swagger
