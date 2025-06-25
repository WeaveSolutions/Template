# AI Advisor Microservice Implementation Guide

## Overview

The AI Advisor microservice provides intelligent recommendations and insights for the Nexpo application using multiple AI models including Claude 4 Sonnet, OpenAI GPT-4, and Google Gemini. It generates OKRs, goal suggestions, performance insights, and predictive analytics based on KPI data and user behavior.

## Architecture

### Core Technologies
- **Language**: TypeScript/Node.js
- **Framework**: Express.js or Fastify
- **AI Models**: Claude 4 Sonnet, GPT-4, Gemini
- **Vector Store**: Pinecone, Weaviate, or PostgreSQL with pgvector
- **Database**: PostgreSQL for metadata and context
- **Cache**: Redis for prompt caching

### Service Structure
```
ai-advisor-service/
├── src/
│   ├── api/
│   │   ├── routes/
│   │   │   ├── recommendations.ts
│   │   │   ├── insights.ts
│   │   │   ├── chat.ts
│   │   │   └── okr.ts
│   │   └── middleware/
│   │       ├── auth.ts
│   │       └── rate-limiting.ts
│   ├── ai-models/
│   │   ├── claude/
│   │   │   ├── client.ts
│   │   │   └── prompts.ts
│   │   ├── openai/
│   │   │   ├── client.ts
│   │   │   └── prompts.ts
│   │   ├── gemini/
│   │   │   ├── client.ts
│   │   │   └── prompts.ts
│   │   └── base/
│   │       ├── model-interface.ts
│   │       └── prompt-manager.ts
│   ├── services/
│   │   ├── okr-generator.ts
│   │   ├── goal-recommender.ts
│   │   ├── insight-analyzer.ts
│   │   ├── forecasting.ts
│   │   └── context-builder.ts
│   ├── vector-store/
│   │   ├── embeddings.ts
│   │   ├── retrieval.ts
│   │   └── indexing.ts
│   ├── prompts/
│   │   ├── templates/
│   │   ├── chains/
│   │   └── validators/
│   └── utils/
│       ├── logger.ts
│       └── metrics.ts
├── tests/
├── docs/
└── package.json
```

## AI Model Integration

### Base Model Interface
```typescript
// src/ai-models/base/model-interface.ts
export interface AIModel {
  name: string;
  version: string;
  maxTokens: number;
  temperature: number;
  
  generateCompletion(prompt: string, options?: CompletionOptions): Promise<AIResponse>;
  generateEmbedding(text: string): Promise<number[]>;
  streamCompletion(prompt: string, onChunk: (chunk: string) => void): Promise<void>;
}

export interface CompletionOptions {
  temperature?: number;
  maxTokens?: number;
  topP?: number;
  stopSequences?: string[];
  systemPrompt?: string;
}

export interface AIResponse {
  content: string;
  usage: {
    promptTokens: number;
    completionTokens: number;
    totalTokens: number;
  };
  model: string;
  finishReason: string;
}

export abstract class BaseAIModel implements AIModel {
  protected config: ModelConfig;
  protected rateLimiter: RateLimiter;
  
  constructor(config: ModelConfig) {
    this.config = config;
    this.rateLimiter = new RateLimiter(config.rateLimit);
  }
  
  abstract generateCompletion(prompt: string, options?: CompletionOptions): Promise<AIResponse>;
  abstract generateEmbedding(text: string): Promise<number[]>;
  
  protected async checkRateLimit(): Promise<void> {
    await this.rateLimiter.checkLimit();
  }
}
```

### Claude 4 Sonnet Implementation
```typescript
// src/ai-models/claude/client.ts
import Anthropic from '@anthropic-ai/sdk';
import { BaseAIModel } from '../base/model-interface';

export class ClaudeModel extends BaseAIModel {
  private client: Anthropic;
  
  constructor(config: ModelConfig) {
    super(config);
    this.client = new Anthropic({
      apiKey: config.apiKey,
    });
  }
  
  async generateCompletion(prompt: string, options?: CompletionOptions): Promise<AIResponse> {
    await this.checkRateLimit();
    
    const response = await this.client.messages.create({
      model: 'claude-3-sonnet-20240229',
      max_tokens: options?.maxTokens || this.config.maxTokens,
      temperature: options?.temperature || this.config.temperature,
      system: options?.systemPrompt || this.config.systemPrompt,
      messages: [{ role: 'user', content: prompt }]
    });
    
    return {
      content: response.content[0].text,
      usage: {
        promptTokens: response.usage.input_tokens,
        completionTokens: response.usage.output_tokens,
        totalTokens: response.usage.input_tokens + response.usage.output_tokens
      },
      model: response.model,
      finishReason: response.stop_reason
    };
  }
  
  async generateEmbedding(text: string): Promise<number[]> {
    // Claude doesn't have native embeddings, use OpenAI for embeddings
    throw new Error('Use OpenAI or dedicated embedding model for embeddings');
  }
}
```

### Multi-Model Orchestrator
```typescript
// src/ai-models/orchestrator.ts
export class AIModelOrchestrator {
  private models: Map<string, AIModel>;
  private primaryModel: string;
  
  constructor() {
    this.models = new Map();
    this.primaryModel = 'claude';
  }
  
  registerModel(name: string, model: AIModel): void {
    this.models.set(name, model);
  }
  
  async generateWithFallback(
    prompt: string, 
    options?: CompletionOptions
  ): Promise<AIResponse> {
    const modelPriority = ['claude', 'gpt4', 'gemini'];
    
    for (const modelName of modelPriority) {
      const model = this.models.get(modelName);
      if (!model) continue;
      
      try {
        return await model.generateCompletion(prompt, options);
      } catch (error) {
        console.error(`Model ${modelName} failed:`, error);
        continue;
      }
    }
    
    throw new Error('All AI models failed');
  }
  
  async generateMultiModel(
    prompt: string,
    options?: CompletionOptions
  ): Promise<Map<string, AIResponse>> {
    const responses = new Map<string, AIResponse>();
    
    await Promise.all(
      Array.from(this.models.entries()).map(async ([name, model]) => {
        try {
          const response = await model.generateCompletion(prompt, options);
          responses.set(name, response);
        } catch (error) {
          console.error(`Model ${name} failed:`, error);
        }
      })
    );
    
    return responses;
  }
}
```

## Core Services

### OKR Generator
```typescript
// src/services/okr-generator.ts
export class OKRGenerator {
  private aiOrchestrator: AIModelOrchestrator;
  private contextBuilder: ContextBuilder;
  private vectorStore: VectorStore;
  
  constructor(
    aiOrchestrator: AIModelOrchestrator,
    contextBuilder: ContextBuilder,
    vectorStore: VectorStore
  ) {
    this.aiOrchestrator = aiOrchestrator;
    this.contextBuilder = contextBuilder;
    this.vectorStore = vectorStore;
  }
  
  async generateOKRs(request: OKRRequest): Promise<OKRSet> {
    // Build context from KPIs and historical data
    const context = await this.contextBuilder.buildOKRContext({
      userId: request.userId,
      timeframe: request.timeframe,
      focusArea: request.focusArea,
      currentKPIs: request.kpis
    });
    
    // Retrieve similar successful OKRs
    const similarOKRs = await this.vectorStore.searchSimilar(
      `OKR ${request.focusArea} ${request.timeframe}`,
      5
    );
    
    // Generate OKRs using AI
    const prompt = this.buildOKRPrompt(context, similarOKRs);
    const response = await this.aiOrchestrator.generateWithFallback(prompt, {
      temperature: 0.7,
      maxTokens: 2000,
      systemPrompt: OKR_SYSTEM_PROMPT
    });
    
    // Parse and validate OKRs
    const okrs = this.parseOKRResponse(response.content);
    await this.validateOKRs(okrs);
    
    // Store for future reference
    await this.storeOKRs(okrs, request.userId);
    
    return okrs;
  }
  
  private buildOKRPrompt(context: OKRContext, similarOKRs: any[]): string {
    return `
Based on the following business context and KPIs, generate 3-5 Objectives with 3-4 Key Results each.

Current Business Metrics:
${JSON.stringify(context.metrics, null, 2)}

Focus Area: ${context.focusArea}
Timeframe: ${context.timeframe}

Similar Successful OKRs:
${similarOKRs.map(okr => okr.content).join('\n')}

Guidelines:
- Objectives should be inspiring and qualitative
- Key Results must be measurable and time-bound
- Align with current business performance
- Be ambitious but achievable

Format the response as JSON with this structure:
{
  "objectives": [
    {
      "title": "Objective title",
      "description": "Brief description",
      "keyResults": [
        {
          "title": "Key Result",
          "target": "Specific measurable target",
          "metric": "KPI name",
          "baseline": "Current value"
        }
      ]
    }
  ]
}
    `;
  }
}
```

### Goal Recommender
```typescript
// src/services/goal-recommender.ts
export class GoalRecommender {
  private aiOrchestrator: AIModelOrchestrator;
  private kpiService: KPIService;
  private userAnalytics: UserAnalytics;
  
  async recommendGoals(userId: string): Promise<Goal[]> {
    // Analyze user's current performance
    const performance = await this.userAnalytics.getPerformanceMetrics(userId);
    
    // Get trending goals in the industry
    const industryTrends = await this.getIndustryTrends(performance.industry);
    
    // Identify gaps and opportunities
    const gaps = await this.identifyPerformanceGaps(performance);
    
    // Generate personalized recommendations
    const prompt = this.buildGoalPrompt(performance, industryTrends, gaps);
    const response = await this.aiOrchestrator.generateWithFallback(prompt, {
      temperature: 0.8,
      maxTokens: 1500
    });
    
    // Parse and rank goals
    const goals = this.parseGoalResponse(response.content);
    return this.rankGoalsByRelevance(goals, performance);
  }
  
  private async identifyPerformanceGaps(
    performance: PerformanceMetrics
  ): Promise<Gap[]> {
    const gaps: Gap[] = [];
    
    // Compare against benchmarks
    for (const metric of performance.metrics) {
      if (metric.value < metric.benchmark * 0.8) {
        gaps.push({
          metric: metric.name,
          current: metric.value,
          benchmark: metric.benchmark,
          gap: metric.benchmark - metric.value,
          priority: this.calculatePriority(metric)
        });
      }
    }
    
    return gaps.sort((a, b) => b.priority - a.priority);
  }
}
```

### Insight Analyzer
```typescript
// src/services/insight-analyzer.ts
export class InsightAnalyzer {
  private aiOrchestrator: AIModelOrchestrator;
  private dataAnalyzer: DataAnalyzer;
  
  async analyzePerformance(request: AnalysisRequest): Promise<Insights> {
    // Gather data from multiple sources
    const data = await this.gatherAnalysisData(request);
    
    // Perform statistical analysis
    const statistics = await this.dataAnalyzer.analyze(data);
    
    // Generate AI insights
    const insights = await this.generateInsights(data, statistics);
    
    // Add actionable recommendations
    const recommendations = await this.generateRecommendations(insights);
    
    return {
      insights,
      recommendations,
      statistics,
      confidence: this.calculateConfidence(data)
    };
  }
  
  private async generateInsights(
    data: AnalysisData,
    statistics: Statistics
  ): Promise<Insight[]> {
    const prompt = `
Analyze the following business data and provide key insights:

Data Summary:
${JSON.stringify(data.summary, null, 2)}

Statistical Analysis:
${JSON.stringify(statistics, null, 2)}

Identify:
1. Key trends and patterns
2. Anomalies or concerns
3. Opportunities for improvement
4. Correlations between metrics

Format each insight with:
- Title
- Description
- Impact (high/medium/low)
- Supporting data points
    `;
    
    const response = await this.aiOrchestrator.generateWithFallback(prompt);
    return this.parseInsights(response.content);
  }
}
```

## Vector Store Integration

### Embedding Service
```typescript
// src/vector-store/embeddings.ts
export class EmbeddingService {
  private openaiClient: OpenAI;
  private cache: EmbeddingCache;
  
  constructor(openaiApiKey: string) {
    this.openaiClient = new OpenAI({ apiKey: openaiApiKey });
    this.cache = new EmbeddingCache();
  }
  
  async generateEmbedding(text: string): Promise<number[]> {
    // Check cache first
    const cached = await this.cache.get(text);
    if (cached) return cached;
    
    // Generate new embedding
    const response = await this.openaiClient.embeddings.create({
      model: 'text-embedding-3-large',
      input: text,
      dimensions: 1536
    });
    
    const embedding = response.data[0].embedding;
    
    // Cache for future use
    await this.cache.set(text, embedding);
    
    return embedding;
  }
  
  async generateBatchEmbeddings(texts: string[]): Promise<number[][]> {
    const embeddings: number[][] = [];
    
    // Process in batches of 100
    for (let i = 0; i < texts.length; i += 100) {
      const batch = texts.slice(i, i + 100);
      const response = await this.openaiClient.embeddings.create({
        model: 'text-embedding-3-large',
        input: batch,
        dimensions: 1536
      });
      
      embeddings.push(...response.data.map(d => d.embedding));
    }
    
    return embeddings;
  }
}
```

### Vector Retrieval
```typescript
// src/vector-store/retrieval.ts
import { Pinecone } from '@pinecone-database/pinecone';

export class VectorRetrieval {
  private pinecone: Pinecone;
  private index: any;
  
  constructor(config: VectorStoreConfig) {
    this.pinecone = new Pinecone({
      apiKey: config.apiKey,
      environment: config.environment
    });
    this.index = this.pinecone.index(config.indexName);
  }
  
  async searchSimilar(
    query: string,
    topK: number = 10,
    filter?: any
  ): Promise<SearchResult[]> {
    // Generate embedding for query
    const queryEmbedding = await this.embeddingService.generateEmbedding(query);
    
    // Search in vector store
    const results = await this.index.query({
      vector: queryEmbedding,
      topK,
      filter,
      includeMetadata: true,
      includeValues: false
    });
    
    return results.matches.map(match => ({
      id: match.id,
      score: match.score,
      metadata: match.metadata,
      content: match.metadata.text
    }));
  }
  
  async upsertDocuments(documents: Document[]): Promise<void> {
    // Generate embeddings
    const texts = documents.map(doc => doc.content);
    const embeddings = await this.embeddingService.generateBatchEmbeddings(texts);
    
    // Prepare vectors
    const vectors = documents.map((doc, i) => ({
      id: doc.id,
      values: embeddings[i],
      metadata: {
        text: doc.content,
        type: doc.type,
        userId: doc.userId,
        timestamp: doc.timestamp
      }
    }));
    
    // Upsert in batches
    for (let i = 0; i < vectors.length; i += 100) {
      const batch = vectors.slice(i, i + 100);
      await this.index.upsert(batch);
    }
  }
}
```

## Prompt Management

### Prompt Templates
```typescript
// src/prompts/templates/base.ts
export abstract class PromptTemplate {
  protected template: string;
  protected variables: string[];
  
  constructor(template: string) {
    this.template = template;
    this.variables = this.extractVariables(template);
  }
  
  render(context: Record<string, any>): string {
    let rendered = this.template;
    
    for (const variable of this.variables) {
      const value = context[variable];
      if (value === undefined) {
        throw new Error(`Missing variable: ${variable}`);
      }
      rendered = rendered.replace(
        new RegExp(`{{${variable}}}`, 'g'),
        String(value)
      );
    }
    
    return rendered;
  }
  
  private extractVariables(template: string): string[] {
    const regex = /{{(\w+)}}/g;
    const variables: string[] = [];
    let match;
    
    while ((match = regex.exec(template)) !== null) {
      variables.push(match[1]);
    }
    
    return [...new Set(variables)];
  }
}

// OKR Generation Prompt
export const OKR_SYSTEM_PROMPT = `
You are an expert business strategist specializing in OKR (Objectives and Key Results) development.
Your goal is to create ambitious yet achievable OKRs that align with business metrics and drive growth.
Always provide specific, measurable key results with clear targets and timelines.
`;

// Insight Analysis Prompt
export const INSIGHT_SYSTEM_PROMPT = `
You are a data analyst expert who identifies patterns, trends, and actionable insights from business data.
Focus on practical recommendations that can be implemented immediately.
Quantify the potential impact of your suggestions when possible.
`;
```

## API Endpoints

### Recommendation Endpoints
```typescript
// GET /api/ai/recommendations/goals
// POST /api/ai/recommendations/okrs
// GET /api/ai/recommendations/actions

interface GoalRecommendationResponse {
  goals: Goal[];
  reasoning: string[];
  basedOn: {
    performance: PerformanceSnapshot;
    trends: Trend[];
    gaps: Gap[];
  };
}

interface OKRGenerationRequest {
  focusArea: string;
  timeframe: 'quarter' | 'year';
  currentKPIs?: KPI[];
  constraints?: string[];
}
```

### Insight Endpoints
```typescript
// POST /api/ai/insights/analyze
// GET /api/ai/insights/performance
// POST /api/ai/insights/forecast

interface InsightAnalysisRequest {
  metrics: string[];
  timeRange: TimeRange;
  compareWith?: 'previous_period' | 'benchmark' | 'goal';
  depth: 'summary' | 'detailed' | 'comprehensive';
}

interface ForecastRequest {
  metric: string;
  horizon: number; // days
  confidence: number; // 0.0 to 1.0
  includeFactors?: boolean;
}
```

### Chat Interface
```typescript
// POST /api/ai/chat
// GET /api/ai/chat/history
// POST /api/ai/chat/feedback

interface ChatRequest {
  message: string;
  context?: {
    conversationId?: string;
    metrics?: string[];
    timeframe?: TimeRange;
  };
  model?: 'claude' | 'gpt4' | 'gemini';
}

interface ChatResponse {
  response: string;
  conversationId: string;
  suggestions?: string[];
  visualizations?: ChartConfig[];
  sources?: DataSource[];
}
```

## Environment Variables

```bash
# Service Configuration
NODE_ENV=production
PORT=3006
SERVICE_NAME=ai-advisor-service

# Auth0 M2M
AUTH0_DOMAIN=your-tenant.auth0.com
AUTH0_CLIENT_ID=your_m2m_client_id
AUTH0_CLIENT_SECRET=your_m2m_client_secret
AUTH0_AUDIENCE=https://api.nexpo.app

# AI Model API Keys
ANTHROPIC_API_KEY=your_claude_api_key
OPENAI_API_KEY=your_openai_api_key
GOOGLE_AI_API_KEY=your_gemini_api_key

# Vector Store
PINECONE_API_KEY=your_pinecone_api_key
PINECONE_ENVIRONMENT=us-west1-gcp
PINECONE_INDEX_NAME=nexpo-ai-advisor

# Database
DATABASE_URL=postgresql://user:password@localhost:5432/aiadviso

# Redis Cache
REDIS_URL=redis://localhost:6379
CACHE_TTL_SECONDS=3600

# Model Configuration
DEFAULT_MODEL=claude
MAX_TOKENS=4000
TEMPERATURE=0.7

# Rate Limiting
RATE_LIMIT_PER_MINUTE=20
RATE_LIMIT_PER_HOUR=100

# Feature Flags
ENABLE_MULTI_MODEL=true
ENABLE_STREAMING=true
ENABLE_PROMPT_CACHING=true
```

## Security Considerations

### API Key Management
- Store all API keys in secure environment variables
- Implement key rotation policies
- Use separate keys for development/production
- Monitor API usage and costs

### Data Privacy
- Anonymize sensitive business data before AI processing
- Implement data retention policies
- Use on-premise models for highly sensitive data
- Audit all AI interactions

### Rate Limiting
- Implement per-user rate limits
- Add cost controls for AI API usage
- Queue management for high-volume requests
- Fallback strategies for rate limit errors

## Deployment

### Docker Configuration
```dockerfile
FROM node:18-alpine

WORKDIR /app

COPY package*.json ./
RUN npm ci --only=production

COPY . .
RUN npm run build

EXPOSE 3006
CMD ["node", "dist/index.js"]
```

### Health Checks
```typescript
// GET /health
app.get('/health', async (req, res) => {
  const checks = {
    service: 'ok',
    database: await checkDatabase(),
    redis: await checkRedis(),
    vectorStore: await checkVectorStore(),
    aiModels: {
      claude: await checkClaude(),
      gpt4: await checkOpenAI(),
      gemini: await checkGemini()
    }
  };

  const healthy = Object.values(checks).every(check => 
    typeof check === 'string' ? check === 'ok' : 
    Object.values(check).every(v => v === 'ok')
  );

  res.status(healthy ? 200 : 503).json(checks);
});
```

## Performance Optimization

### Caching Strategy
- Cache AI responses with intelligent TTL
- Implement semantic caching for similar queries
- Use Redis for distributed caching
- Pre-generate common insights

### Model Selection
- Use appropriate model for each task
- Implement dynamic model selection based on:
  - Query complexity
  - Required accuracy
  - Cost constraints
  - Response time requirements

### Batch Processing
- Group similar requests for batch processing
- Implement async job queues for large analyses
- Use streaming for real-time responses
- Optimize embedding generation

## Next Steps

1. **Set Up AI Models**
   - Configure Claude, GPT-4, and Gemini APIs
   - Test model responses and latencies
   - Implement fallback strategies

2. **Initialize Vector Store**
   - Set up Pinecone or alternative
   - Create embedding pipeline
   - Index historical data

3. **Build Core Services**
   - Implement OKR generator
   - Create goal recommender
   - Build insight analyzer

4. **Create API Layer**
   - Set up REST endpoints
   - Add authentication
   - Implement rate limiting

5. **Add Monitoring**
   - Track AI usage and costs
   - Monitor response quality
   - Set up alerts

6. **Deploy Service**
   - Create Docker image
   - Configure auto-scaling
   - Set up load balancing