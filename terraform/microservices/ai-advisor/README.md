# AI Advisor Service Infrastructure

This directory contains the Terraform configuration for deploying the AI Advisor Service, which provides intelligent recommendations and insights using Claude 4 Sonnet, OpenAI GPT-4, and Google Gemini for the Nexpo application.

## 🤖 Service Overview

The AI Advisor provides:
- **OKR Generation**: AI-powered Objectives and Key Results based on KPIs
- **Goal Suggestions**: Intelligent goal recommendations using historical data
- **Performance Insights**: AI analysis of business and user metrics
- **Predictive Analytics**: Forecasting and trend analysis
- **Multi-Model Support**: Claude 4 Sonnet, GPT-4, Gemini integration
- **Context-Aware Recommendations**: Personalized advice based on user behavior
- **Natural Language Queries**: Chat interface for business intelligence

## 🏗️ Architecture

```
┌─────────────────┐     ┌──────────────┐     ┌─────────────┐
│  Dashboard/App  │────▶│  API Gateway │────▶│ AI Advisor  │
└─────────────────┘     └──────────────┘     └─────────────┘
                                                      │
                        ┌─────────────────────────────┼─────────────────────────────┐
                        │                             │                             │
                        ▼                             ▼                             ▼
                ┌──────────────┐              ┌──────────────┐              ┌──────────────┐
                │ Claude 4     │              │   GPT-4      │              │   Gemini     │
                │  Sonnet      │              │   OpenAI     │              │   Google     │
                └──────────────┘              └──────────────┘              └──────────────┘
                        │                             │                             │
                        └─────────────┬───────────────┴─────────────────────────────┘
                                      │
                                      ▼
                              ┌──────────────┐
                              │ Vector Store │
                              │  (Embeddings)│
                              └──────────────┘
                                      │
                                      ▼
                              ┌──────────────┐
                              │ KPI Engine & │
                              │ Data Sources │
                              └──────────────┘
```

## 📁 Directory Structure

```
ai-advisor/
├── main.tf                    # Main service configuration
├── variables.tf               # Service variables
├── outputs.tf                # Service outputs
├── provider-specific/        # Cloud-specific configs
│   ├── aws.tf               # ECS, Bedrock, SageMaker
│   ├── gcp.tf               # Cloud Run, Vertex AI, AI Platform
│   ├── azure.tf             # Container Apps, OpenAI Service
│   ├── oci.tf               # Container Instances, AI Services
│   ├── ibm.tf               # Code Engine, Watson AI
│   └── cloudflare.tf        # Workers AI, Vectorize
├── ai-models/                # AI model configurations
│   ├── claude.tf            # Anthropic Claude 4 Sonnet setup
│   ├── openai.tf            # OpenAI GPT-4 configuration
│   ├── gemini.tf            # Google Gemini setup
│   ├── bedrock.tf           # AWS Bedrock models
│   └── vertex.tf            # Google Vertex AI
├── vector-store/             # Vector database configurations
│   ├── pinecone.tf          # Pinecone vector database
│   ├── weaviate.tf          # Weaviate setup
│   ├── chroma.tf            # ChromaDB configuration
│   └── pgvector.tf          # PostgreSQL with pgvector
├── prompts/                  # AI prompt templates
│   ├── okr-generation.tf    # OKR generation prompts
│   ├── goal-suggestions.tf  # Goal recommendation prompts
│   ├── insights.tf          # Performance insight prompts
│   └── forecasting.tf       # Predictive analytics prompts
├── workflows/                # AI workflow configurations
│   ├── recommendation.tf    # Recommendation engine workflow
│   ├── analysis.tf          # Data analysis workflow
│   ├── chat.tf              # Conversational AI workflow
│   └── batch-processing.tf  # Batch AI processing
├── monitoring/               # Observability configurations
│   ├── dashboards.tf        # AI service dashboards
│   ├── alerts.tf            # AI-specific alerts
│   └── logs.tf              # AI interaction logging
└── README.md                # This file
```

## 🚀 Deployment

### Prerequisites
- Terraform >= 1.0
- Cloud provider CLI configured
- AI service API keys (Claude, OpenAI, Gemini)
- Vector database setup
- KPI Engine integration

### Quick Start
```bash
# Navigate to AI advisor service
cd terraform/microservices/ai-advisor

# Initialize Terraform
terraform init

# Plan deployment
terraform plan -var-file="../../terraform.tfvars"

# Deploy service
terraform apply -var-file="../../terraform.tfvars"
```

### Environment-Specific Deployment
```bash
# Development (single model)
terraform apply -var="environment=dev" -var="enable_claude=true" -var="enable_openai=false"

# Production (multi-model)
terraform apply -var="environment=prod" -var="enable_all_models=true"
```

## 🔧 Configuration

### Required Variables
```hcl
# Service Configuration
service_name     = "ai-advisor"
environment      = "dev"
instance_count   = 2
cpu_units        = 1024
memory_mb        = 2048

# AI Model Configuration
enable_claude    = true
enable_openai    = true
enable_gemini    = true
enable_bedrock   = false
enable_vertex    = false

# Model-specific settings
claude_model     = "claude-3-5-sonnet-20241022"
openai_model     = "gpt-4o"
gemini_model     = "gemini-1.5-pro"

# Vector Store Configuration
vector_store_type = "pinecone"  # pinecone, weaviate, chroma, pgvector
embedding_model   = "text-embedding-3-large"
vector_dimensions = 3072

# Rate Limiting
max_requests_per_minute = 100
max_tokens_per_request  = 4000
enable_request_queuing  = true
```

### AI Model API Keys
```hcl
# Store in cloud secret managers
ai_api_keys = {
  claude_api_key  = var.claude_api_key
  openai_api_key  = var.openai_api_key
  gemini_api_key  = var.gemini_api_key
}

# Cost management
monthly_budget_usd = 1000
cost_alerts = {
  warning_threshold = 0.8
  critical_threshold = 0.95
}
```

## 🤖 AI Model Integration

### Claude 4 Sonnet Configuration
```python
# Claude configuration for business insights
claude_config = {
    "model": "claude-3-5-sonnet-20241022",
    "max_tokens": 4000,
    "temperature": 0.3,
    "system_prompt": """You are a business intelligence advisor with expertise in:
    - OKR (Objectives and Key Results) methodology
    - KPI analysis and interpretation
    - Strategic goal setting
    - Performance optimization
    
    Analyze the provided business data and generate actionable insights.""",
    "use_cases": [
        "okr_generation",
        "strategic_analysis",
        "performance_insights",
        "goal_recommendations"
    ]
}
```

### OpenAI GPT-4 Configuration
```python
# GPT-4 configuration for conversational AI
openai_config = {
    "model": "gpt-4o",
    "max_tokens": 3000,
    "temperature": 0.4,
    "functions": [
        {
            "name": "analyze_kpi_trend",
            "description": "Analyze KPI trends and provide insights",
            "parameters": {
                "type": "object",
                "properties": {
                    "kpi_name": {"type": "string"},
                    "time_period": {"type": "string"},
                    "trend_data": {"type": "array"}
                }
            }
        }
    ],
    "use_cases": [
        "conversational_bi",
        "data_exploration",
        "trend_analysis",
        "forecasting"
    ]
}
```

### Google Gemini Configuration
```python
# Gemini configuration for multimodal analysis
gemini_config = {
    "model": "gemini-1.5-pro",
    "max_tokens": 2000,
    "temperature": 0.2,
    "safety_settings": [
        {
            "category": "HARM_CATEGORY_HARASSMENT",
            "threshold": "BLOCK_MEDIUM_AND_ABOVE"
        }
    ],
    "use_cases": [
        "chart_analysis",
        "document_processing",
        "multimodal_insights",
        "visual_data_interpretation"
    ]
}
```

## 🔌 API Endpoints

### OKR Generation APIs
```
POST   /api/ai/okr/generate          # Generate OKRs based on KPIs
GET    /api/ai/okr/suggestions       # Get OKR suggestions for team/individual
PUT    /api/ai/okr/:id/refine        # Refine existing OKR with AI
POST   /api/ai/okr/evaluate          # Evaluate OKR progress and suggest adjustments
```

### Performance Insights APIs
```
POST   /api/ai/insights/analyze      # Analyze performance data
GET    /api/ai/insights/trends       # Get trend analysis and predictions
POST   /api/ai/insights/compare      # Compare performance across periods
GET    /api/ai/insights/recommendations # Get improvement recommendations
```

### Conversational AI APIs
```
POST   /api/ai/chat/query            # Natural language business queries
GET    /api/ai/chat/history/:session # Get chat history
POST   /api/ai/chat/context          # Set context for conversation
WS     /api/ai/chat/stream           # Real-time chat interface
```

### Goal Management APIs
```
POST   /api/ai/goals/suggest         # Suggest goals based on data
POST   /api/ai/goals/prioritize      # AI-powered goal prioritization
GET    /api/ai/goals/progress        # Analyze goal progress
POST   /api/ai/goals/adjust          # Suggest goal adjustments
```

## 🧠 AI Workflows

### OKR Generation Workflow
```python
def generate_okr_workflow(user_id, team_id, time_period):
    """
    AI-powered OKR generation workflow
    """
    # 1. Gather context data
    kpi_data = fetch_kpi_data(team_id, time_period)
    historical_performance = fetch_historical_data(team_id)
    company_objectives = fetch_company_objectives()
    
    # 2. Prepare AI prompt
    context = {
        "current_kpis": kpi_data,
        "historical_trends": historical_performance,
        "company_goals": company_objectives,
        "team_context": fetch_team_context(team_id)
    }
    
    # 3. Generate OKRs using Claude
    okr_response = claude_client.generate_okrs(
        context=context,
        model="claude-3-5-sonnet-20241022",
        max_objectives=3,
        key_results_per_objective=3
    )
    
    # 4. Validate and refine
    validated_okrs = validate_okr_feasibility(okr_response, kpi_data)
    
    # 5. Store and return
    return store_generated_okrs(user_id, team_id, validated_okrs)
```

### Performance Analysis Workflow
```python
def analyze_performance_workflow(kpi_name, analysis_type):
    """
    AI-powered performance analysis
    """
    # 1. Fetch relevant data
    kpi_history = fetch_kpi_history(kpi_name, days=90)
    benchmark_data = fetch_industry_benchmarks(kpi_name)
    
    # 2. Generate embeddings for similar patterns
    embeddings = generate_embeddings(kpi_history)
    similar_patterns = vector_store.similarity_search(embeddings)
    
    # 3. AI analysis using multiple models
    analyses = []
    
    # Claude for strategic insights
    claude_analysis = claude_client.analyze_performance(
        kpi_data=kpi_history,
        benchmarks=benchmark_data,
        focus="strategic_insights"
    )
    
    # GPT-4 for trend analysis
    gpt4_analysis = openai_client.analyze_trends(
        data=kpi_history,
        similar_patterns=similar_patterns,
        focus="trend_prediction"
    )
    
    # 4. Combine and synthesize insights
    combined_insights = synthesize_insights([claude_analysis, gpt4_analysis])
    
    return {
        "insights": combined_insights,
        "recommendations": generate_recommendations(combined_insights),
        "confidence_score": calculate_confidence(analyses)
    }
```

## 📊 Prompt Templates

### OKR Generation Prompts
```python
OKR_GENERATION_PROMPT = """
Based on the following business context, generate 3 strategic Objectives and 3-4 Key Results for each:

CURRENT PERFORMANCE:
{kpi_summary}

HISTORICAL TRENDS:
{trend_analysis}

COMPANY OBJECTIVES:
{company_goals}

TEAM CONTEXT:
{team_info}

Generate OKRs that are:
1. Ambitious but achievable
2. Measurable with specific metrics
3. Aligned with company objectives
4. Based on data-driven insights

Format as JSON with confidence scores for each OKR.
"""

GOAL_SUGGESTION_PROMPT = """
Analyze the performance data and suggest 5 specific, actionable goals:

PERFORMANCE DATA:
{performance_metrics}

AREAS FOR IMPROVEMENT:
{improvement_areas}

RESOURCE CONSTRAINTS:
{constraints}

Provide goals that are SMART (Specific, Measurable, Achievable, Relevant, Time-bound).
Include expected impact and required resources for each goal.
"""
```

### Analysis Prompts
```python
PERFORMANCE_INSIGHT_PROMPT = """
Analyze the following KPI data and provide insights:

KPI: {kpi_name}
TIME PERIOD: {time_period}
DATA: {kpi_data}
BENCHMARKS: {benchmark_data}

Provide:
1. Key trends and patterns
2. Performance vs benchmarks
3. Potential causes for changes
4. Actionable recommendations
5. Risk factors to monitor

Be specific and data-driven in your analysis.
"""

FORECASTING_PROMPT = """
Based on historical data, provide a forecast for the next {forecast_period}:

HISTORICAL DATA:
{historical_data}

SEASONAL PATTERNS:
{seasonal_info}

EXTERNAL FACTORS:
{external_factors}

Provide:
1. Point forecast with confidence intervals
2. Key assumptions
3. Risk factors that could affect forecast
4. Recommended monitoring metrics
"""
```

## 🔒 Security Features

### API Security
- API key authentication for AI services
- Rate limiting per user/organization
- Request/response logging and monitoring
- PII detection and redaction

### Data Privacy
- Sensitive data anonymization before AI processing
- Compliance with AI service data policies
- Local processing for sensitive operations
- Audit trails for all AI interactions

### Cost Management
- Token usage tracking and alerts
- Budget limits per user/organization
- Model selection based on cost/performance
- Automatic fallback to cheaper models

## 📈 Monitoring & Observability

### Health Checks
```
GET /health              # Basic health check
GET /health/models       # AI model availability
GET /health/vector-store # Vector database connectivity
GET /metrics            # Prometheus metrics
```

### Key Metrics
- **AI Performance**: Response time, token usage, model accuracy
- **Business Impact**: OKR adoption rate, goal achievement
- **Cost Metrics**: API costs per model, cost per insight
- **Quality Metrics**: User satisfaction, recommendation relevance

### AI-Specific Monitoring
```python
# Custom metrics for AI service
ai_metrics = {
    "model_response_time": histogram_metric,
    "token_usage_total": counter_metric,
    "insight_generation_success": counter_metric,
    "user_satisfaction_score": gauge_metric,
    "cost_per_request": histogram_metric
}
```

## 🔄 Integration Points

### Internal Services
- **KPI Engine**: Real-time data for AI analysis
- **User Service**: User context and preferences
- **Notifications Service**: AI-generated alerts and insights
- **Reports Service**: AI-enhanced report generation

### External Integrations
- **Business Intelligence Tools**: Enhanced with AI insights
- **Project Management**: OKR integration with Jira, Asana
- **Communication**: Slack/Teams bots for AI insights
- **CRM Systems**: AI-powered customer insights

## 🚦 Deployment Strategies

### A/B Testing for AI Models
```bash
# Deploy with model split testing
terraform apply -var="claude_traffic_percentage=50" \
                -var="openai_traffic_percentage=50"

# Monitor performance and user satisfaction
# Adjust traffic based on results
terraform apply -var="claude_traffic_percentage=70" \
                -var="openai_traffic_percentage=30"
```

### Gradual Model Rollout
```bash
# Start with single model
terraform apply -var="enable_claude=true" -var="enable_others=false"

# Add models incrementally
terraform apply -var="enable_openai=true"
terraform apply -var="enable_gemini=true"
```

## 🛠️ Development

### Local Development
```bash
# Start vector database
docker-compose up -d pinecone-local

# Set environment variables
export CLAUDE_API_KEY="your-claude-key"
export OPENAI_API_KEY="your-openai-key"
export GEMINI_API_KEY="your-gemini-key"
export VECTOR_STORE_URL="http://localhost:8080"

# Run AI advisor service
npm run dev
```

### Testing
```bash
# Unit tests
npm run test

# AI model integration tests
npm run test:ai-models

# Performance tests
npm run test:performance

# Cost simulation tests
npm run test:cost-simulation
```

## 🆘 Troubleshooting

### Common Issues

**High AI API Costs**
```bash
# Check token usage patterns
curl /api/ai/metrics/token-usage

# Review expensive queries
kubectl logs -f deployment/ai-advisor | grep "high-cost"

# Optimize prompts and model selection
```

**Slow AI Response Times**
```bash
# Check model latency
curl /api/ai/health/models

# Monitor vector store performance
curl /api/ai/health/vector-store

# Review prompt complexity
```

**Poor AI Insight Quality**
```bash
# Check training data quality
curl /api/ai/insights/quality-metrics

# Review user feedback
curl /api/ai/feedback/summary

# Adjust prompts and model parameters
```

## 📚 Related Documentation

- [KPI Engine Integration](../kpi-engine/README.md)
- [Vector Database Setup](../../vector-stores/README.md)
- [AI Model Configuration](../../ai-models/README.md)
- [Cost Optimization Guide](../../cost-optimization/README.md)
- [Security Best Practices](../../security/ai-security.md)
