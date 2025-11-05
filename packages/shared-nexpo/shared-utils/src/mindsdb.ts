import { Platform } from 'react-native';

interface MindsDBConfig {
  apiUrl?: string;
  auth0Token?: string;
}

interface Model {
  name: string;
  status: string;
  accuracy: number;
  created_at: string;
}

interface Prediction {
  [key: string]: any;
}

interface CloudStatus {
  aws: boolean;
  gcp: boolean;
  azure: boolean;
  oci: boolean;
}

export class MindsDBClient {
  private apiUrl: string;
  private auth0Token?: string;

  constructor(config: MindsDBConfig = {}) {
    this.apiUrl = config.apiUrl || process.env.NEXT_PUBLIC_MINDSDB_API_URL || process.env.EXPO_PUBLIC_MINDSDB_API_URL || 'http://localhost:47334';
    this.auth0Token = config.auth0Token;
  }

  setAuthToken(token: string) {
    this.auth0Token = token;
  }

  private async fetchWithAuth(url: string, options: RequestInit = {}) {
    const headers = {
      'Content-Type': 'application/json',
      ...(this.auth0Token && { Authorization: `Bearer ${this.auth0Token}` }),
      ...options.headers,
    };

    const response = await fetch(`${this.apiUrl}${url}`, {
      ...options,
      headers,
    });

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.error || 'Request failed');
    }

    return response.json();
  }

  // Health check
  async healthCheck(): Promise<{ status: string; service: string }> {
    return this.fetchWithAuth('/health');
  }

  // Get cloud provider status
  async getCloudStatus(): Promise<{ cloudProviders: CloudStatus }> {
    return this.fetchWithAuth('/cloud-status');
  }

  // List datasources
  async listDatasources(): Promise<{ datasources: string[] }> {
    return this.fetchWithAuth('/datasources');
  }

  // Create datasource
  async createDatasource(
    name: string,
    engine: string,
    parameters: Record<string, string>
  ): Promise<{ message: string; name: string }> {
    return this.fetchWithAuth('/datasources', {
      method: 'POST',
      body: JSON.stringify({ name, engine, parameters }),
    });
  }

  // List models
  async listModels(): Promise<{ models: Model[] }> {
    return this.fetchWithAuth('/models');
  }

  // Create model
  async createModel(
    name: string,
    datasource: string,
    query: string,
    target: string,
    engine?: string
  ): Promise<{ message: string; name: string }> {
    return this.fetchWithAuth('/models', {
      method: 'POST',
      body: JSON.stringify({ name, datasource, query, target, engine }),
    });
  }

  // Get model info
  async getModelInfo(name: string): Promise<{ model: string; schema: any[] }> {
    return this.fetchWithAuth(`/models/${name}`);
  }

  // Delete model
  async deleteModel(name: string): Promise<{ message: string; name: string }> {
    return this.fetchWithAuth(`/models/${name}`, {
      method: 'DELETE',
    });
  }

  // Make predictions
  async predict(model: string, data: Record<string, any>): Promise<{ predictions: Prediction[] }> {
    return this.fetchWithAuth('/predict', {
      method: 'POST',
      body: JSON.stringify({ model, data }),
    });
  }

  // Execute custom query (admin only)
  async executeQuery(query: string): Promise<{ result: any[] }> {
    return this.fetchWithAuth('/query', {
      method: 'POST',
      body: JSON.stringify({ query }),
    });
  }
}

// Singleton instance
let mindsdbClient: MindsDBClient | null = null;

export function getMindsDBClient(config?: MindsDBConfig): MindsDBClient {
  if (!mindsdbClient) {
    mindsdbClient = new MindsDBClient(config);
  }
  return mindsdbClient;
}

// Helper functions for common operations
export async function createCloudDatasource(
  client: MindsDBClient,
  provider: 'aws' | 'gcp' | 'azure' | 'oci',
  name: string,
  config: Record<string, string>
): Promise<void> {
  const engineMap = {
    aws: 's3',
    gcp: 'bigquery',
    azure: 'azureblob',
    oci: 'oci_object_storage',
  };

  await client.createDatasource(name, engineMap[provider], config);
}

export async function trainSalesPredictor(
  client: MindsDBClient,
  datasource: string,
  tableName: string
): Promise<void> {
  const query = `SELECT * FROM ${tableName}`;
  await client.createModel(
    'sales_predictor',
    datasource,
    query,
    'revenue',
    'lightwood'
  );
}

export async function predictSales(
  client: MindsDBClient,
  productData: {
    product: string;
    region: string;
    month: string;
  }
): Promise<number> {
  const result = await client.predict('sales_predictor', productData);
  return result.predictions[0]?.revenue_predict || 0;
}

// React Hook for MindsDB
export function useMindsDB(auth0Token?: string) {
  const client = getMindsDBClient();
  
  if (auth0Token) {
    client.setAuthToken(auth0Token);
  }

  return {
    client,
    createCloudDatasource: (provider: 'aws' | 'gcp' | 'azure' | 'oci', name: string, config: Record<string, string>) =>
      createCloudDatasource(client, provider, name, config),
    trainSalesPredictor: (datasource: string, tableName: string) =>
      trainSalesPredictor(client, datasource, tableName),
    predictSales: (productData: { product: string; region: string; month: string }) =>
      predictSales(client, productData),
  };
}
