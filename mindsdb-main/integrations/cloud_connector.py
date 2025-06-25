"""
MindsDB Multi-Cloud Connector.

Dynamically connects to cloud providers based on environment configuration.
"""

import os
from typing import Dict, Any, Optional, List
from dataclasses import dataclass
from pathlib import Path
import logging

# Conditional imports to handle missing dependencies
try:
    import boto3
except ImportError:
    boto3 = None

try:
    from google.cloud import storage as gcs
except ImportError:
    gcs = None

try:
    from azure.storage.blob import BlobServiceClient
except ImportError:
    BlobServiceClient = None

try:
    import oci
except ImportError:
    oci = None
    
try:
    import ibm_boto3
    from ibm_botocore.client import Config
except ImportError:
    ibm_boto3 = None

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

@dataclass
class CloudProviderConfig:
    name: str
    enabled: bool = False
    required_vars: List[str] = None
    optional_vars: Dict[str, Any] = None
    
    def is_configured(self) -> bool:
        """Check if all required environment variables are set"""
        if not self.enabled:
            return False
            
        if not self.required_vars:
            return True
            
        return all(os.getenv(var) for var in self.required_vars)

class CloudConnector:
    def __init__(self, config_path: str = None):
        """
        Initialize the CloudConnector with configuration from environment variables
        or a configuration file.
        
        Args:
            config_path: Optional path to a configuration file (JSON or .env format)
        """
        self.providers: Dict[str, Any] = {}
        self.provider_configs = self._initialize_provider_configs()
        self._load_config(config_path)
        self._initialize_providers()
    
    def _initialize_provider_configs(self) -> Dict[str, CloudProviderConfig]:
        """Initialize configuration for all supported cloud providers"""
        return {
            'aws': CloudProviderConfig(
                name='AWS',
                required_vars=[
                    'AWS_ACCESS_KEY_ID',
                    'AWS_SECRET_ACCESS_KEY',
                    'AWS_DEFAULT_REGION'
                ]
            ),
            'gcp': CloudProviderConfig(
                name='GCP',
                required_vars=[
                    'GOOGLE_APPLICATION_CREDENTIALS',
                    'GCP_PROJECT_ID'
                ]
            ),
            'azure': CloudProviderConfig(
                name='Azure',
                required_vars=[
                    'AZURE_TENANT_ID',
                    'AZURE_CLIENT_ID',
                    'AZURE_CLIENT_SECRET',
                    'AZURE_SUBSCRIPTION_ID'
                ]
            ),
            'oci': CloudProviderConfig(
                name='OCI',
                required_vars=[
                    'OCI_USER_OCID',
                    'OCI_TENANCY_OCID',
                    'OCI_FINGERPRINT',
                    'OCI_REGION',
                    'OCI_COMPARTMENT_ID',
                    'OCI_KEY_FILE'
                ]
            ),
            'ibm': CloudProviderConfig(
                name='IBM',
                required_vars=[
                    'IBMCLOUD_API_KEY',
                    'IBMCLOUD_REGION',
                    'IBMCLOUD_RESOURCE_GROUP'
                ],
                optional_vars={
                    'IBMCLOUD_COS_ENDPOINT': 's3.us-south.cloud-object-storage.appdomain.cloud',
                    'IBMCLOUD_COS_BUCKET': None
                }
            )
        }
    
    def _load_config(self, config_path: str = None) -> None:
        """Load configuration from environment variables or config file"""
        # First, check environment variables for provider flags
        for provider in self.provider_configs.values():
            env_var = f'ENABLE_{provider.name.upper()}'
            provider.enabled = os.getenv(env_var, 'false').lower() == 'true'
        
        # If config path is provided, load additional settings
        if config_path and os.path.exists(config_path):
            self._load_config_file(config_path)
    
    def _load_config_file(self, config_path: str) -> None:
        """Load configuration from a file (JSON or .env format)"""
        try:
            if config_path.endswith('.json'):
                with open(config_path, 'r') as f:
                    config = json.load(f)
                
                # Update provider enabled status from config file
                for provider_name, provider in self.provider_configs.items():
                    if provider_name in config:
                        provider.enabled = config[provider_name].get('enabled', provider.enabled)
                        
            elif config_path.endswith('.env'):
                with open(config_path, 'r') as f:
                    for line in f:
                        line = line.strip()
                        if line and not line.startswith('#'):
                            key, value = line.split('=', 1)
                            os.environ[key] = value.strip('"\'')
                
                # Re-check environment variables after loading .env
                for provider in self.provider_configs.values():
                    env_var = f'ENABLE_{provider.name.upper()}'
                    provider.enabled = os.getenv(env_var, 'false').lower() == 'true'
                    
        except Exception as e:
            logger.warning(f"Failed to load config from {config_path}: {e}")
    
    def _initialize_providers(self):
        """Initialize only the enabled and properly configured cloud providers"""
        for provider_name, config in self.provider_configs.items():
            if config.enabled and config.is_configured():
                try:
                    init_method = getattr(self, f'_init_{provider_name.lower()}')
                    init_method()
                    logger.info(f"Successfully initialized {config.name} provider")
                except Exception as e:
                    logger.error(f"Failed to initialize {config.name} provider: {e}")
            elif config.enabled:
                logger.warning(
                    f"{config.name} is enabled but missing required configuration. "
                    f"Required vars: {', '.join(config.required_vars)}"
                )
    
    def _init_aws(self):
        """Initialize AWS connection"""
        if boto3 is None:
            raise ImportError("boto3 is required for AWS support")
            
        aws_config = self.provider_configs['aws']
        
        # Initialize AWS session with credentials from environment variables
        session = boto3.Session(
            aws_access_key_id=os.getenv('AWS_ACCESS_KEY_ID'),
            aws_secret_access_key=os.getenv('AWS_SECRET_ACCESS_KEY'),
            region_name=os.getenv('AWS_DEFAULT_REGION', 'us-east-1')
        )
        
        # Initialize AWS services
        self.providers['aws'] = {
            's3': session.client('s3'),
            'dynamodb': session.client('dynamodb'),
            'rds': session.client('rds'),
            'redshift': session.client('redshift'),
            'athena': session.client('athena'),
            'session': session
        }
    
    def _init_gcp(self):
        """Initialize GCP connection"""
        if gcs is None:
            raise ImportError("google-cloud-storage is required for GCP support")
            
        gcp_config = self.provider_configs['gcp']
        
        # Initialize GCP clients
        self.providers['gcp'] = {
            'storage': gcs.Client(),
            'project_id': os.getenv('GCP_PROJECT_ID')
            # Additional GCP services can be added here
        }
    
    def _init_azure(self):
        """Initialize Azure connection"""
        if BlobServiceClient is None or ClientSecretCredential is None:
            raise ImportError("azure-storage-blob and azure-identity are required for Azure support")
            
        azure_config = self.provider_configs['azure']
        
        # Try connection string first, then service principal
        connection_string = os.getenv('AZURE_STORAGE_CONNECTION_STRING')
        if connection_string:
            self.providers['azure'] = {
                'blob': BlobServiceClient.from_connection_string(connection_string),
                'auth_method': 'connection_string'
            }
        else:
            # Use service principal authentication
            credential = ClientSecretCredential(
                tenant_id=os.getenv('AZURE_TENANT_ID'),
                client_id=os.getenv('AZURE_CLIENT_ID'),
                client_secret=os.getenv('AZURE_CLIENT_SECRET')
            )
            self.providers['azure'] = {
                'credential': credential,
                'subscription_id': os.getenv('AZURE_SUBSCRIPTION_ID'),
                'auth_method': 'service_principal'
            }
            print(f"✗ Azure initialization failed: {e}")
    
    def _init_oci(self):
        """Initialize OCI connection"""
        if oci is None:
            raise ImportError("oci is required for Oracle Cloud Infrastructure support")
            
        oci_config = self.provider_configs['oci']
        """List all active providers"""
        return list(self.providers.keys())
    
    def create_mindsdb_datasources(self, mindsdb_api):
        """Create MindsDB datasources for all active providers"""
        datasources = []
        
        # AWS datasources
        if 'aws' in self.providers:
            datasources.extend(self._create_aws_datasources(mindsdb_api))
        
        # GCP datasources
        if 'gcp' in self.providers:
            datasources.extend(self._create_gcp_datasources(mindsdb_api))
        
        # Azure datasources
        if 'azure' in self.providers:
            datasources.extend(self._create_azure_datasources(mindsdb_api))
        
        # OCI datasources
        if 'oci' in self.providers:
            datasources.extend(self._create_oci_datasources(mindsdb_api))
        
        return datasources
    
    def _create_aws_datasources(self, mindsdb_api):
        """Create AWS-specific datasources"""
        datasources = []
        
        # S3 datasource
        if os.getenv('AWS_S3_BUCKET'):
            datasources.append({
                'name': 'aws_s3_data',
                'engine': 's3',
                'parameters': {
                    'aws_access_key_id': os.getenv('AWS_ACCESS_KEY_ID'),
                    'aws_secret_access_key': os.getenv('AWS_SECRET_ACCESS_KEY'),
                    'bucket': os.getenv('AWS_S3_BUCKET'),
                    'region_name': os.getenv('AWS_DEFAULT_REGION')
                }
            })
        
        # RDS datasource
        if os.getenv('AWS_RDS_ENDPOINT'):
            datasources.append({
                'name': 'aws_rds_data',
                'engine': 'postgres',  # or mysql, depending on your RDS
                'parameters': {
                    'host': os.getenv('AWS_RDS_ENDPOINT'),
                    'port': os.getenv('AWS_RDS_PORT', '5432'),
                    'database': os.getenv('AWS_RDS_DATABASE'),
                    'user': os.getenv('AWS_RDS_USER'),
                    'password': os.getenv('AWS_RDS_PASSWORD')
                }
            })
        
        return datasources
    
    def _create_gcp_datasources(self, mindsdb_api):
        """Create GCP-specific datasources"""
        datasources = []
        
        # BigQuery datasource
        if os.getenv('GCP_PROJECT_ID'):
            datasources.append({
                'name': 'gcp_bigquery_data',
                'engine': 'bigquery',
                'parameters': {
                    'project_id': os.getenv('GCP_PROJECT_ID'),
                    'service_account_json': os.getenv('GOOGLE_APPLICATION_CREDENTIALS')
                }
            })
        
        return datasources
    
    def _create_azure_datasources(self, mindsdb_api):
        """Create Azure-specific datasources"""
        datasources = []
        
        # Azure SQL datasource
        if os.getenv('AZURE_SQL_SERVER'):
            datasources.append({
                'name': 'azure_sql_data',
                'engine': 'mssql',
                'parameters': {
                    'host': os.getenv('AZURE_SQL_SERVER'),
                    'database': os.getenv('AZURE_SQL_DATABASE'),
                    'user': os.getenv('AZURE_SQL_USER'),
                    'password': os.getenv('AZURE_SQL_PASSWORD'),
                    'encrypt': True
                }
            })
        
        return datasources
    
    def _create_oci_datasources(self, mindsdb_api):
        """Create OCI-specific datasources"""
        datasources = []
        
        # OCI Autonomous Database
        if os.getenv('OCI_ADB_CONNECTION_STRING'):
            datasources.append({
                'name': 'oci_adb_data',
                'engine': 'oracle',
                'parameters': {
                    'dsn': os.getenv('OCI_ADB_CONNECTION_STRING'),
                    'user': os.getenv('OCI_ADB_USER'),
                    'password': os.getenv('OCI_ADB_PASSWORD')
                }
            })
        
        return datasources


# Example usage
if __name__ == "__main__":
    connector = CloudConnector()
    print(f"Active providers: {connector.list_active_providers()}")
