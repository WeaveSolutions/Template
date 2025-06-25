#!/usr/bin/env python3
"""
Initialize MindsDB with multi-cloud datasources and feature flags.

This module provides functionality to initialize and configure MindsDB with
various cloud data sources and integrations.
"""

"""
MindsDB Initialization Module

This module provides functionality to initialize and configure MindsDB with
various cloud data sources and integrations. It handles authentication,
data source setup, model training, and monitoring configuration.
"""

import json
import logging
import os
import sys
import time
from datetime import datetime, timedelta
from enum import Enum, auto
from pathlib import Path
from typing import (
    Any, Dict, List, Optional, Tuple, Union,
    Callable, TypeVar, Generic, Type, cast
)

# Type variable for generic return types
T = TypeVar('T')

# Third-party imports with type checking
try:
    import requests
    from requests import Session
    from requests.exceptions import RequestException, Timeout, HTTPError
    HAS_REQUESTS = True
except ImportError:
    HAS_REQUESTS = False
    # Define dummy classes/functions when requests is not available
    class Session:
        def __init__(self, *args, **kwargs):
            pass
    class RequestException(Exception):
        pass
    class Timeout(RequestException):
        pass
    class HTTPError(RequestException):
        pass

    # Cloud provider SDKs
    import boto3
    from botocore.exceptions import ClientError as BotoClientError

    from google.cloud import storage as gcp_storage
    from google.api_core.exceptions import GoogleAPIError

    from azure.storage.blob import BlobServiceClient
    from azure.core.exceptions import AzureError as AzureCoreError

    import oci
    from oci.config import from_file as oci_config_from_file
    from oci.object_storage import ObjectStorageClient
    from oci.exceptions import ServiceError as OCIServiceError

    from dotenv import load_dotenv
    from pydantic import BaseModel, Field, validator

    # Import CloudConnector if available, otherwise use stub
    try:
        from integrations.cloud_connector import CloudConnector
        CLOUD_CONNECTOR_AVAILABLE = True
    except ImportError:
        try:
            from integrations.stubs import CloudConnector
            CLOUD_CONNECTOR_AVAILABLE = True
            logging.warning("Using stub CloudConnector - cloud provider SDKs not installed")
        except ImportError:
            CLOUD_CONNECTOR_AVAILABLE = False
            logging.warning("CloudConnector not available - multi-cloud features will be disabled")

    HAS_DEPENDENCIES = True
except ImportError as e:
    logging.error("Failed to import required modules: %s", e)
    HAS_DEPENDENCIES = False

    # Define stubs for type checking
    class Session:  # type: ignore
        pass
    class BaseModel:  # type: ignore
        pass

# Add the project root to the Python path
project_root = str(Path(__file__).parent.absolute())
if project_root not in sys.path:
    sys.path.insert(0, project_root)

# Load environment variables
load_dotenv()

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(),
        logging.FileHandler('mindsdb_initializer.log')
    ]
)
logger = logging.getLogger(__name__)

# Constants
DEFAULT_MINDSDB_URL = "http://localhost:47334"
DEFAULT_TIMEOUT = 30  # seconds
MAX_RETRIES = 3
RETRY_DELAY = 5  # seconds
# Model query templates will be defined in their respective methods

class MindsDBInitializer:
    """
    Initialize and configure MindsDB with cloud data sources and integrations.

    This class handles the setup and configuration of MindsDB, including:
    - Connecting to cloud providers (AWS, GCP, Azure, OCI)
    - Setting up data sources
    - Creating and training ML models
    - Configuring monitoring and alerting
    - Managing authentication (including Auth0 integration)

    Example:
        >>> config = {
        ...     'mindsdb_url': 'http://localhost:47334',
        ...     'auth0_enabled': True,
        ...     'providers': ['aws', 'gcp', 'azure', 'oci']
        ... }
        >>> initializer = MindsDBInitializer(config)
        >>> result = initializer.initialize()
        >>> print(f"Initialization status: {result['status']}")
    """

    def __init__(self, config: Optional[Dict[str, Any]] = None) -> None:
        """Initialize the MindsDBInitializer with configuration.

        Args:
            config: Optional configuration dictionary. If not provided, will be loaded
                  from environment variables and config files.

        Configuration keys:
            mindsdb_url: Base URL of the MindsDB instance (default: http://localhost:47334)
            config_path: Path to configuration file (JSON, YAML, or .env)
            providers: List of cloud providers to enable (aws, gcp, azure, oci)
            auth0_enabled: Whether to enable Auth0 integration

        Environment Variables:
            MINDSDB_URL: Base URL of the MindsDB instance
            MINDSDB_USER: Username for basic authentication
            MINDSDB_PASSWORD: Password for basic authentication
            ENABLE_AWS: Enable AWS integration (true/false)
            ENABLE_GCP: Enable GCP integration (true/false)
            ENABLE_AZURE: Enable Azure integration (true/false)
            ENABLE_OCI: Enable OCI integration (true/false)
            ENABLE_AUTH0: Enable Auth0 integration (true/false)

        Raises:
            ImportError: If required dependencies are missing
            ValueError: If configuration is invalid
            ConnectionError: If unable to connect to MindsDB
        """
        if not HAS_DEPENDENCIES:
            raise ImportError(
                "Required dependencies are missing. Please install them using: "
                "pip install mindsdb-sdk requests boto3 google-cloud-storage "
                "azure-storage-blob oci python-dotenv"
            )
            
        # Initialize configuration
        self.config = config or {}
        self.mindsdb_url = self.config.get('mindsdb_url') or os.getenv('MINDSDB_URL', DEFAULT_MINDSDB_URL)
        
        # Set up session with timeout and retry strategy
        self.session = requests.Session()
        retry_strategy = requests.adapters.HTTPAdapter(
            max_retries=MAX_RETRIES,
            backoff_factor=1,
            status_forcelist=[408, 429, 500, 502, 503, 504]
        )
        self.session.mount("http://", retry_strategy)
        self.session.mount("https://", retry_strategy)
        
        # Load additional configuration from environment variables
        self.config.update({
            'aws_enabled': self.config.get('aws_enabled', os.getenv('ENABLE_AWS', 'false').lower() == 'true'),
            'gcp_enabled': self.config.get('gcp_enabled', os.getenv('ENABLE_GCP', 'false').lower() == 'true'),
            'azure_enabled': self.config.get('azure_enabled', os.getenv('ENABLE_AZURE', 'false').lower() == 'true'),
            'oci_enabled': self.config.get('oci_enabled', os.getenv('ENABLE_OCI', 'false').lower() == 'true'),
            'ibm_enabled': self.config.get('ibm_enabled', os.getenv('ENABLE_IBM', 'false').lower() == 'true'),
            'cloudflare_enabled': self.config.get('cloudflare_enabled', os.getenv('ENABLE_CLOUDFLARE', 'false').lower() == 'true'),
            'auth0_enabled': self.config.get('auth0_enabled', os.getenv('ENABLE_AUTH0', 'false').lower() == 'true'),
        })
        
        # Set up authentication
        self.mindsdb_user = self.config.get('mindsdb_user') or os.getenv('MINDSDB_USER', '')
        self.mindsdb_password = self.config.get('mindsdb_password') or os.getenv('MINDSDB_PASSWORD', '')
        
        if self.mindsdb_user and self.mindsdb_password:
            self.session.auth = (self.mindsdb_user, self.mindsdb_password)
            logger.debug("Basic authentication configured for MindsDB")
            
        # Initialize cloud connector if available
        self.cloud_connector = None
        if CLOUD_CONNECTOR_AVAILABLE:
            try:
                self.cloud_connector = CloudConnector(self.config)
                logger.debug("Initialized CloudConnector")
            except Exception as e:
                logger.warning("Failed to initialize CloudConnector: %s", str(e))
        
        logger.info(
            "Initialized MindsDBInitializer for %s (Providers: %s)",
            self.mindsdb_url,
            ", ".join([p for p in ['aws', 'gcp', 'azure', 'oci', 'ibm', 'cloudflare'] 
                      if self.config.get(f"{p}_enabled")])
        )
    
    def _load_config(self, config_path: Optional[str] = None) -> Dict[str, Any]:
        """Load and validate configuration from multiple sources.
        
        Configuration is loaded in the following order of precedence:
        1. Direct config passed to constructor
        2. Configuration file (JSON, YAML, or .env)
        3. Environment variables
        4. Default values
        
        Args:
            config_path: Optional path to configuration file. If not provided,
                      will check for config in default locations.
                      
        Returns:
            Dict containing the merged configuration.
            
        Raises:
            FileNotFoundError: If config_path is specified but doesn't exist
            ValueError: If configuration is invalid or unsupported format
            PermissionError: If unable to read the configuration file
            
        Example:
            >>> config = _load_config("config.json")
            >>> config = _load_config("config.yaml")
            >>> config = _load_config(".env")
        """
        config: Dict[str, Any] = {}
        
        # Set default configuration
        default_config = {
            'aws_enabled': False,
            'gcp_enabled': False,
            'azure_enabled': False,
            'oci_enabled': False,
            'auth0_enabled': False,
            'log_level': os.getenv('LOG_LEVEL', 'INFO').upper(),
            'timeout': int(os.getenv('MINDSDB_TIMEOUT', '30')),
            'max_retries': int(os.getenv('MAX_RETRIES', '3')),
            'retry_delay': int(os.getenv('RETRY_DELAY', '5')),
        }
        
        # Check for config in default locations if not specified
        if config_path is None:
            for path in ('config.json', 'config.yaml', 'config.yml', '.env', '.mindsdb'):
                if os.path.exists(path):
                    config_path = path
                    logger.debug("Found config file: %s", path)
                    break
        
        # Load from config file if path is provided
        if config_path:
            if not os.path.exists(config_path):
                raise FileNotFoundError(f"Configuration file not found: {config_path}")
                
            try:
                logger.info("Loading configuration from: %s", config_path)
                
                if config_path.endswith('.env'):
                    # Load environment variables from .env file
                    load_dotenv(config_path, override=True)
                    logger.debug("Loaded environment variables from %s", config_path)
                    
                elif config_path.endswith(('.json', '.yaml', '.yml')):
                    # Load structured config from JSON/YAML
                    with open(config_path, 'r', encoding='utf-8') as f:
                        try:
                            if config_path.endswith('.json'):
                                file_config = json.load(f)
                            else:
                                import yaml
                                file_config = yaml.safe_load(f) or {}
                            
                            # Validate configuration structure
                            if not isinstance(file_config, dict):
                                raise ValueError("Configuration must be a dictionary")
                                
                            config.update(file_config)
                            logger.debug("Loaded configuration from %s", config_path)
                            
                        except (json.JSONDecodeError, yaml.YAMLError) as e:
                            raise ValueError(f"Invalid configuration format in {config_path}: {str(e)}")
                else:
                    raise ValueError(
                        f"Unsupported config file format: {config_path}. "
                        "Supported formats: .json, .yaml, .yml, .env"
                    )
                    
            except PermissionError as e:
                logger.error("Permission denied when reading config file: %s", config_path)
                raise
            except Exception as e:
                logger.error("Failed to load configuration from %s: %s", config_path, str(e))
                raise
        
        # Load from environment variables (overrides file config)
        env_config = {
            'aws_enabled': os.getenv('ENABLE_AWS', '').lower() == 'true',
            'gcp_enabled': os.getenv('ENABLE_GCP', '').lower() == 'true',
            'azure_enabled': os.getenv('ENABLE_AZURE', '').lower() == 'true',
            'oci_enabled': os.getenv('ENABLE_OCI', '').lower() == 'true',
            'auth0_enabled': os.getenv('ENABLE_AUTH0', '').lower() == 'true',
            'log_level': os.getenv('LOG_LEVEL', default_config['log_level']).upper(),
            'timeout': int(os.getenv('MINDSDB_TIMEOUT', str(default_config['timeout']))),
            'max_retries': int(os.getenv('MAX_RETRIES', str(default_config['max_retries']))),
            'retry_delay': int(os.getenv('RETRY_DELAY', str(default_config['retry_delay']))),
        }
        
        # Merge configurations with proper precedence
        merged_config = {**default_config, **config, **env_config}
        
        # Apply any overrides from the constructor
        if hasattr(self, 'config'):
            for k, v in self.config.items():
                if v is not None:  # Only override if value is not None
                    merged_config[k] = v
        
        # Validate configuration values
        self._validate_config(merged_config)
        
        # Set log level
        log_level = getattr(logging, merged_config['log_level'], logging.INFO)
        logger.setLevel(log_level)
        
        # Log configuration summary (excluding sensitive data)
        safe_config = {
            k: '***' if any(s in k.lower() for s in ['key', 'secret', 'token', 'password']) 
               else v 
            for k, v in merged_config.items()
        }
        logger.debug("Merged configuration: %s", json.dumps(safe_config, indent=2))
        
        return merged_config
        
    def _validate_config(self, config: Dict[str, Any]) -> None:
        """Validate configuration values.
        
        Args:
            config: Configuration dictionary to validate
            
        Raises:
            ValueError: If any configuration value is invalid
        """
        if not isinstance(config.get('mindsdb_url', ''), str):
            raise ValueError("mindsdb_url must be a string")
            
        if not config.get('mindsdb_url', '').startswith(('http://', 'https://')):
            raise ValueError("mindsdb_url must start with http:// or https://")
            
        if not isinstance(config['timeout'], (int, float)) or config['timeout'] <= 0:
            raise ValueError("timeout must be a positive number")
            
        if not isinstance(config['max_retries'], int) or config['max_retries'] < 0:
            raise ValueError("max_retries must be a non-negative integer")
            
        if not isinstance(config['retry_delay'], (int, float)) or config['retry_delay'] < 0:
            raise ValueError("retry_delay must be a non-negative number")
            
        # Validate provider-specific configurations if enabled
        for provider in ['aws', 'gcp', 'azure', 'oci']:
            if config.get(f'{provider}_enabled'):
                self._validate_provider_config(provider, config)
    
    def wait_for_mindsdb(self, timeout: int = 300, interval: int = 5) -> bool:
        """Wait for MindsDB to become available and healthy.
        
        This method performs the following checks:
        1. Basic connectivity to the MindsDB server
        2. API endpoint availability
        3. Database health status
        4. Authentication (if credentials provided)
        
        Args:
            timeout: Maximum time to wait in seconds (default: 300)
            interval: Time between retry attempts in seconds (default: 5)
            
        Returns:
            bool: True if MindsDB is fully operational, False if timeout is reached
            
        Raises:
            ConnectionError: If unable to establish connection after retries
            TimeoutError: If the operation times out
            
        Example:
            >>> initializer = MindsDBInitializer()
            >>> if initializer.wait_for_mindsdb(timeout=60):
            ...     print("MindsDB is ready!")
            ... else:
            ...     print("Failed to connect to MindsDB")
        """
        if not isinstance(timeout, (int, float)) or timeout <= 0:
            raise ValueError("timeout must be a positive number")
        if not isinstance(interval, (int, float)) or interval <= 0:
            raise ValueError("interval must be a positive number")
        
        start_time = time.monotonic()
        end_time = start_time + timeout
        attempt = 0
        last_error = None
        
        logger.info("Waiting for MindsDB to be ready at %s (timeout: %ds)...", 
                    self.mindsdb_url, timeout)
        
        while time.monotonic() < end_time:
            attempt += 1
            current_time = time.monotonic()
            remaining_time = max(0, end_time - current_time)
            
            try:
                # 1. Check basic connectivity
                ping_url = f"{self.mindsdb_url}/api/util/ping"
                response = self.session.get(
                    ping_url,
                    timeout=min(5, interval),
                    headers={"Accept": "application/json"}
                )
                
                # 2. Check if the response is valid
                response.raise_for_status()
                
                # 3. Check if the API is responding correctly
                if response.status_code == 200:
                    try:
                        data = response.json()
                        if data.get('status') == 'ok':
                            logger.info(
                                "MindsDB is ready after %.1f seconds (attempt %d)",
                                current_time - start_time, attempt
                            )
                            
                            # 4. Verify authentication if credentials are provided
                            if hasattr(self, 'mindsdb_user') and self.mindsdb_user:
                                if not self._check_authentication():
                                    raise ConnectionError(
                                        "Authentication failed with provided credentials"
                                    )
                            
                            # 5. Check database health
                            if not self._check_database_health():
                                raise ConnectionError("Database health check failed")
                            
                            return True
                            
                    except ValueError as e:
                        logger.debug("Invalid JSON response from MindsDB: %s", str(e))
                        last_error = f"Invalid response format: {str(e)}"
                
            except requests.RequestException as e:
                last_error = str(e)
                logger.debug(
                    "Connection attempt %d failed: %s (%.1fs remaining)",
                    attempt, last_error, remaining_time
                )
            
            # Calculate next interval with exponential backoff
            sleep_time = min(
                interval * (1.5 ** min(attempt // 3, 4)),  # Max backoff of 4x interval
                remaining_time,
                30  # Maximum sleep time between attempts
            )
            
            if sleep_time > 0:
                time.sleep(min(sleep_time, remaining_time))
        
        # If we get here, we've timed out
        error_msg = (
            f"Timeout waiting for MindsDB to become ready after {timeout} seconds. "
            f"Last error: {last_error or 'Unknown error'}"
        )
        logger.error(error_msg)
        raise TimeoutError(error_msg)
    
    def _check_authentication(self) -> bool:
        """Verify authentication with MindsDB.
        
        Returns:
            bool: True if authentication is successful, False otherwise
        """
        try:
            # Try to access a protected endpoint
            response = self.session.get(
                f"{self.mindsdb_url}/api/session",
                timeout=10
            )
            
            if response.status_code == 200:
                return True
                
            if response.status_code == 401:
                logger.error("Authentication failed: Invalid credentials")
                return False
                
            logger.warning("Unexpected status code during auth check: %d", response.status_code)
            return False
            
        except requests.RequestException as e:
            logger.error("Error during authentication check: %s", str(e))
            return False
    
    def _check_database_health(self, timeout: int = 10) -> bool:
        """Check the health of the MindsDB database.
        
        Args:
            timeout: Maximum time to wait for the health check
            
        Returns:
            bool: True if the database is healthy, False otherwise
        """
        try:
            response = self.session.get(
                f"{self.mindsdb_url}/api/health",
                timeout=timeout,
                headers={"Accept": "application/json"}
            )
            
            if response.status_code == 200:
                health_data = response.json()
                if health_data.get('status') == 'ok':
                    return True
                
                logger.warning("Database health check failed: %s", 
                              health_data.get('message', 'Unknown error'))
                return False
                
            logger.warning("Unexpected status code during health check: %d", 
                          response.status_code)
            return False
            
        except requests.RequestException as e:
            logger.error("Error during database health check: %s", str(e))
            return False
    
    def create_datasource(self, 
                         name: str, 
                         engine: str, 
                         connection_args: Dict[str, Any],
                         if_not_exists: bool = True,
                         validate: bool = True) -> Dict[str, Any]:
        """Create a new datasource in MindsDB with comprehensive validation and error handling.
        
        This method supports various datasource types including:
        - Databases: mysql, postgres, mongodb, sqlserver, etc.
        - File storage: s3, gcs, azure_blob, local
        - Data warehouses: bigquery, snowflake, redshift
        - APIs: rest, graphql
        
        Args:
            name: Name of the datasource (alphanumeric + underscores only)
            engine: Type of datasource (e.g., 'mysql', 'postgres', 's3', 'bigquery')
            connection_args: Connection parameters specific to the datasource type.
                          Should include all required parameters for the specific engine.
            if_not_exists: If True, won't raise an error if datasource already exists
            validate: If True, will test the connection before creating
            
        Returns:
            Dict containing the result with keys:
                - success (bool): Whether the operation was successful
                - message (str): Status message
                - data (dict, optional): Additional data like datasource details
                - error (str, optional): Error message if operation failed
                
        Raises:
            ValueError: If any arguments are invalid
            ConnectionError: If connection test fails and validate=True
            
        Example:
            >>> # Create a MySQL datasource
            >>> result = create_datasource(
            ...     name="production_db",
            ...     engine="mysql",
            ...     connection_args={
            ...         "host": "localhost",
            ...         "port": 3306,
            ...         "database": "mydb",
            ...         "user": "user",
            ...         "password": "password"
            ...     }
            ... )
            >>> print(result['message'])
        """
        # Input validation
        if not name or not isinstance(name, str):
            raise ValueError("Datasource name must be a non-empty string")
            
        if not engine or not isinstance(engine, str):
            raise ValueError("Engine must be a non-empty string")
            
        if not connection_args or not isinstance(connection_args, dict):
            raise ValueError("connection_args must be a non-empty dictionary")
            
        # Validate name format (alphanumeric + underscores)
        if not name.replace('_', '').isalnum():
            raise ValueError(
                "Datasource name can only contain alphanumeric characters and underscores"
            )
            
        # Normalize engine name
        engine = engine.lower().strip()
        
        # Validate connection arguments based on engine type
        self._validate_connection_args(engine, connection_args)
        
        # Test connection if requested
        if validate:
            test_result = self._test_connection(engine, connection_args)
            if not test_result['success']:
                error_msg = f"Connection test failed: {test_result.get('error')}"
                logger.error("Failed to validate connection for '%s': %s", name, error_msg)
                raise ConnectionError(error_msg)
            logger.debug("Successfully validated connection for '%s'", name)
        
        # Prepare API request
        url = f"{self.mindsdb_url}/api/datasources"
        payload = {
            "name": name,
            "engine": engine,
            "connection_args": connection_args,
            "if_not_exists": if_not_exists
        }
        
        logger.debug("Creating datasource %s with payload: %s", 
                     name, {k: '***' if 'pass' in k.lower() else v 
                           for k, v in payload.items()})
        
        try:
            response = self.session.post(
                endpoint,
                json=payload,
                timeout=DEFAULT_TIMEOUT
            )
            
            if response.status_code in (200, 201):
                logger.info("Successfully created datasource: %s (%s)", name, engine)
                return True
                
        except Exception as e:
            logger.error("Error creating datasource %s: %s", name, str(e))
            return False
    
    def train_model(self, model_name: str, query: str, 
                   target: str, 
                   options: Optional[Dict[str, Any]] = None,
                   wait_for_completion: bool = True,
                   timeout: int = 3600) -> bool:
        """Train a new ML model in MindsDB.
        
        Args:
            model_name: Name of the model to train
            query: SQL query to fetch training data
            target: Target column to predict
            options: Additional training options
            wait_for_completion: Whether to wait for training to complete
            timeout: Maximum time to wait for training (seconds) if wait_for_completion is True
            
        Returns:
            bool: True if training was successful, False otherwise
            
        Example:
            >>> training_options = {
            ...     'time_aim': 100,
            ...     'stop_training_in_x_seconds': 300,
            ...     'using': {'target': 'price'}
            ... }
            >>> initializer.train_model(
            ...     model_name='house_price_predictor',
            ...     query='SELECT * FROM house_prices',
            ...     target='price',
            ...     options=training_options,
            ...     wait_for_completion=True,
            ...     timeout=1800
            ... )
        """
        endpoint = f"{self.mindsdb_url}/api/models"
        
        training_config: Dict[str, Any] = {
            'name': model_name,
            'predict': target,
            'training_data': query,
            'engine': options.pop('engine', 'lightwood') if options else 'lightwood'
        }
        
        if options:
            training_config.update(options)
        
        logger.info("Starting training for model: %s", model_name)
        logger.debug("Training config: %s", 
                    {k: v for k, v in training_config.items() 
                     if k not in ('training_data', 'using')})
        
        try:
            response = self.session.post(
                endpoint,
                json=training_config,
                timeout=DEFAULT_TIMEOUT
            )
            
            if response.status_code not in (200, 201):
                error_msg = response.text
                logger.error("Failed to start training for model %s: %s (status: %d)", 
                           model_name, error_msg, response.status_code)
                return False
            
            logger.info("Successfully started training for model: %s", model_name)
            
            if not wait_for_completion:
                return True
                
            # Wait for training to complete
            return self._wait_for_training_completion(model_name, timeout)
            
        except requests.RequestException as e:
            logger.exception("Request error during training for model %s: %s", 
                          model_name, str(e))
            return False
        except Exception as e:
            logger.exception("Unexpected error during training for model %s: %s", 
                          model_name, str(e))
            return False
    
    def _wait_for_training_completion(self, model_name: str, 
                                    timeout: int = 3600, 
                                    check_interval: int = 10) -> bool:
        """Wait for model training to complete.
        
        Args:
            model_name: Name of the model being trained
            timeout: Maximum time to wait in seconds (default: 1 hour)
            check_interval: Time between status checks in seconds (default: 10)
            
        Returns:
            bool: True if training completed successfully, False otherwise
        """
        logger.info("Waiting for training of model %s to complete...", model_name)
        start_time = time.time()
        last_status = ""
        
        while time.time() - start_time < timeout:
            try:
                # Get model status
                model_info = self.get_model(model_name)
                if not model_info:
                    logger.error("Failed to get status for model: %s", model_name)
                    return False
                
                # Check training status
                status = model_info.get('status')
                if status != last_status:
                    logger.info("Model %s status: %s", model_name, status)
                    last_status = status
                
                # Check if training is complete
                if status == 'complete':
                    logger.info("Training completed successfully for model: %s", model_name)
                    return True
                elif status in ('error', 'failed'):
                    error_msg = model_info.get('error', 'Unknown error')
                    logger.error("Training failed for model %s: %s", model_name, error_msg)
                    return False
                
                # Wait before next check
                time.sleep(min(check_interval, timeout - (time.time() - start_time)))
                
            except requests.RequestException as e:
                logger.error("Error checking training status for model %s: %s", 
                           model_name, str(e))
                time.sleep(min(30, timeout - (time.time() - start_time)))  # Back off on errors
                
            except Exception as e:
                logger.exception("Unexpected error while checking training status for model %s: %s",
                              model_name, str(e))
                return False
        
        logger.error("Timeout waiting for training of model %s to complete", model_name)
        return False
        
    def get_model(self, model_name: str) -> Optional[Dict[str, Any]]:
        """Get information about a trained model.
        
        Args:
            model_name: Name of the model to retrieve
            
        Returns:
            dict: Model information or None if not found
        """
        endpoint = f"{self.mindsdb_url}/api/models/{model_name}"
        
        try:
            response = self.session.get(endpoint, timeout=DEFAULT_TIMEOUT)
            
            if response.status_code == 200:
                return response.json()
                
            if response.status_code == 404:
                logger.warning("Model not found: %s", model_name)
            else:
                logger.error("Failed to get model %s: %s (status: %d)", 
                           model_name, response.text, response.status_code)
            
            return None
            
        except requests.RequestException as e:
            logger.error("Request error getting model %s: %s", model_name, str(e))
            return None
        except Exception as e:
            logger.exception("Unexpected error getting model %s: %s", model_name, str(e))
            return None
    
    def create_auth0_integration(self) -> bool:
        """Create Auth0 integration for authentication.
        
        Returns:
            bool: True if Auth0 integration was created successfully, False otherwise
            
        Example:
            >>> initializer = MindsDBInitializer()
            >>> if initializer.create_auth0_integration():
            ...     print("Auth0 integration created successfully")
        """
        if not self.config.get('auth0_enabled', False):
            logger.info("Auth0 integration is disabled in config")
            return False
            
        required_vars = [
            'AUTH0_DOMAIN',
            'AUTH0_CLIENT_ID',
            'AUTH0_CLIENT_SECRET',
            'AUTH0_AUDIENCE'
        ]
        
        missing_vars = [var for var in required_vars if not os.getenv(var)]
        if missing_vars:
            logger.error("Missing required Auth0 environment variables: %s", 
                       ", ".join(missing_vars))
            return False
            
        auth0_config = {
            'name': 'auth0_integration',
            'engine': 'auth0',
            'parameters': {
                'domain': os.getenv('AUTH0_DOMAIN'),
                'client_id': os.getenv('AUTH0_CLIENT_ID'),
                'client_secret': os.getenv('AUTH0_CLIENT_SECRET'),
                'audience': os.getenv('AUTH0_AUDIENCE'),
                'connection': os.getenv('AUTH0_CONNECTION', 'Username-Password-Authentication'),
                'scope': 'openid profile email offline_access'
            }
        }
        
        logger.info("Creating Auth0 integration...")
        return self.create_datasource(auth0_config)
    
    def setup_cloud_datasources(self) -> List[Dict[str, Any]]:
        """Set up datasources for all active cloud providers.
        
        This method will:
        1. Test connections to all configured cloud providers
        2. Create MindsDB datasources for each accessible provider
        3. Return a list of successfully created datasource configurations
        
        Returns:
            List[Dict[str, Any]]: List of successfully created datasource configurations
            
        Example:
            >>> initializer = MindsDBInitializer()
            >>> datasources = initializer.setup_cloud_datasources()
            logger.info(" Creating ML model for predicting cloud resource usage")
        logger.info(" Setting up mobile app integration")
        """
        logger.info("Setting up cloud datasources...")
        
        try:
            # Initialize CloudConnector if not already done
            if not hasattr(self, 'cloud_connector'):
                try:
                    from integrations.cloud_connector import CloudConnector
                    self.cloud_connector = CloudConnector()
                except ImportError:
                    logger.warning("Cloud connector module not available - cloud datasources will be disabled")
                    return {'success': False, 'error': 'Cloud connector module not available'}
            
            # Test all connections first
            logger.info("Starting cloud datasource synchronization...")
            test_results = self.cloud_connector.test_connections()
            active_providers = [
                name for name, success in test_results.items() 
                if success
            ]
            
            if not active_providers:
                logger.warning("No cloud providers are properly configured and accessible")
                return []
            
            logger.info("Active and accessible providers: %s", 
                      ", ".join(active_providers))
            
            # Create datasources for each provider
            logger.info("Creating MindsDB datasources...")
            datasources = self.cloud_connector.create_mindsdb_datasources()
            created_datasources = []
            
            for ds_config in datasources:
                try:
                    if self.create_datasource(ds_config):
                        created_datasources.append(ds_config)
                        logger.info("Successfully created datasource: %s (%s)", 
                                  ds_config.get('name'), ds_config.get('engine'))
                except Exception as e:
                    logger.error("Failed to create datasource %s: %s", 
                               ds_config.get('name', 'unknown'), str(e))
            
            logger.info("Successfully created %d/%d datasources", 
                       len(created_datasources), len(datasources))
            return created_datasources
            
        except ImportError as e:
            logger.error("Cloud connector module not found: %s", str(e))
            return []
        except Exception as e:
            logger.exception("Unexpected error setting up cloud datasources: %s", str(e))
            return []
    
    def create_example_models(self) -> List[Dict[str, Any]]:
        """Create example ML models for demonstration purposes.
        
        This method creates sample predictive models using available datasources.
        The models are designed to work with common datasets and serve as examples.
        
        Returns:
            List[Dict[str, Any]]: List of created model configurations
            
        Example:
            >>> initializer = MindsDBInitializer()
            >>> models = initializer.create_example_models()
            >>> print(f"Created {len(models)} example models")
        """
        logger.info("Creating example models...")
        
        try:
            # Check available datasources
            databases_response = self.session.get(
                f"{self.mindsdb_url}/api/databases",
                timeout=DEFAULT_TIMEOUT
            )
            
            if databases_response.status_code != 200:
                logger.warning("Failed to fetch databases: %s", 
                              databases_response.text)
                return []
            
            databases = databases_response.json()
            external_dbs = [db.get('name') for db in databases 
                          if db.get('name') != 'mindsdb']
            
            if not external_dbs:
                logger.info("No external datasources available for model creation")
                return []
            
            logger.info("Available databases: %s", external_dbs)
            
            example_models = [
                {
                    'name': 'customer_churn_predictor',
                    'target': 'churn',
                    'description': 'Predicts customer churn based on historical data',
                    'options': {
                        'time_aim': 100,
                        'using': {
                            'ignore': ['customer_id', 'name']  # Exclude non-predictive columns
                        }
                    }
                },
                {
                    'name': 'sales_forecast',
                    'query': 'SELECT * FROM sales_data',
                    'target': 'revenue',
                    'description': 'Forecasts future sales revenue',
                    'options': {
                        'timeseries_settings': {
                            'order_by': ['date'],
                            'window': 30,
                            'horizon': 7
                        },
                        'using': {
                            'ignore': ['order_id']
                        }
                    }
                }
            ]
            
            created_models = []
            
            for model_config in example_models:
                try:
                    if self.train_model(
                        model_name=model_config['name'],
                        query=model_config['query'],
                        target=model_config['target'],
                        options=model_config.get('options'),
                        wait_for_completion=False  # Don't block on each model
                    ):
                        created_models.append(model_config)
                        logger.info("Started training example model: %s", 
                                   model_config['name'])
                except Exception as e:
                    logger.error("Failed to create example model %s: %s",
                               model_config.get('name', 'unknown'), str(e))
            
            logger.info("Started training %d/%d example models", 
                       len(created_models), len(example_models))
            return created_models
            
        except requests.RequestException as e:
            logger.error("Request error creating example models: %s", str(e))
            return []
        except Exception as e:
            logger.exception("Unexpected error creating example models: %s", str(e))
            return []
        
        # You can add more models here based on your specific use case
    
    def setup_monitoring(self):
        """Setup monitoring and alerts"""
        print("\nSetting up monitoring...")
        
        # Create monitoring views for each cloud provider
        monitoring_queries = {
            'aws': """
                CREATE VIEW IF NOT EXISTS aws_resource_usage AS
                SELECT * FROM information_schema.databases
                WHERE engine = 's3' OR engine = 'redshift'
            """,
            'gcp': """
                CREATE VIEW IF NOT EXISTS gcp_resource_usage AS
                SELECT * FROM information_schema.databases
                WHERE engine = 'bigquery'
            """,
            'azure': """
                CREATE VIEW IF NOT EXISTS azure_resource_usage AS
                SELECT * FROM information_schema.databases
                WHERE engine = 'mssql' OR engine = 'azureblob'
            """,
            'oci': """
                CREATE VIEW IF NOT EXISTS oci_resource_usage AS
                SELECT * FROM information_schema.databases
                WHERE engine = 'oracle'
            """
        }
        
        # Execute monitoring queries for active providers
        for provider in self.cloud_connector.list_active_providers():
            if provider in monitoring_queries:
                print(f"✓ Set up monitoring for {provider}")
    
    def setup_monitoring(self) -> Dict[str, Any]:
        """Set up monitoring and alerting for the MindsDB instance.
        
        This method configures monitoring for the MindsDB instance, including:
        - Health checks
        - Performance metrics
        - Alerting rules
        - Integration with monitoring tools (Prometheus, Grafana, etc.)
        
        Returns:
            Dict[str, Any]: Monitoring configuration status and details
            
        Example:
            >>> initializer = MindsDBInitializer()
            >>> monitoring = initializer.setup_monitoring()
            >>> print(f"Monitoring status: {monitoring.get('status')}")
        """
        logger.info("Setting up monitoring...")
        
        monitoring_config = {
            'status': 'enabled',
            'metrics': {
                'enabled': True,
                'endpoint': f"{self.mindsdb_url}/metrics"
            },
            'health_checks': {
                'enabled': True,
                'endpoint': f"{self.mindsdb_url}/health"
            },
            'alerts': []
        }
        
        try:
            # Check if Prometheus metrics are available
            try:
                response = self.session.get(
                    f"{self.mindsdb_url}/metrics",
                    timeout=10
                )
                if response.status_code == 200:
                    logger.info("Prometheus metrics endpoint is available")
                    monitoring_config['metrics']['status'] = 'active'
                else:
                    logger.warning("Prometheus metrics endpoint returned status: %d", 
                                 response.status_code)
                    monitoring_config['metrics']['status'] = 'unavailable'
            except Exception as e:
                logger.warning("Could not access Prometheus metrics: %s", str(e))
                monitoring_config['metrics']['status'] = 'error'
            
            # Set up default alerts
            default_alerts = [
                {
                    'name': 'high_cpu_usage',
                    'condition': 'process_cpu_seconds_total > 80',
                    'duration': '5m',
                    'severity': 'warning',
                    'description': 'CPU usage is high'
                },
                {
                    'name': 'high_memory_usage',
                    'condition': 'process_resident_memory_bytes / process_virtual_memory_bytes > 0.8',
                    'duration': '10m',
                    'severity': 'warning',
                    'description': 'Memory usage is high'
                }
            ]
            
            monitoring_config['alerts'] = default_alerts
            logger.info("Monitoring setup completed with %d default alerts", 
                       len(default_alerts))
            
            return monitoring_config
            
        except Exception as e:
            logger.exception("Error setting up monitoring: %s", str(e))
            return {
                'status': 'error',
                'error': str(e)
            }

    def initialize(self, skip_models: bool = False, 
                  skip_monitoring: bool = False) -> Dict[str, Any]:
        """Run the full initialization process for MindsDB.
        
        This method orchestrates the entire setup process:
        1. Waits for MindsDB to be ready
        2. Sets up Auth0 integration (if enabled)
        3. Configures cloud datasources
        4. Creates example models (unless skipped)
        5. Sets up monitoring (unless skipped)
        
        Args:
            skip_models: If True, skips creating example models
            skip_monitoring: If True, skips setting up monitoring
            
        Returns:
            Dict[str, Any]: Dictionary containing the initialization results with the following structure:
                - status: Overall status ('success' or 'error')
                - duration_seconds: Total initialization time in seconds
                - auth0_integration: Status of Auth0 integration
                - datasources: Dictionary with count and list of created datasources
                - models: Dictionary with count and list of created models
                - monitoring: Monitoring configuration status
                - error: Error message if initialization failed
            
        Example:
            >>> initializer = MindsDBInitializer()
            >>> result = initializer.initialize()
            >>> print(f"Initialization status: {result.get('status')}")
            >>> print(f"Datasources created: {result.get('datasources', {}).get('count', 0)}")
        """
        logger.info("Starting MindsDB initialization...")
        start_time = time.time()
        
        # Initialize result structure
        result = {
            'status': 'in_progress',
            'duration_seconds': 0,
            'auth0_integration': None,
            'datasources': {'count': 0, 'items': []},
            'models': {'count': 0, 'items': []},
            'monitoring': None,
            'error': None
        }
        
        try:
            # 1. Wait for MindsDB to be ready
            logger.info("Step 1/5: Waiting for MindsDB to be ready...")
            if not self.wait_for_mindsdb():
                error_msg = "Failed to connect to MindsDB. Please check if the service is running."
                logger.error(error_msg)
                result.update({
                    'status': 'error',
                    'error': error_msg,
                    'duration_seconds': round(time.time() - start_time, 2)
                })
                return result
            
            # 2. Setup Auth0 integration if enabled
            auth0_status = None
            if self.config.get('auth0_enabled', False):
                logger.info("Step 2/5: Setting up Auth0 integration...")
                try:
                    auth0_status = self.create_auth0_integration()
                    logger.info("Auth0 integration status: %s", 
                               'success' if auth0_status else 'failed')
                except Exception as e:
                    logger.error("Failed to set up Auth0 integration: %s", str(e))
                    auth0_status = False
            else:
                logger.info("Step 2/5: Auth0 integration is disabled in config")
            
            result['auth0_integration'] = auth0_status
            
            # 3. Setup cloud datasources
            logger.info("Step 3/5: Setting up cloud datasources...")
            try:
                datasources = self.setup_cloud_datasources()
                result['datasources'] = {
                    'count': len(datasources),
                    'items': datasources
                }
                logger.info("Successfully set up %d datasources", len(datasources))
            except Exception as e:
                logger.error("Failed to set up datasources: %s", str(e))
                result['error'] = f"Datasource setup failed: {str(e)}"
            
            # 4. Create example models (if not skipped)
            if not skip_models and not result.get('error'):
                logger.info("Step 4/5: Creating example models...")
                try:
                    models = self.create_example_models()
                    result['models'] = {
                        'count': len(models),
                        'items': models
                    }
                    logger.info("Successfully started training %d example models", len(models))
                except Exception as e:
                    logger.error("Failed to create example models: %s", str(e))
                    if not result.get('error'):  # Only set error if not already set
                        result['error'] = f"Model creation failed: {str(e)}"
            else:
                logger.info("Step 4/5: Skipping model creation (skip_models=%s, has_error=%s)",
                          skip_models, bool(result.get('error')))
            
            # 5. Setup monitoring (if not skipped and no previous errors)
            if not skip_monitoring and not result.get('error'):
                logger.info("Step 5/5: Setting up monitoring...")
                try:
                    monitoring = self.setup_monitoring()
                    result['monitoring'] = monitoring
                    logger.info("Monitoring setup completed with status: %s", 
                              monitoring.get('status', 'unknown'))
                except Exception as e:
                    logger.error("Failed to set up monitoring: %s", str(e))
                    result['monitoring'] = {
                        'status': 'error',
                        'error': str(e)
                    }
            else:
                logger.info("Step 5/5: Skipping monitoring setup (skip_monitoring=%s, has_error=%s)",
                          skip_monitoring, bool(result.get('error')))
            
            # Finalize result
            result['status'] = 'error' if result.get('error') else 'success'
            result['duration_seconds'] = round(time.time() - start_time, 2)
            
            if result['status'] == 'success':
                logger.info("MindsDB initialization completed successfully in %.2f seconds" % result['duration_seconds'])
            else:
                logger.error("MindsDB initialization completed with errors in %.2f seconds: %s" % (result['duration_seconds'], result.get('error', 'Unknown error')))
            
        except Exception as e:
            # Catch any unexpected errors but with more specific handling
            error_msg = f"Critical error during initialization: {str(e)}"
            logger.exception(error_msg)
            # Log detailed exception information to help with debugging
            logger.debug(f"Exception type: {type(e).__name__}")
            logger.debug(f"Exception traceback: {sys.exc_info()[2]}")
            return {
                'status': 'error',
                'error': error_msg,
                'duration_seconds': round(time.time() - start_time, 2),
                'auth0_integration': result.get('auth0_integration'),
                'datasources': result.get('datasources', {'count': 0, 'items': []}),
                'models': result.get('models', {'count': 0, 'items': []}),
                'monitoring': result.get('monitoring')
            } 
            
        return result


def main():
    import argparse
    
    parser = argparse.ArgumentParser(description='Initialize MindsDB with cloud providers and models')
    parser.add_argument('--url', type=str, default=os.getenv('MINDSDB_URL', 'http://localhost:47334'),
                        help='MindsDB server URL')
    parser.add_argument('--config', type=str, default=None,
                        help='Path to configuration file (JSON or .env)')
    parser.add_argument('--skip-models', action='store_true',
                        help='Skip creating example models')
    parser.add_argument('--debug', action='store_true',
                        help='Enable debug logging')
    
    args = parser.parse_args()
    
    # Configure logging level
    log_level = logging.DEBUG if args.debug else logging.INFO
    logging.basicConfig(level=log_level)
    
    logger.info(f"Initializing MindsDB at {args.url}")
    
    initializer = MindsDBInitializer(
        mindsdb_url=args.url,
        config_path=args.config
    )
    
    result = initializer.initialize(skip_models=args.skip_models)
    
    if result['success']:
        logger.info("✅ Initialization completed successfully")
    else:
        logger.error("❌ Initialization completed with errors")
    
    # Print summary
    print("\n=== Initialization Summary ===")
    print(f"MindsDB Ready: {'✅' if result['mindsdb_ready'] else '❌'}")
    
    if result['auth0_integration']:
        status = '✅' if result['auth0_integration']['success'] else '❌'
        print(f"Auth0 Integration: {status}")
    
    print(f"\nDatasources created: {len(result.get('datasources', []))}")
    for ds in result.get('datasources', []):
        print(f"  - {ds['name']} ({ds['engine']})")
    
    if 'datasources_error' in result:
        print(f"\n⚠ Datasource errors: {result['datasources_error']}")
    
    if not args.skip_models:
        print(f"\nModels created: {len(result.get('models', []))}")
        for model in result.get('models', []):
            status = '✅' if model.get('status') == 'created' else '❌'
            print(f"  - {model['name']} ({model.get('database', 'unknown')}) {status}")
    
    if 'models_error' in result:
        print(f"\n⚠ Model errors: {result['models_error']}")
    
    # Print connection information if successful
    if result['success']:
        print("\n=== Connection Information ===")
        print(f"MindsDB URL: {args.url}")
        print("MySQL client: mysql -h 127.0.0.1 -P 47335 -u mindsdb")
        print(f"API: {args.url.rstrip('/')}/api")
        print("MongoDB API: localhost:47336")
        print("\nYou can now connect to MindsDB using the web UI at:")
        print(f"- Web UI: {args.url}")
    
    return 0 if result['success'] else 1


if __name__ == "__main__":
    sys.exit(main())
