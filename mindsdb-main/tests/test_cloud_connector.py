import os
import pytest
from unittest.mock import patch, MagicMock
from dotenv import load_dotenv

# Load environment variables from .env file
load_dotenv()

# Mock the required environment variables for testing
os.environ["AWS_ACCESS_KEY_ID"] = "test_aws_key"
os.environ["AWS_SECRET_ACCESS_KEY"] = "test_aws_secret"
os.environ["AWS_REGION"] = "us-west-2"

# Import after setting up environment variables
from cloud_connector import CloudConnector

@pytest.fixture
def cloud_connector():
    """Fixture to create a CloudConnector instance for testing."""
    return CloudConnector()

def test_cloud_connector_initialization(cloud_connector):
    """Test that CloudConnector initializes correctly."""
    assert cloud_connector is not None
    assert hasattr(cloud_connector, 'providers')
    assert isinstance(cloud_connector.providers, dict)

@patch('boto3.client')
def test_aws_initialization(mock_boto3, cloud_connector):
    """Test AWS provider initialization."""
    # Mock boto3 client
    mock_s3 = MagicMock()
    mock_boto3.return_value = mock_s3
    
    # Initialize AWS provider
    cloud_connector._init_aws()
    
    # Check if boto3.client was called with correct arguments
    mock_boto3.assert_called_once_with(
        's3',
        aws_access_key_id=os.getenv("AWS_ACCESS_KEY_ID"),
        aws_secret_access_key=os.getenv("AWS_SECRET_ACCESS_KEY"),
        region_name=os.getenv("AWS_REGION")
    )
    
    # Check if provider was added to providers dict
    assert 'aws' in cloud_connector.providers
    assert cloud_connector.providers['aws'] is not None

def test_get_provider(cloud_connector):
    """Test getting a provider by name."""
    # Test with non-existent provider
    assert cloud_connector.get_provider('nonexistent') is None
    
    # Test with existing provider (after initialization)
    with patch('boto3.client'):
        cloud_connector._init_aws()
        assert cloud_connector.get_provider('aws') is not None

def test_list_providers(cloud_connector):
    """Test listing available providers."""
    # Initially no providers should be initialized
    assert cloud_connector.list_providers() == []
    
    # After initializing AWS, it should be in the list
    with patch('boto3.client'):
        cloud_connector._init_aws()
        providers = cloud_connector.list_providers()
        assert 'aws' in providers
        assert len(providers) == 1
