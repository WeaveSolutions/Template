import os
import sys
from unittest.mock import patch, MagicMock
import pytest
from dotenv import load_dotenv

# Add parent directory to path to allow importing from the package
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Load environment variables from .env file
load_dotenv()

# Mock environment variables for testing
os.environ["MINDSDB_URL"] = "http://localhost:47334"
os.environ["MINDSDB_USER"] = "testuser"
os.environ["MINDSDB_PASSWORD"] = "testpassword"

# Import after setting up environment variables
from mindsdb_main.init_mindsdb import MindsDBInitializer  # noqa: E402

@pytest.fixture(name="mindsdb_initializer")
def fixture_mindsdb_initializer():
    """Fixture to create a MindsDBInitializer instance for testing."""
    return MindsDBInitializer()

def test_mindsdb_initializer_creation(mindsdb_initializer):
    """Test that MindsDBInitializer is created with correct attributes."""
    assert mindsdb_initializer is not None
    assert hasattr(mindsdb_initializer, 'mindsdb_url')
    assert hasattr(mindsdb_initializer, 'mindsdb_user')
    assert hasattr(mindsdb_initializer, 'mindsdb_password')
    assert hasattr(mindsdb_initializer, 'session')

@patch('requests.Session')
def test_wait_for_mindsdb(mock_session, mindsdb_initializer):
    """Test waiting for MindsDB to be ready."""
    # Mock successful response
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_session.return_value.get.return_value = mock_response
    
    assert mindsdb_initializer.wait_for_mindsdb() is True
    mock_session.return_value.get.assert_called_once_with(
        f"{mindsdb_initializer.mindsdb_url}/api/sql/query",
        params={"query": "SELECT 1"},
        timeout=10
    )

@patch('requests.Session')
def test_create_auth0_integration(mock_session, mindsdb_initializer):
    """Test Auth0 integration creation."""
    # Mock successful response
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_session.return_value.post.return_value = mock_response
    
    # Set up test data
    test_domain = "test.auth0.com"
    test_client_id = "test_client_id"
    test_client_secret = "test_client_secret"
    
    result = mindsdb_initializer.create_auth0_integration(
        domain=test_domain,
        client_id=test_client_id,
        client_secret=test_client_secret
    )
    
    assert result is True
    mock_session.return_value.post.assert_called_once()
    
    # Verify the SQL query in the request
    call_args = mock_session.return_value.post.call_args[1]
    assert "CREATE DATABASE auth0_datasource" in call_args["params"]["query"]
    assert test_domain in call_args["params"]["query"]
    assert test_client_id in call_args["params"]["query"]

@patch('requests.Session')
def test_setup_cloud_datasources(mock_session, mindsdb_initializer):
    """Test setting up cloud datasources."""
    # Mock successful responses
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_session.return_value.post.return_value = mock_response
    
    # Mock cloud connector
    mock_connector = MagicMock()
    mock_connector.list_providers.return_value = ["aws"]
    
    with patch('cloud_connector.CloudConnector') as mock_connector_class:
        mock_connector_class.return_value = mock_connector
        
        # Call the method
        result = mindsdb_initializer.setup_cloud_datasources()
        
        # Verify results
        assert isinstance(result, dict)
        assert "aws" in result
        assert result["aws"] is True
        
        # Verify the connector was used
        mock_connector_class.assert_called_once()
        mock_connector.list_providers.assert_called_once()
        
        # Verify the SQL query was made to create the datasource
        mock_session.return_value.post.assert_called()
        call_args = mock_session.return_value.post.call_args[1]
        assert "CREATE DATABASE aws_datasource" in call_args["params"]["query"]
