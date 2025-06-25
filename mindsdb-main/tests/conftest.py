"""Configuration file for pytest."""
import os
import sys
from pathlib import Path
from unittest.mock import patch, MagicMock

# Add the project root to the Python path
project_root = str(Path(__file__).parent.parent)
if project_root not in sys.path:
    sys.path.insert(0, project_root)

import pytest  # noqa: E402
from dotenv import load_dotenv  # noqa: E402

# Load environment variables from .env file
load_dotenv()

# Set up test environment variables
os.environ["TESTING"] = "true"

# Common test fixtures
@pytest.fixture(scope="session", autouse=True)
def setup_test_environment():
    """Set up test environment before tests run."""
    # Any setup code that needs to run before all tests
    yield
    # Any teardown code that needs to run after all tests

@pytest.fixture
def mock_requests():
    """Fixture to mock requests for testing HTTP calls."""
    with patch('requests.Session') as mock_session:
        # Configure the mock session
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_session.return_value.get.return_value = mock_response
        mock_session.return_value.post.return_value = mock_response
        yield mock_session

@pytest.fixture
def mock_cloud_connector():
    """Fixture to mock CloudConnector for testing."""
    with patch('cloud_connector.CloudConnector') as mock_connector:
        # Configure the mock connector
        mock_instance = MagicMock()
        mock_connector.return_value = mock_instance
        yield mock_instance

# Add custom markers
def pytest_configure(config):
    """Configure pytest with custom markers."""
    config.addinivalue_line(
        "markers",
        "integration: mark test as integration test (dependencies required)",
    )
    config.addinivalue_line(
        "markers",
        "slow: mark test as slow-running",
    )
