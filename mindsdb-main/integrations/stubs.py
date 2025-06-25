"""
Stub module providing placeholder classes for MindsDB integrations when the actual
cloud provider SDK dependencies aren't available.

This ensures that the application can still run in "plug-and-play" mode without requiring
users to install all cloud provider SDKs immediately.
"""

import logging
from typing import Dict, Any, List, Optional, Union

logger = logging.getLogger(__name__)

# Base cloud connector stubs

class CloudConnector:
    """Stub for the CloudConnector class"""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """Initialize the CloudConnector stub."""
        self.config = config or {}
        self.providers_enabled = {}
        logger.debug("CloudConnector stub initialized")
    
    def test_connections(self) -> Dict[str, bool]:
        """Test connections to all configured providers."""
        return {"aws": False, "gcp": False, "azure": False, "oci": False}
    
    def get_aws_client(self, service_name: str):
        """Get a stub AWS client."""
        logger.warning(f"AWS SDK not available. Stub for {service_name} returned.")
        return None
    
    def get_gcp_client(self, service_name: str):
        """Get a stub GCP client."""
        logger.warning(f"GCP SDK not available. Stub for {service_name} returned.")
        return None
    
    def get_azure_client(self, service_name: str):
        """Get a stub Azure client."""
        logger.warning(f"Azure SDK not available. Stub for {service_name} returned.")
        return None
    
    def get_oci_client(self, service_name: str):
        """Get a stub OCI client."""
        logger.warning(f"OCI SDK not available. Stub for {service_name} returned.")
        return None
    
    def create_mindsdb_datasource(self, provider: str, datasource_name: str, 
                                 mindsdb_api: Any = None) -> Dict[str, Any]:
        """Create a stub datasource in MindsDB."""
        logger.warning(f"Cannot create {provider} datasource - SDK not available")
        return {
            "success": False, 
            "name": datasource_name,
            "error": f"{provider} SDK not available"
        }

# Export stubs to be imported elsewhere
__all__ = ['CloudConnector']
