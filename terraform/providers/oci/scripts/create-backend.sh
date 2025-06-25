#!/bin/bash
# Create Object Storage backend for Terraform state

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

echo -e "${YELLOW}Creating OCI Object Storage backend for Terraform state...${NC}"

# Check required environment variables
if [ -z "$OCI_COMPARTMENT_ID" ]; then
    echo -e "${RED}Error: OCI_COMPARTMENT_ID is not set${NC}"
    exit 1
fi

if [ -z "$OCI_REGION" ]; then
    echo -e "${RED}Error: OCI_REGION is not set${NC}"
    exit 1
fi

# Create bucket for Terraform state
BUCKET_NAME="terraform-state-$(date +%s)"
NAMESPACE=$(oci os ns get --query 'data' --raw-output)

echo -e "${YELLOW}Creating bucket: $BUCKET_NAME${NC}"
oci os bucket create \
    --compartment-id "$OCI_COMPARTMENT_ID" \
    --name "$BUCKET_NAME" \
    --public-access-type NoPublicAccess \
    --storage-tier Standard \
    --versioning Enabled

# Create pre-authenticated request for backend access
echo -e "${YELLOW}Creating pre-authenticated request...${NC}"
PAR_EXPIRY=$(date -u -d "+1 year" +"%Y-%m-%dT%H:%M:%S.000Z")
PAR_RESPONSE=$(oci os preauth-request create \
    --bucket-name "$BUCKET_NAME" \
    --name "terraform-backend-par" \
    --access-type AnyObjectReadWrite \
    --time-expires "$PAR_EXPIRY" \
    --bucket-listing-action ListObjects)

PAR_URL=$(echo "$PAR_RESPONSE" | jq -r '.data."access-uri"')
FULL_PAR_URL="https://objectstorage.$OCI_REGION.oraclecloud.com$PAR_URL"

# Create backend configuration file
cat > backend-config.tf << EOF
terraform {
  backend "http" {
    address = "$FULL_PAR_URL"
    update_method = "PUT"
  }
}
EOF

# Save configuration
cat > backend-config.json << EOF
{
  "bucket_name": "$BUCKET_NAME",
  "namespace": "$NAMESPACE",
  "par_url": "$FULL_PAR_URL",
  "par_expiry": "$PAR_EXPIRY",
  "region": "$OCI_REGION"
}
EOF

echo -e "${GREEN}Backend created successfully!${NC}"
echo -e "${GREEN}Bucket: $BUCKET_NAME${NC}"
echo -e "${GREEN}Namespace: $NAMESPACE${NC}"
echo -e "${YELLOW}Backend configuration saved to: backend-config.tf${NC}"
echo -e "${YELLOW}Configuration details saved to: backend-config.json${NC}"
echo ""
echo -e "${YELLOW}To use this backend, copy backend-config.tf to your environment directory${NC}"
