#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
BUCKET_NAME="${TF_STATE_BUCKET:-Nexpo-terraform-state}"
TABLE_NAME="${TF_STATE_LOCK_TABLE:-terraform-state-lock}"
REGION="${AWS_REGION:-us-east-1}"

echo -e "${YELLOW}Setting up Terraform backend infrastructure...${NC}"

# Check AWS CLI is installed
if ! command -v aws &> /dev/null; then
    echo -e "${RED}AWS CLI is not installed. Please install it first.${NC}"
    exit 1
fi

# Check AWS credentials
if ! aws sts get-caller-identity &> /dev/null; then
    echo -e "${RED}AWS credentials not configured. Please run 'aws configure'.${NC}"
    exit 1
fi

# Get AWS account ID
ACCOUNT_ID=$(aws sts get-caller-identity --query Account --output text)
echo -e "${GREEN}Using AWS Account: ${ACCOUNT_ID}${NC}"

# Create S3 bucket
echo -e "${YELLOW}Creating S3 bucket: ${BUCKET_NAME}...${NC}"
if aws s3api head-bucket --bucket "${BUCKET_NAME}" 2>/dev/null; then
    echo -e "${YELLOW}Bucket already exists.${NC}"
else
    if aws s3api create-bucket \
        --bucket "${BUCKET_NAME}" \
        --region "${REGION}" \
        $(if [ "${REGION}" != "us-east-1" ]; then echo "--create-bucket-configuration LocationConstraint=${REGION}"; fi) \
        2>/dev/null; then
        echo -e "${GREEN}Bucket created successfully.${NC}"
    else
        echo -e "${RED}Failed to create bucket.${NC}"
        exit 1
    fi
fi

# Enable versioning
echo -e "${YELLOW}Enabling versioning...${NC}"
aws s3api put-bucket-versioning \
    --bucket "${BUCKET_NAME}" \
    --versioning-configuration Status=Enabled

# Enable encryption
echo -e "${YELLOW}Enabling encryption...${NC}"
aws s3api put-bucket-encryption \
    --bucket "${BUCKET_NAME}" \
    --server-side-encryption-configuration '{
        "Rules": [{
            "ApplyServerSideEncryptionByDefault": {
                "SSEAlgorithm": "AES256"
            }
        }]
    }'

# Block public access
echo -e "${YELLOW}Blocking public access...${NC}"
aws s3api put-public-access-block \
    --bucket "${BUCKET_NAME}" \
    --public-access-block-configuration \
    "BlockPublicAcls=true,IgnorePublicAcls=true,BlockPublicPolicy=true,RestrictPublicBuckets=true"

# Create bucket policy
echo -e "${YELLOW}Creating bucket policy...${NC}"
cat > /tmp/bucket-policy.json <<EOF
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Sid": "EnforcedTLS",
            "Effect": "Deny",
            "Principal": "*",
            "Action": "s3:*",
            "Resource": [
                "arn:aws:s3:::${BUCKET_NAME}/*",
                "arn:aws:s3:::${BUCKET_NAME}"
            ],
            "Condition": {
                "Bool": {
                    "aws:SecureTransport": "false"
                }
            }
        }
    ]
}
EOF

aws s3api put-bucket-policy \
    --bucket "${BUCKET_NAME}" \
    --policy file:///tmp/bucket-policy.json

rm /tmp/bucket-policy.json

# Create DynamoDB table
echo -e "${YELLOW}Creating DynamoDB table: ${TABLE_NAME}...${NC}"
if aws dynamodb describe-table --table-name "${TABLE_NAME}" --region "${REGION}" 2>/dev/null; then
    echo -e "${YELLOW}Table already exists.${NC}"
else
    if aws dynamodb create-table \
        --table-name "${TABLE_NAME}" \
        --attribute-definitions AttributeName=LockID,AttributeType=S \
        --key-schema AttributeName=LockID,KeyType=HASH \
        --billing-mode PAY_PER_REQUEST \
        --region "${REGION}" \
        --tags Key=Project,Value=Nexpo Key=ManagedBy,Value=Terraform \
        2>/dev/null; then
        echo -e "${GREEN}Table created successfully.${NC}"
        
        # Wait for table to be active
        echo -e "${YELLOW}Waiting for table to be active...${NC}"
        aws dynamodb wait table-exists --table-name "${TABLE_NAME}" --region "${REGION}"
        echo -e "${GREEN}Table is active.${NC}"
    else
        echo -e "${RED}Failed to create table.${NC}"
        exit 1
    fi
fi

# Create backend configuration file
echo -e "${YELLOW}Creating backend configuration...${NC}"
cat > terraform/backend.tf <<EOF
terraform {
  backend "s3" {
    bucket         = "${BUCKET_NAME}"
    key            = "Nexpo/terraform.tfstate"
    region         = "${REGION}"
    dynamodb_table = "${TABLE_NAME}"
    encrypt        = true
  }
}
EOF

# Create example backend config for environments
for env in dev prod; do
    cat > "terraform/environments/${env}/backend.tf" <<EOF
terraform {
  backend "s3" {
    bucket         = "${BUCKET_NAME}"
    key            = "Nexpo/${env}/terraform.tfstate"
    region         = "${REGION}"
    dynamodb_table = "${TABLE_NAME}"
    encrypt        = true
  }
}
EOF
done

echo -e "${GREEN}Backend setup complete!${NC}"
echo -e "${YELLOW}Backend configuration:${NC}"
echo -e "  S3 Bucket: ${BUCKET_NAME}"
echo -e "  DynamoDB Table: ${TABLE_NAME}"
echo -e "  Region: ${REGION}"
echo -e ""
echo -e "${YELLOW}Next steps:${NC}"
echo -e "1. Update GitHub secrets with these values:"
echo -e "   TF_STATE_BUCKET=${BUCKET_NAME}"
echo -e "   TF_STATE_LOCK_TABLE=${TABLE_NAME}"
echo -e "2. Run 'terraform init' in your environment directories"
