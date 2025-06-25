# PowerShell script to set up Terraform backend infrastructure

# Configuration
$BucketName = if ($env:TF_STATE_BUCKET) { $env:TF_STATE_BUCKET } else { "Nexpo-terraform-state" }
$TableName = if ($env:TF_STATE_LOCK_TABLE) { $env:TF_STATE_LOCK_TABLE } else { "terraform-state-lock" }
$Region = if ($env:AWS_REGION) { $env:AWS_REGION } else { "us-east-1" }

Write-Host "Setting up Terraform backend infrastructure..." -ForegroundColor Yellow

# Check AWS CLI is installed
try {
    $awsVersion = aws --version 2>&1
    Write-Host "AWS CLI found: $awsVersion" -ForegroundColor Green
} catch {
    Write-Host "AWS CLI is not installed. Please install it first." -ForegroundColor Red
    Write-Host "Download from: https://aws.amazon.com/cli/" -ForegroundColor Yellow
    exit 1
}

# Check AWS credentials
try {
    $identity = aws sts get-caller-identity --output json | ConvertFrom-Json
    Write-Host "Using AWS Account: $($identity.Account)" -ForegroundColor Green
} catch {
    Write-Host "AWS credentials not configured. Please run 'aws configure'." -ForegroundColor Red
    exit 1
}

# Create S3 bucket
Write-Host "Creating S3 bucket: $BucketName..." -ForegroundColor Yellow
try {
    aws s3api head-bucket --bucket $BucketName 2>$null
    Write-Host "Bucket already exists." -ForegroundColor Yellow
} catch {
    try {
        if ($Region -eq "us-east-1") {
            aws s3api create-bucket --bucket $BucketName --region $Region
        } else {
            $config = @{ LocationConstraint = $Region } | ConvertTo-Json -Compress
            aws s3api create-bucket --bucket $BucketName --region $Region --create-bucket-configuration $config
        }
        Write-Host "Bucket created successfully." -ForegroundColor Green
    } catch {
        Write-Host "Failed to create bucket: $_" -ForegroundColor Red
        exit 1
    }
}

# Enable versioning
Write-Host "Enabling versioning..." -ForegroundColor Yellow
aws s3api put-bucket-versioning `
    --bucket $BucketName `
    --versioning-configuration Status=Enabled

# Enable encryption
Write-Host "Enabling encryption..." -ForegroundColor Yellow
$encryptionConfig = @'
{
    "Rules": [{
        "ApplyServerSideEncryptionByDefault": {
            "SSEAlgorithm": "AES256"
        }
    }]
}
'@
aws s3api put-bucket-encryption `
    --bucket $BucketName `
    --server-side-encryption-configuration $encryptionConfig

# Block public access
Write-Host "Blocking public access..." -ForegroundColor Yellow
aws s3api put-public-access-block `
    --bucket $BucketName `
    --public-access-block-configuration `
    "BlockPublicAcls=true,IgnorePublicAcls=true,BlockPublicPolicy=true,RestrictPublicBuckets=true"

# Create bucket policy
Write-Host "Creating bucket policy..." -ForegroundColor Yellow
$bucketPolicy = @"
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Sid": "EnforcedTLS",
            "Effect": "Deny",
            "Principal": "*",
            "Action": "s3:*",
            "Resource": [
                "arn:aws:s3:::$BucketName/*",
                "arn:aws:s3:::$BucketName"
            ],
            "Condition": {
                "Bool": {
                    "aws:SecureTransport": "false"
                }
            }
        }
    ]
}
"@

$bucketPolicy | Out-File -FilePath "$env:TEMP\bucket-policy.json" -Encoding UTF8
aws s3api put-bucket-policy `
    --bucket $BucketName `
    --policy file://$env:TEMP/bucket-policy.json
Remove-Item "$env:TEMP\bucket-policy.json"

# Create DynamoDB table
Write-Host "Creating DynamoDB table: $TableName..." -ForegroundColor Yellow
try {
    aws dynamodb describe-table --table-name $TableName --region $Region 2>$null | Out-Null
    Write-Host "Table already exists." -ForegroundColor Yellow
} catch {
    try {
        aws dynamodb create-table `
            --table-name $TableName `
            --attribute-definitions AttributeName=LockID,AttributeType=S `
            --key-schema AttributeName=LockID,KeyType=HASH `
            --billing-mode PAY_PER_REQUEST `
            --region $Region `
            --tags Key=Project,Value=Nexpo Key=ManagedBy,Value=Terraform
        
        Write-Host "Table created successfully." -ForegroundColor Green
        
        # Wait for table to be active
        Write-Host "Waiting for table to be active..." -ForegroundColor Yellow
        aws dynamodb wait table-exists --table-name $TableName --region $Region
        Write-Host "Table is active." -ForegroundColor Green
    } catch {
        Write-Host "Failed to create table: $_" -ForegroundColor Red
        exit 1
    }
}

# Create backend configuration files
Write-Host "Creating backend configuration..." -ForegroundColor Yellow

# Main backend configuration
$backendConfig = @"
terraform {
  backend "s3" {
    bucket         = "$BucketName"
    key            = "Nexpo/terraform.tfstate"
    region         = "$Region"
    dynamodb_table = "$TableName"
    encrypt        = true
  }
}
"@
$backendConfig | Out-File -FilePath "terraform\backend.tf" -Encoding UTF8

# Environment-specific backend configurations
foreach ($env in @("dev", "prod")) {
    $envBackendConfig = @"
terraform {
  backend "s3" {
    bucket         = "$BucketName"
    key            = "Nexpo/$env/terraform.tfstate"
    region         = "$Region"
    dynamodb_table = "$TableName"
    encrypt        = true
  }
}
"@
    $envBackendConfig | Out-File -FilePath "terraform\environments\$env\backend.tf" -Encoding UTF8
}

Write-Host "`nBackend setup complete!" -ForegroundColor Green
Write-Host "`nBackend configuration:" -ForegroundColor Yellow
Write-Host "  S3 Bucket: $BucketName"
Write-Host "  DynamoDB Table: $TableName"
Write-Host "  Region: $Region"
Write-Host "`nNext steps:" -ForegroundColor Yellow
Write-Host "1. Update GitHub secrets with these values:"
Write-Host "   TF_STATE_BUCKET=$BucketName" -ForegroundColor Cyan
Write-Host "   TF_STATE_LOCK_TABLE=$TableName" -ForegroundColor Cyan
Write-Host "2. Run 'terraform init' in your environment directories"
