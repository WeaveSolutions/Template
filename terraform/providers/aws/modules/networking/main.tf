# Networking Module - Following Well-Architected Framework
# Provides VPC, subnets, security groups, and network ACLs

locals {
  azs = var.availability_zones
  
  # Calculate subnet CIDR blocks
  public_subnet_cidrs  = [for i in range(length(local.azs)) : cidrsubnet(var.vpc_cidr, 8, i)]
  private_subnet_cidrs = [for i in range(length(local.azs)) : cidrsubnet(var.vpc_cidr, 8, i + 10)]
  database_subnet_cidrs = [for i in range(length(local.azs)) : cidrsubnet(var.vpc_cidr, 8, i + 20)]
}

# VPC
resource "aws_vpc" "main" {
  cidr_block           = var.vpc_cidr
  enable_dns_hostnames = true
  enable_dns_support   = true

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-vpc"
    }
  )
}

# Internet Gateway
resource "aws_internet_gateway" "main" {
  vpc_id = aws_vpc.main.id

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-igw"
    }
  )
}

# Public Subnets
resource "aws_subnet" "public" {
  count                   = length(local.azs)
  vpc_id                  = aws_vpc.main.id
  cidr_block              = local.public_subnet_cidrs[count.index]
  availability_zone       = local.azs[count.index]
  map_public_ip_on_launch = true

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-public-${local.azs[count.index]}"
      Type = "public"
    }
  )
}

# Private Subnets
resource "aws_subnet" "private" {
  count             = length(local.azs)
  vpc_id            = aws_vpc.main.id
  cidr_block        = local.private_subnet_cidrs[count.index]
  availability_zone = local.azs[count.index]

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-private-${local.azs[count.index]}"
      Type = "private"
    }
  )
}

# Database Subnets
resource "aws_subnet" "database" {
  count             = length(local.azs)
  vpc_id            = aws_vpc.main.id
  cidr_block        = local.database_subnet_cidrs[count.index]
  availability_zone = local.azs[count.index]

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-database-${local.azs[count.index]}"
      Type = "database"
    }
  )
}

# Elastic IPs for NAT Gateways
resource "aws_eip" "nat" {
  count  = var.enable_nat_gateway ? length(local.azs) : 0
  domain = "vpc"

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-nat-eip-${local.azs[count.index]}"
    }
  )
}

# NAT Gateways
resource "aws_nat_gateway" "main" {
  count         = var.enable_nat_gateway ? length(local.azs) : 0
  subnet_id     = aws_subnet.public[count.index].id
  allocation_id = aws_eip.nat[count.index].id

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-nat-${local.azs[count.index]}"
    }
  )

  depends_on = [aws_internet_gateway.main]
}

# Route Tables
resource "aws_route_table" "public" {
  vpc_id = aws_vpc.main.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.main.id
  }

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-public-rt"
    }
  )
}

resource "aws_route_table" "private" {
  count  = length(local.azs)
  vpc_id = aws_vpc.main.id

  dynamic "route" {
    for_each = var.enable_nat_gateway ? [1] : []
    content {
      cidr_block     = "0.0.0.0/0"
      nat_gateway_id = aws_nat_gateway.main[count.index].id
    }
  }

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-private-rt-${local.azs[count.index]}"
    }
  )
}

# Route Table Associations
resource "aws_route_table_association" "public" {
  count          = length(local.azs)
  subnet_id      = aws_subnet.public[count.index].id
  route_table_id = aws_route_table.public.id
}

resource "aws_route_table_association" "private" {
  count          = length(local.azs)
  subnet_id      = aws_subnet.private[count.index].id
  route_table_id = aws_route_table.private[count.index].id
}

resource "aws_route_table_association" "database" {
  count          = length(local.azs)
  subnet_id      = aws_subnet.database[count.index].id
  route_table_id = aws_route_table.private[count.index].id
}

# Security Groups
resource "aws_security_group" "alb" {
  name_prefix = "${var.project_name}-${var.environment}-alb-"
  description = "Security group for Application Load Balancer"
  vpc_id      = aws_vpc.main.id

  ingress {
    description = "HTTP from anywhere"
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    description = "HTTPS from anywhere"
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    description = "Allow all outbound"
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-alb-sg"
    }
  )

  lifecycle {
    create_before_destroy = true
  }
}

resource "aws_security_group" "app" {
  name_prefix = "${var.project_name}-${var.environment}-app-"
  description = "Security group for application instances"
  vpc_id      = aws_vpc.main.id

  ingress {
    description     = "HTTP from ALB"
    from_port       = var.app_port
    to_port         = var.app_port
    protocol        = "tcp"
    security_groups = [aws_security_group.alb.id]
  }

  egress {
    description = "Allow all outbound"
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-app-sg"
    }
  )

  lifecycle {
    create_before_destroy = true
  }
}

resource "aws_security_group" "database" {
  name_prefix = "${var.project_name}-${var.environment}-db-"
  description = "Security group for database"
  vpc_id      = aws_vpc.main.id

  ingress {
    description     = "PostgreSQL from app"
    from_port       = 5432
    to_port         = 5432
    protocol        = "tcp"
    security_groups = [aws_security_group.app.id]
  }

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-db-sg"
    }
  )

  lifecycle {
    create_before_destroy = true
  }
}

# VPC Flow Logs (Operational Excellence)
resource "aws_flow_log" "main" {
  count                = var.enable_flow_logs ? 1 : 0
  iam_role_arn         = aws_iam_role.flow_log[0].arn
  log_destination      = aws_cloudwatch_log_group.flow_log[0].arn
  traffic_type         = "ALL"
  vpc_id               = aws_vpc.main.id
  max_aggregation_interval = 60

  tags = merge(
    var.common_tags,
    {
      Name = "${var.project_name}-${var.environment}-flow-logs"
    }
  )
}

resource "aws_cloudwatch_log_group" "flow_log" {
  count             = var.enable_flow_logs ? 1 : 0
  name              = "/aws/vpc/${var.project_name}-${var.environment}"
  retention_in_days = var.flow_log_retention_days
  kms_key_id        = var.kms_key_id

  tags = var.common_tags
}

resource "aws_iam_role" "flow_log" {
  count = var.enable_flow_logs ? 1 : 0
  name  = "${var.project_name}-${var.environment}-flow-log-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "vpc-flow-logs.amazonaws.com"
        }
      }
    ]
  })

  tags = var.common_tags
}

resource "aws_iam_role_policy" "flow_log" {
  count = var.enable_flow_logs ? 1 : 0
  name  = "${var.project_name}-${var.environment}-flow-log-policy"
  role  = aws_iam_role.flow_log[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = [
          "logs:CreateLogGroup",
          "logs:CreateLogStream",
          "logs:PutLogEvents",
          "logs:DescribeLogGroups",
          "logs:DescribeLogStreams"
        ]
        Effect   = "Allow"
        Resource = "*"
      }
    ]
  })
}

# Network ACLs for additional security layer
resource "aws_network_acl_rule" "public_ingress" {
  count          = var.enable_network_acls ? 1 : 0
  network_acl_id = aws_vpc.main.default_network_acl_id
  rule_number    = 100
  rule_action    = "allow"
  protocol       = -1
  cidr_block     = "0.0.0.0/0"
  from_port      = 0
  to_port        = 0
}

resource "aws_network_acl_rule" "public_egress" {
  count          = var.enable_network_acls ? 1 : 0
  network_acl_id = aws_vpc.main.default_network_acl_id
  rule_number    = 100
  rule_action    = "allow"
  egress         = true
  protocol       = -1
  cidr_block     = "0.0.0.0/0"
  from_port      = 0
  to_port        = 0
}
