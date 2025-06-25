output "api_gateway_url" {
  description = "The URL of the AWS API Gateway"
  value       = aws_apigatewayv2_stage.prod.invoke_url
}

output "vpc_id" {
  description = "The ID of the VPC"
  value       = module.vpc.vpc_id
}

output "vpc_arn" {
  description = "The ARN of the VPC"
  value       = module.vpc.vpc_arn
}

output "private_subnets" {
  description = "List of private subnet IDs"
  value       = module.vpc.private_subnets
}

output "public_subnets" {
  description = "List of public subnet IDs"
  value       = module.vpc.public_subnets
}

output "api_gateway_id" {
  description = "The ID of the API Gateway"
  value       = aws_apigatewayv2_api.main.id
}

output "api_gateway_endpoint" {
  description = "The URL of the API Gateway"
  value       = aws_apigatewayv2_stage.prod.invoke_url
}

output "app_runner_role_arn" {
  description = "The ARN of the IAM role for App Runner"
  value       = aws_iam_role.app_runner_role.arn
}

output "app_runner_sg_id" {
  description = "The ID of the App Runner security group"
  value       = aws_security_group.app_runner_sg.id
}

output "api_gateway_sg_id" {
  description = "The ID of the API Gateway security group"
  value       = aws_security_group.api_gw_sg.id
}

output "app_runner_vpc_connector_arn" {
  description = "The ARN of the App Runner VPC connector"
  value       = aws_apprunner_vpc_connector.main.arn
}

output "cloudwatch_log_group_arn" {
  description = "The ARN of the CloudWatch log group for API Gateway"
  value       = aws_cloudwatch_log_group.api_gw.arn
}
