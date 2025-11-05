# Deploy Kubernetes on Amazon EKS (Elastic Kubernetes Service)

## Prerequisites
- AWS account with admin IAM user
- `aws` CLI installed and configured
- `eksctl` installed
- `kubectl` installed

## 1. Configure AWS CLI
```bash
aws configure
```

## 2. Create EKS Cluster using eksctl
```bash
eksctl create cluster \
  --name my-eks-cluster \
  --region us-west-2 \
  --nodes 3
```

## 3. Test the Cluster
```bash
kubectl get nodes
```

## Delete the Cluster
```bash
eksctl delete cluster --name my-eks-cluster --region us-west-2
```
