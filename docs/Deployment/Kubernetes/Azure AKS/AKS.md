# Deploy Kubernetes on Azure Kubernetes Service (AKS)

## Prerequisites
- Azure account
- `az` CLI installed and logged in
- `kubectl` installed

## 1. Login and Set Subscription
```bash
az login
az account set --subscription "YOUR_SUBSCRIPTION_NAME"
```

## 2. Create Resource Group
```bash
az group create --name myResourceGroup --location eastus
```

## 3. Create AKS Cluster
```bash
az aks create \
  --resource-group myResourceGroup \
  --name my-aks-cluster \
  --node-count 3 \
  --enable-addons monitoring \
  --generate-ssh-keys
```

## 4. Get Credentials for kubectl
```bash
az aks get-credentials --resource-group myResourceGroup --name my-aks-cluster
```

## 5. Test the Cluster
```bash
kubectl get nodes
```

## Delete the Cluster
```bash
az group delete --name myResourceGroup --yes --no-wait
```