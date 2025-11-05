# Test Kubernetes



## Prerequisites

- Minikube
- kubectl

## Steps

1. Install kubectl (Requirement)
```bash
# Linux
curl -LO "https://dl.k8s.io/release/$(curl -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"
chmod +x kubectl && sudo mv kubectl /usr/local/bin/

# Mac (Homebrew)
brew install kubectl

# Windows (PowerShell)
choco install kubernetes-cli
```

2. Install Minikube (Requirement)
```bash
# Linux
sudo apt-get install minikube

# Mac
brew install minikube

# Windows
choco install minikube
```

3. Start Minikube (Development)
```bash
minikube start
```

4. Start a Cluster (Development)
```bash
kubectl get nodes
```

5. Test Kubernetes (Production)
```bash
kubectl get nodes
kubectl create deployment nginx --image=nginx
kubectl expose deployment nginx --port=80 --type=NodePort
minikube service nginx  # If using minikube
```

6. Check if config exists
```powershell
Test-Path $HOME\.kube\config
```

If False, generate config:

Based on your provider:

GKE:
```bash
gcloud container clusters get-credentials my-cluster --zone us-central1-a
```

EKS:
```bash
eksctl utils write-kubeconfig --cluster my-cluster
```

AKS:
```bash
az aks get-credentials --resource-group myResourceGroup --name my-aks-cluster
```

Once that’s done, test:
```powershell
kubectl config view
kubectl get nodes
```