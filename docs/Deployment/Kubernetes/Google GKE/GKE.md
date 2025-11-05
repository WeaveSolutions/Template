# Deploy Kubernetes on Google Kubernetes Engine (GKE)

## Prerequisites
- Google Cloud account
- Billing enabled
- `gcloud` CLI installed and authenticated
- `kubectl` installed

## 1. Authenticate with Google Cloud
```bash
gcloud auth login
gcloud config set project YOUR_PROJECT_ID
```

## 2. Enable required services
```bash
gcloud services enable container.googleapis.com
```

## Create GKE Cluster
```bash
gcloud container clusters create my-gke-cluster \
  --zone us-central1-a \
  --num-nodes 3
```

## 4. Configure kubectl
```bash
gcloud container clusters get-credentials my-gke-cluster --zone us-central1-a
```
## 5. Test the Cluster
```bash
kubectl get nodes
```

## Delete the Cluster
```bash
gcloud container clusters delete my-gke-cluster --zone us-central1-a
```