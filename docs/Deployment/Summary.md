[Nomad vs Kubernetes](https://developer.hashicorp.com/nomad/docs/what-is-nomad#nomad-versus-kubernetes)
[Nomad vs Kubernetes - Complexity](https://www.hashicorp.com/en/resources/hashicorp-nomad-vs-kubernetes-comparing-complexity)
[HashiCorp Conference EU 2016](https://www.youtube.com/playlist?list=PL81sUbsFNc5b-Gd59Lpz7BW0eHJBt0GvE)

# Nomad versus Kubernetes
Kubernetes and Nomad support similar core use cases for application deployment and management, but they differ in a few key ways. Kubernetes aims to provide all the features needed to run Linux container-based applications including cluster management, scheduling, service discovery, monitoring, and secrets management. Nomad only aims to focus on cluster management and scheduling, and Nomad is designed with the Unix philosophy of having a small scope while composing with tools like Consul for service discovery/service mesh and Vault for secret management.

# Simplicity
Kubernetes is designed as a collection of more than a half-dozen interoperating services which together provide the full functionality. Coordination and storage is provided by etcd at the core. The state is wrapped by API controllers which are consumed by other services that provide higher level APIs for features like scheduling. Kubernetes supports running in a highly available configuration but is operationally complex to setup.

Nomad is architecturally much simpler. Nomad is a single binary, both for clients and servers, and requires no external services for coordination or storage. Nomad combines a lightweight resource manager and a sophisticated scheduler into a single system. By default, Nomad is distributed, highly available, and operationally simple.

# Flexible Workload Support
While Kubernetes is specifically focused on Linux containers, Nomad is more general purpose. Nomad supports virtualized, containerized and standalone applications, including Docker, Java, IIS on Windows, Qemu, etc. Nomad is designed with extensible drivers and support will be extended to all common drivers.

# Consistent Deployment
A full Kubernetes installation for a production environment is time consuming, operationally complex, and resource intensive. An increasing number of implementations are created by the Kubernetes community to mitigate these challenges, such as minikube, kubeadm, k3s, and more. These trimmed versions of Kubernetes offer easier adoption for development and testing, but lead to inconsistency in capabilities, configuration, and management when moving into production.

In contrast to Kubernetes' fragmented distributions, Nomad as a single lightweight binary can be deployed in local dev, production, on-prem, at the edge, and in the cloud in a consistent manner, and provides the same operational ease-of-use across all environments.

# Scalability
Kubernetes documentation states that they support clusters up to 5,000 nodes and 300,000 total containers. As the environment grows, the interoperating components with different constraints compound the operational complexity. Even operators at Google revealed the significant challenges of managing the system at scale. The lack of maturity in the Federation project and the additional overhead of managing a centralized management plane also make it a hard experience to deploy a distributed system that spans multiple clusters.

Nomad has been proven to scale to cluster sizes that exceed 10,000 nodes in real-world production environments. It can be deployed across multiple availability zones, regions, and data centers with a single cluster or multiple clusters. Nomad is designed to natively handle multi-cluster deployments without the overhead of running clusters on clusters. This makes it easier to scale the application deployment across multiple datacenters, regions, and clouds with no additional complexity.

Nomad has performed strenuous benchmark on scalability with 1 million container challenge in 2016 and 2 million container challenge in 2020. These tests are aimed to validate Nomad's architectural design and ensure that Nomad performs under the most extreme requirements.

# Supplement to Kubernetes
Enterprises are comprised of multiple groups of people (business units) with different projects, infrastructure environments, technical competencies, team sizes, budgets, and SLAs. Each group has different requirements and leverages technologies based on their particular needs and constraints.

Medium to large scale enterprises run into challenges when trying to standardize hundreds to thousands of software developers and administrators onto one single orchestrator (Kubernetes, Nomad, Mesos) as no scheduler today fits all applications, environments, projects, and teams.

Companies in the Global 2000 today such as Intel, Autodesk and GitHub with multiple products and business units organically run Nomad and Kubernetes to supplement each other. They leverage each scheduler to its strengths with Kubernetes for its cutting edge ecosystem and Nomad for simple maintenance and flexibility in core scheduling.

# How organizations leverage Nomad and Kubernetes

These are the characteristics we see in teams that typically adopt self-hosted Kubernetes:

Greenfield use cases such as machine learning (ML), serverless, and big data that require the Kubernetes ecosystem and Helm chart
High budget and full-time staffing to maintain Kubernetes
High-profile projects with significant investment and long-term timeline (multi-year)
Deploying and managing new, cloud-native applications
Public cloud environment such as AWS, GCP, Azure

# Characteristics of teams that typically adopt Nomad:

Run a mix of containerized and non-containerized workloads (Windows, Java)
Small/medium-sized teams with limited capacity to maintain an orchestrator
Deploying and managing core, existing applications
On-premises environment, or hybrid environments
Require simplicity to move fast and fulfill business needs with hard deadlines
We continue to see small enterprises continue to standardize on a single orchestrator given the natural staffing and organizational constraints. There are not enough DevOps members to maintain more than one orchestrator, not enough developers to warrant diverging workflows, or simply not enough workload diversity to require more than one orchestrator.

# Resources
Review the following resources for in-depth comparisons between Nomad and Kubernetes:

[A Kubernetes User's Guide to HashiCorp Nomad](https://www.hashicorp.com/en/blog/a-kubernetes-user-s-guide-to-hashicorp-nomad)
[The Kubernetes to Nomad Cheat Sheet](https://www.hashicorp.com/en/blog/the-kubernetes-to-nomad-cheat-sheet)
[A Kubernetes User's Guide to HashiCorp Nomad Secret Management](https://www.hashicorp.com/en/blog/a-kubernetes-user-s-guide-to-hashicorp-nomad-secret-management)