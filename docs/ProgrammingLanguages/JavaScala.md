Java [https://www.java.com/](Programming Language)
Scala [https://www.scala-lang.org/](Programming Language)

Play Framework [https://www.playframework.com/](Web Framework)

# Java & Scala Installation Guide (Latest Version)

## Overview

- **Java**: [https://adoptium.net/] (OpenJDK recommended)
- **Scala**: [https://www.scala-lang.org/download/]
- Java is a mature, enterprise-grade programming language with strong ecosystem support.
- Scala is a functional programming language that runs on the JVM and interoperates with Java.
- Current Java LTS version: **Java 21** (as of July 2025)
- Current Scala version: **Scala 3.4.x**
- Build tools: **Gradle** (preferred) or **Maven** for Java, **sbt** for Scala

---

## Java Installation

### Windows Installation

#### Step 1: Download OpenJDK

- Visit: [https://adoptium.net/temurin/releases/]
- Download Java 21 LTS (x64 MSI installer)
- Run the installer and follow the setup wizard
- **Important**: Check "Set JAVA_HOME variable" and "Add to PATH"

#### Step 2: Verify Java Installation

```powershell
java -version
javac -version
echo $env:JAVA_HOME
```

#### Step 3: Install Gradle (Recommended Build Tool)

```powershell
# Using Chocolatey
choco install gradle

# Or download manually from https://gradle.org/install/
```

#### Step 4: Verify Gradle

```powershell
gradle --version
```

### macOS Installation

#### Step 1: Install Java via Homebrew

```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install OpenJDK 21
brew install openjdk@21

# Link OpenJDK
sudo ln -sfn $(brew --prefix)/opt/openjdk@21/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk-21.jdk
```

#### Step 2: Set JAVA_HOME

```bash
echo 'export JAVA_HOME=$(/usr/libexec/java_home -v21)' >> ~/.zshrc
source ~/.zshrc
```

#### Step 3: Install Gradle

```bash
brew install gradle
```

#### Step 4: Verify Installation

```bash
java -version
javac -version
gradle --version
```

### Ubuntu/Debian Linux Installation

#### Step 1: Install OpenJDK

```bash
sudo apt update
sudo apt install openjdk-21-jdk
```

#### Step 2: Set JAVA_HOME

```bash
echo 'export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64' >> ~/.bashrc
echo 'export PATH=$PATH:$JAVA_HOME/bin' >> ~/.bashrc
source ~/.bashrc
```

#### Step 3: Install Gradle

```bash
wget https://services.gradle.org/distributions/gradle-8.5-bin.zip
sudo unzip -d /opt/gradle gradle-8.5-bin.zip
echo 'export GRADLE_HOME=/opt/gradle/gradle-8.5' >> ~/.bashrc
echo 'export PATH=$PATH:$GRADLE_HOME/bin' >> ~/.bashrc
source ~/.bashrc
```

#### Step 4: Verify Installation

```bash
java -version
javac -version
gradle --version
```

---

## Scala Installation

### Step 1: Install Scala (All Platforms)

#### Using Coursier (Recommended)

```bash
# Install Coursier
curl -fL https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz | gzip -d > cs
chmod +x cs
./cs setup
```

#### Alternative: Manual Installation

```bash
# Download from official site
wget https://github.com/lampepfl/dotty/releases/download/3.4.0/scala3-3.4.0.tar.gz
tar -xzf scala3-3.4.0.tar.gz
sudo mv scala3-3.4.0 /opt/scala
echo 'export SCALA_HOME=/opt/scala' >> ~/.bashrc
echo 'export PATH=$PATH:$SCALA_HOME/bin' >> ~/.bashrc
source ~/.bashrc
```

### Step 2: Install sbt (Scala Build Tool)

#### Windows

```powershell
choco install sbt
```

#### macOS

```bash
brew install sbt
```

#### Linux

```bash
echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
sudo apt update
sudo apt install sbt
```

### Step 3: Verify Scala Installation

```bash
scala -version
sbt --version
```

---

## Quick Start: Play Framework

### Java Play Framework

#### Step 1: Create Play Java Project

```bash
sbt new playframework/play-java-seed.g8
```

#### Step 2: Navigate and Run

```bash
cd my-play-java-app
sbt run
```

#### Step 3: Create API Controller

Create `app/controllers/ApiController.java`:
```java
package controllers;

import play.mvc.*;
import play.libs.Json;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class ApiController extends Controller {
    
    public Result health() {
        ObjectNode result = Json.newObject();
        result.put("message", "Hello from Java Play API!");
        result.put("status", "success");
        result.put("port", 8070);
        return ok(result);
    }
    
    public Result getUser(String id) {
        ObjectNode user = Json.newObject();
        user.put("user_id", id);
        user.put("name", "User " + id);
        user.put("platform", "Java");
        return ok(user);
    }
}
```

#### Step 4: Add Routes

Update `conf/routes`:
```
GET     /health                 controllers.ApiController.health()
GET     /api/users/:id          controllers.ApiController.getUser(id: String)
```

### Scala Play Framework

#### Step 1: Create Play Scala Project

```bash
sbt new playframework/play-scala-seed.g8
```

#### Step 2: Navigate and Run

```bash
cd my-play-scala-app
sbt run
```

#### Step 3: Create API Controller

Create `app/controllers/ApiController.scala`:
```scala
package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._

@Singleton
class ApiController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {

  def health() = Action { implicit request: Request[AnyContent] =>
    val response = Json.obj(
      "message" -> "Hello from Scala Play API!",
      "status" -> "success",
      "port" -> 8060
    )
    Ok(response)
  }

  def getUser(id: String) = Action { implicit request: Request[AnyContent] =>
    val user = Json.obj(
      "user_id" -> id,
      "name" -> s"User $id",
      "platform" -> "Scala"
    )
    Ok(user)
  }
}
```

#### Step 4: Add Routes

Update `conf/routes`:
```
GET     /health                 controllers.ApiController.health()
GET     /api/users/:id          controllers.ApiController.getUser(id: String)
```

---

## Summary Table

| Platform   | Java Install        | Scala Install    | Build Tool | Framework Install    |
|------------|---------------------|------------------|------------|---------------------|
| Windows    | Adoptium OpenJDK    | Coursier/Manual  | Gradle/sbt | Play template       |
| macOS      | Homebrew OpenJDK    | Coursier/Manual  | Gradle/sbt | Play template       |
| Linux      | APT OpenJDK         | Coursier/Manual  | Gradle/sbt | Play template       |

---

## Why Java & Scala for APIs?

### Java Benefits:
- **Mature ecosystem** with extensive libraries
- **Enterprise support** and long-term stability
- **JVM performance** and optimization
- **Strong typing** and compile-time error checking
- **Excellent tooling** and IDE support

### Scala Benefits:
- **Functional programming** paradigm
- **Concise syntax** reducing boilerplate
- **Java interoperability** - use Java libraries seamlessly
- **Advanced type system** with pattern matching
- **Actor model** with Akka for concurrency

Play Framework is a web framework for Java and Scala that provides features for routing, templating, authentication, and creating API backends. You can use it to build a performant server-side API that handles data processing and logic in Java or Scala.