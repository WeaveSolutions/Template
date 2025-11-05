PHP [https://www.php.net/](Programming Language built in C, C++)

Composer [https://getcomposer.org/](Package Manager)
Laravel [https://laravel.com/](Web Framework)

# PHP Installation Guide (Latest Version)

## Overview

- Official PHP site: [https://www.php.net/downloads.php]
- PHP is a server-side scripting language used in web development and CLI applications.  
- Current stable version (as of July 2025): PHP 8.3.x

---

## Windows Installation (Manual Setup)

### Step 1: Download PHP

- Visit: [https://windows.php.net/download/]
- Download the latest Thread Safe x64 ZIP package (e.g., `php-8.3.x-Win32-vs16-x64.zip`)
- Extract the contents to `C:\php`

### Step 2: Add PHP to System PATH

- Open Start Menu → Search: `Environment Variables`
- Click: "Edit the system environment variables"
- In the System Properties window, click the "Environment Variables..." button
- Under "System variables", find and select `Path` → Click "Edit"
- Click "New" → Add: `C:\php`
- Click OK to save and exit all dialogs

### Step 3: Configure php.ini

- Go to `C:\php`
- Rename `php.ini-development` to `php.ini`
- Open `php.ini` in a text editor and:
  - Enable common extensions:
    ```
    extension=curl
    extension=mbstring
    extension=mysqli
    extension=openssl
    ```
  - Set your timezone:
    ```
    date.timezone = "America/New_York"
    ```

### Step 4: Restart Terminal and Verify

- Close and reopen PowerShell or Command Prompt
- Run:
  ```
  php -v
  ```

### Step 5: (Optional) Install Composer

- Download from: [https://getcomposer.org/download/]
- Run the installer
- After installation, verify with:
  ```
  composer --version
  ```

---

## macOS Installation (Homebrew)

### Step 1: Install Homebrew (if not already)

```
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

### Step 2: Install PHP

```
brew update
brew install php
```

### Step 3: Link PHP (if not auto-linked)

```
brew link --overwrite --force php
```

### Step 4: Verify Installation

```
php -v
```

### Step 5: (Optional) Install Composer

```
curl -sS https://getcomposer.org/installer | php
sudo mv composer.phar /usr/local/bin/composer
composer --version
```

---

## Ubuntu / Debian Linux Installation

### Step 1: Add the PHP Repository

```
sudo apt update
sudo apt install software-properties-common
sudo add-apt-repository ppa:ondrej/php
sudo apt update
```

### Step 2: Install PHP and Common Extensions

```
sudo apt install php8.3 php8.3-mbstring php8.3-curl php8.3-mysql php8.3-xml
```

### Step 3: Verify Installation

```
php -v
```

### Step 4: (Optional) Install Composer

```
curl -sS https://getcomposer.org/installer | php
sudo mv composer.phar /usr/local/bin/composer
composer --version
```

---

## Optional: Run a Local Development Server

```
php -S localhost:8000
```

Place an `index.php` file in your working directory and visit `http://localhost:8000` in a browser.

---

## Summary Table

| Platform | Install Method      | PATH Setup        | Composer           |
|----------|---------------------|-------------------|--------------------|
| Windows  | Manual ZIP          | Manual (Env Vars) | Composer installer |
| macOS    | Homebrew            | Auto / brew link  | curl or brew       |
| Linux    | APT + PPA           | Auto              | curl method        |
