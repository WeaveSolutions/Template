# MindsDB Environment Isolation Wrapper
# This script completely removes DEBUG and LOG_LEVEL variables before starting MindsDB

# Remove all DEBUG-related variables
$env:DEBUG = $null
$env:debug = $null
$env:LOG_LEVEL = $null
$env:log_level = $null
$env:VITE_DEBUG = $null

# Set MindsDB-safe variables
$env:NUMEXPR_MAX_THREADS = "16"
$env:PYTHONWARNINGS = "ignore::UserWarning,ignore::DeprecationWarning"

# Get arguments passed from Node.js
$configPath = $args[0]

# Start MindsDB with clean environment
python -m mindsdb --api http,mysql --config $configPath
