#!/usr/bin/env julia

# Load the main module
push!(LOAD_PATH, "src")
using NexpoAPI

# Start the server
NexpoAPI.start_server()
