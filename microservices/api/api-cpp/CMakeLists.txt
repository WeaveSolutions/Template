cmake_minimum_required(VERSION 3.16)
project(cpp_api VERSION 1.0.0)

# Enforce 64-bit architecture
if(CMAKE_SIZEOF_VOID_P EQUAL 4)
    message(FATAL_ERROR "32-bit builds are not supported. Please use a 64-bit compiler.")
endif()

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Optimization flags for 64-bit
if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    if(MSVC)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /O2 /arch:AVX2")
    else()
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -march=native")
    endif()
endif()

# Find required packages
find_package(Drogon CONFIG REQUIRED)
find_package(PkgConfig REQUIRED)
pkg_check_modules(JSONCPP jsoncpp)

# Include directories
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# Source files
file(GLOB_RECURSE SOURCES 
    "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cc"
)

# Headers
file(GLOB_RECURSE HEADERS 
    "${CMAKE_CURRENT_SOURCE_DIR}/include/*.h"
    "${CMAKE_CURRENT_SOURCE_DIR}/include/*.hpp"
)

# Create executable
add_executable(${PROJECT_NAME} ${SOURCES} ${HEADERS})

# Link libraries
target_link_libraries(${PROJECT_NAME} 
    PRIVATE 
    Drogon::Drogon
    ${JSONCPP_LIBRARIES}
)

# Compiler flags
target_compile_options(${PROJECT_NAME} PRIVATE 
    -Wall 
    -Wextra 
    -O2
    -DNDEBUG
    ${JSONCPP_CFLAGS_OTHER}
)

# Set output directory
set_target_properties(${PROJECT_NAME} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}
)

# Install target
install(TARGETS ${PROJECT_NAME} DESTINATION bin)
install(FILES config.json DESTINATION bin)
