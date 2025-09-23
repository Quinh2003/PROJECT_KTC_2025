#!/bin/bash

# Script để chạy Spring Boot Backend
# Author: KTC Project
# Date: $(date +%Y-%m-%d)

echo "🚀 Starting Spring Boot Backend..."
echo "================================="

# Kiểm tra Java version
echo "📋 Checking Java version..."
if command -v java &> /dev/null; then
    java -version
else
    echo "❌ Java not found! Please install Java 21 or higher."
    exit 1
fi

# Di chuyển đến thư mục spring-project
cd spring-project

# Kiểm tra xem có file gradlew không
if [ ! -f "./gradlew" ]; then
    echo "❌ gradlew not found in spring-project directory!"
    exit 1
fi

# Cấp quyền thực thi cho gradlew (nếu cần)
chmod +x ./gradlew

echo "🔧 Building and starting Spring Boot application..."
echo "This may take a few minutes for the first run..."

# Chạy Spring Boot application
./gradlew bootRun

echo "🛑 Spring Boot Backend stopped."
