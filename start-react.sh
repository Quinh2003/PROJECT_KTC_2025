#!/bin/bash

# Script để chạy React Frontend
# Author: KTC Project
# Date: $(date +%Y-%m-%d)

echo "🚀 Starting React Frontend..."
echo "============================="

# Kiểm tra pnpm
echo "📋 Checking pnpm installation..."
if command -v pnpm &> /dev/null; then
    echo "✅ pnpm found: $(pnpm --version)"
else
    echo "❌ pnpm not found! Please install pnpm first:"
    echo "npm install -g pnpm"
    exit 1
fi

# Di chuyển đến thư mục reactjs-project
cd reactjs-project

# Kiểm tra package.json
if [ ! -f "package.json" ]; then
    echo "❌ package.json not found in reactjs-project directory!"
    exit 1
fi

echo "📦 Installing dependencies..."
pnpm install

echo "🔧 Starting React development server..."
echo "🌐 Application will be available at: http://localhost:5173 (Vite default)"

# Chạy React development server
pnpm dev

echo "🛑 React Frontend stopped."
