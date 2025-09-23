#!/bin/bash

# Script để chạy Next.js Frontend
# Author: KTC Project
# Date: $(date +%Y-%m-%d)

echo "🚀 Starting Next.js Frontend..."
echo "==============================="

# Kiểm tra pnpm
echo "📋 Checking pnpm installation..."
if command -v pnpm &> /dev/null; then
    echo "✅ pnpm found: $(pnpm --version)"
else
    echo "❌ pnpm not found! Please install pnpm first:"
    echo "npm install -g pnpm"
    exit 1
fi

# Di chuyển đến thư mục nextjs-project
cd nextjs-project

# Kiểm tra package.json
if [ ! -f "package.json" ]; then
    echo "❌ package.json not found in nextjs-project directory!"
    exit 1
fi

echo "📦 Installing dependencies..."
pnpm install

echo "🔧 Starting Next.js development server..."
echo "🌐 Application will be available at: http://localhost:3000"

# Chạy Next.js development server
pnpm dev

echo "🛑 Next.js Frontend stopped."
