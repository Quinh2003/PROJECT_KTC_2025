#!/bin/bash

# Script để chạy tất cả các services (Backend + Frontend)
# Author: KTC Project
# Date: $(date +%Y-%m-%d)

echo "🚀 Starting All KTC Project Services..."
echo "======================================"

# Kiểm tra các dependency cần thiết
echo "📋 Checking system requirements..."

# Kiểm tra Java
if command -v java &> /dev/null; then
    echo "✅ Java found: $(java -version 2>&1 | head -1)"
else
    echo "❌ Java not found! Please install Java 21 or higher."
    exit 1
fi

# Kiểm tra pnpm
if command -v pnpm &> /dev/null; then
    echo "✅ pnpm found: $(pnpm --version)"
else
    echo "❌ pnpm not found! Please install pnpm first:"
    echo "npm install -g pnpm"
    exit 1
fi

# Function để dọn dẹp processes khi script bị thoát
cleanup() {
    echo ""
    echo "🛑 Stopping all services..."
    
    # Kill tất cả background jobs
    jobs -p | xargs -r kill
    
    # Đợi một chút để processes dọn dẹp
    sleep 2
    
    echo "✅ All services stopped."
    exit 0
}

# Đăng ký cleanup function để chạy khi script bị interrupt
trap cleanup SIGINT SIGTERM

echo ""
echo "🔧 Starting services..."
echo "Press Ctrl+C to stop all services"
echo ""

# Tạo log directory nếu chưa có
mkdir -p logs

# Function để chạy service trong background
run_service() {
    local service_name=$1
    local command=$2
    local log_file=$3
    
    echo "🚀 Starting $service_name..."
    eval "$command" > "$log_file" 2>&1 &
    local pid=$!
    echo "   PID: $pid, Log: $log_file"
    
    # Đợi một chút để service khởi động
    sleep 2
    
    # Kiểm tra xem process có còn chạy không
    if kill -0 $pid 2>/dev/null; then
        echo "✅ $service_name started successfully"
    else
        echo "❌ $service_name failed to start. Check log: $log_file"
        return 1
    fi
}

# Chạy Spring Boot Backend
echo "1️⃣ Starting Spring Boot Backend..."
cd spring-project
chmod +x ./gradlew
cd ..
run_service "Spring Boot Backend" "cd spring-project && ./gradlew bootRun" "logs/backend.log"

# Đợi backend khởi động hoàn toàn
echo "⏳ Waiting for backend to fully start (30 seconds)..."
sleep 30

# Chạy Next.js Frontend
echo "2️⃣ Starting Next.js Frontend..."
cd nextjs-project
pnpm install --silent
cd ..
run_service "Next.js Frontend" "cd nextjs-project && pnpm dev" "logs/nextjs.log"

# Đợi Next.js khởi động
echo "⏳ Waiting for Next.js to start (10 seconds)..."
sleep 10

# Chạy React Frontend
echo "3️⃣ Starting React Frontend..."
cd reactjs-project
pnpm install --silent
cd ..
run_service "React Frontend" "cd reactjs-project && pnpm dev" "logs/react.log"

echo ""
echo "🎉 All services are running!"
echo "================================"
echo "📊 Service URLs:"
echo "   🔧 Spring Boot Backend: http://localhost:8080"
echo "   🌐 Next.js Frontend:    http://localhost:3000"
echo "   ⚛️  React Frontend:     http://localhost:5173"
echo ""
echo "📋 Log files:"
echo "   📄 Backend:  logs/backend.log"
echo "   📄 Next.js:  logs/nextjs.log"
echo "   📄 React:    logs/react.log"
echo ""
echo "💡 Tips:"
echo "   - Use 'tail -f logs/[service].log' to monitor logs"
echo "   - Press Ctrl+C to stop all services"
echo "   - Check individual logs if any service fails"
echo ""
echo "⏳ Services are running... Press Ctrl+C to stop all"

# Vòng lặp vô tận để giữ script chạy
while true; do
    sleep 1
done
