#!/bin/bash

# Script để dừng tất cả các services
# Author: KTC Project
# Date: $(date +%Y-%m-%d)

echo "🛑 Stopping All KTC Project Services..."
echo "======================================"

# Function để kill process theo port
kill_process_by_port() {
    local port=$1
    local service_name=$2
    
    echo "🔍 Looking for $service_name on port $port..."
    
    # Tìm process ID theo port
    local pid=$(lsof -ti:$port)
    
    if [ -n "$pid" ]; then
        echo "🛑 Stopping $service_name (PID: $pid)..."
        kill -TERM $pid 2>/dev/null
        
        # Đợi 5 giây để process tự dừng
        sleep 5
        
        # Kiểm tra xem process có còn chạy không
        if kill -0 $pid 2>/dev/null; then
            echo "⚠️  Force killing $service_name..."
            kill -KILL $pid 2>/dev/null
        fi
        
        echo "✅ $service_name stopped"
    else
        echo "ℹ️  $service_name is not running on port $port"
    fi
}

# Function để kill process theo tên
kill_process_by_name() {
    local process_name=$1
    local service_name=$2
    
    echo "🔍 Looking for $service_name processes..."
    
    # Tìm và kill tất cả processes có tên chứa pattern
    local pids=$(pgrep -f "$process_name")
    
    if [ -n "$pids" ]; then
        echo "🛑 Stopping $service_name processes..."
        echo "$pids" | xargs -r kill -TERM
        
        # Đợi 5 giây
        sleep 5
        
        # Force kill nếu cần
        local remaining_pids=$(pgrep -f "$process_name")
        if [ -n "$remaining_pids" ]; then
            echo "⚠️  Force killing remaining $service_name processes..."
            echo "$remaining_pids" | xargs -r kill -KILL
        fi
        
        echo "✅ $service_name processes stopped"
    else
        echo "ℹ️  No $service_name processes found"
    fi
}

# Dừng các services theo port
kill_process_by_port 8080 "Spring Boot Backend"
kill_process_by_port 3000 "Next.js Frontend"
kill_process_by_port 5173 "React Frontend (Vite)"

# Dừng các processes theo tên (backup method)
kill_process_by_name "gradlew bootRun" "Spring Boot"
kill_process_by_name "next dev" "Next.js"
kill_process_by_name "vite" "React Vite"
kill_process_by_name "pnpm dev" "Frontend Dev Servers"

# Dọn dẹp log files cũ (tùy chọn)
read -p "🗑️  Do you want to clear log files? (y/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    if [ -d "logs" ]; then
        rm -f logs/*.log
        echo "✅ Log files cleared"
    fi
fi

echo ""
echo "✅ All services stopped successfully!"
echo "You can now start them again using:"
echo "   ./start-all.sh        (Start all services)"
echo "   ./start-backend.sh    (Backend only)"
echo "   ./start-nextjs.sh     (Next.js only)"
echo "   ./start-react.sh      (React only)"
