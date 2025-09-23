@echo off
REM Script để chạy tất cả các services trên Windows
REM Author: KTC Project

echo 🚀 Starting All KTC Project Services...
echo ======================================

REM Kiểm tra Java
echo 📋 Checking Java installation...
java -version >nul 2>&1
if %errorlevel% neq 0 (
    echo ❌ Java not found! Please install Java 21 or higher.
    pause
    exit /b 1
)
echo ✅ Java found

REM Kiểm tra pnpm
echo 📋 Checking pnpm installation...
pnpm --version >nul 2>&1
if %errorlevel% neq 0 (
    echo ❌ pnpm not found! Please install pnpm first:
    echo npm install -g pnpm
    pause
    exit /b 1
)
echo ✅ pnpm found

REM Tạo logs directory
if not exist logs mkdir logs

echo.
echo 🔧 Starting services...
echo.

REM Chạy Spring Boot Backend
echo 1️⃣ Starting Spring Boot Backend...
cd spring-project
start "Spring Boot Backend" cmd /k "gradlew bootRun > ..\logs\backend.log 2>&1"
cd ..

REM Đợi backend khởi động
echo ⏳ Waiting for backend to start...
timeout /t 30 /nobreak >nul

REM Chạy Next.js Frontend
echo 2️⃣ Starting Next.js Frontend...
cd nextjs-project
call pnpm install --silent
start "Next.js Frontend" cmd /k "pnpm dev > ..\logs\nextjs.log 2>&1"
cd ..

REM Đợi Next.js khởi động
echo ⏳ Waiting for Next.js to start...
timeout /t 10 /nobreak >nul

REM Chạy React Frontend
echo 3️⃣ Starting React Frontend...
cd reactjs-project
call pnpm install --silent
start "React Frontend" cmd /k "pnpm dev > ..\logs\react.log 2>&1"
cd ..

echo.
echo 🎉 All services are starting!
echo ================================
echo 📊 Service URLs:
echo    🔧 Spring Boot Backend: http://localhost:8080
echo    🌐 Next.js Frontend:    http://localhost:3000
echo    ⚛️  React Frontend:     http://localhost:5173
echo.
echo 📋 Log files:
echo    📄 Backend:  logs\backend.log
echo    📄 Next.js:  logs\nextjs.log
echo    📄 React:    logs\react.log
echo.
echo 💡 Services are running in separate windows.
echo    Close the command windows to stop individual services.
echo.
pause
