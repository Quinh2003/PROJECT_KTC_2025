# KTC Logistics Backend - Spring Boot Application

## 🚀 Docker Hub Repository

**Public Repository:** https://hub.docker.com/r/fanglee2003/ktc-logistics-backend

## 📦 Quick Start

### Pull và chạy container:
```bash
docker pull fanglee2003/ktc-logistics-backend:latest
docker run -d -p 8080:8080 fanglee2003/ktc-logistics-backend:latest
```

### Hoặc sử dụng docker-compose:
```bash
git clone <repository-url>
cd spring-project
docker-compose up -d
```

## 🌐 API Endpoints

- **Base URL:** http://localhost:8080
- **Health Check:** http://localhost:8080/actuator/health
- **API Documentation:** http://localhost:8080/swagger-ui.html

## 🔧 Configuration

Application sử dụng remote MySQL database:
- **Server:** server.aptech.io:3307
- **Database:** fastroute
- **Username:** fastroute_user

## 📊 Features

- ✅ JWT Authentication & RBAC
- ✅ Order Management System
- ✅ Delivery Tracking
- ✅ Vehicle & Driver Management
- ✅ Warehouse Operations
- ✅ Real-time GPS Tracking
- ✅ RESTful API with Spring Boot
- ✅ MySQL Database Integration

## 🏗️ Tech Stack

- **Backend:** Spring Boot 3.5.4, Java 21
- **Database:** MySQL 8.0
- **Authentication:** JWT, Spring Security
- **Build Tool:** Gradle
- **Container:** Docker
- **Runtime:** Amazon Corretto JDK 21

## 👨‍💻 Development

```bash
# Build locally
./gradlew clean bootJar

# Run tests
./gradlew test

# Build Docker image
docker build -t fanglee2003/ktc-logistics-backend:latest .
```

---

**Developed by:** fanglee2003  
**Course:** KTC Project 2025  
**Docker Hub:** https://hub.docker.com/r/fanglee2003/ktc-logistics-backend
