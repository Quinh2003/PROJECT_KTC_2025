# ⚙️ KTC Logistics Management Platform - Backend API

A robust backend API service for KTC Logistics built with Spring Boot 3.5.4 and Java 21. This microservice provides comprehensive RESTful APIs for logistics management, including user authentication, order processing, fleet management, and real-time tracking capabilities. The backend serves as the core data layer for the KTC Logistics 2025 ecosystem, integrating with PostgreSQL database and providing secure, scalable APIs for web and mobile applications.

![KTC Logistics Backend Architecture](docs/backend-architecture.png)

## 📋 Table of Contents

1. [Getting Started](#-getting-started)
2. [Main Features](#-main-features)
3. [Project Structure](#-project-structure)
4. [Tech Stack](#-tech-stack)
5. [License & Contact](#-license--contact)

## 🚀 Getting Started

### Prerequisites

- **Java**: 21 (LTS)
- **Gradle**: 8.14.3 or later
- **PostgreSQL**: 15 or later
- **Docker**: Latest version (optional)
- **Git**: Latest version

### Installation

1. **Clone the repository**

   ```bash
   git clone https://github.com/Quinh2003/PROJECT_KTC_2025.git
   cd PROJECT_KTC_2025/spring-project
   ```

2. **Database setup**

   ```bash
   # Create PostgreSQL database
   createdb ktc_logistics_db
   
   # Import initial schema (optional)
   psql -d ktc_logistics_db -f docs/schemaDB/database-8-4.sql
   ```

3. **Environment configuration**

   ```bash
   cp application.properties.example src/main/resources/application.properties
   ```

   Configure your database and environment variables:

   ```properties
   spring.datasource.url=jdbc:postgresql://localhost:5432/ktc_logistics_db
   spring.datasource.username=your-db-username
   spring.datasource.password=your-db-password
   
   # JWT Configuration
   app.jwt.secret=your-jwt-secret-key
   app.jwt.expiration=86400000
   
   # Email Configuration
   spring.mail.host=smtp.gmail.com
   spring.mail.username=your-email@gmail.com
   spring.mail.password=your-app-password
   ```

4. **Run the application**
   ```bash
   ./gradlew bootRun
   ```
   API server will be available at [http://localhost:8080](http://localhost:8080)

### Docker Deployment

#### Quick Start with Docker Hub

```bash
# Pull and run the latest image
docker pull fanglee2003/ktc-logistics-backend
docker run -d -p 8080:8080 fanglee2003/ktc-logistics-backend
```

#### Build from Source

```bash
# Build Docker image
docker build -t ktc-logistics-backend .

# Run with Docker Compose
docker-compose up -d
```

### Available Scripts

| Command             | Description                |
| ------------------- | -------------------------- |
| `./gradlew bootRun` | Start development server   |
| `./gradlew build`   | Build production JAR       |
| `./gradlew test`    | Run unit tests             |
| `./gradlew bootJar` | Create executable JAR file |
| `./gradlew clean`   | Clean build artifacts      |

### Test Accounts

#### 🔒 **Admin Account**
- **Email**: `admin@ktclogistics.com`
- **Password**: `Admin123456`

#### 🔒 **Driver Account**
- **Email**: `driver@gmail.com`
- **Password**: `123456`

#### 🔒 **Customer Account**
- **Email**: `customer@ktclogistics.com`
- **Password**: `Customer123456`

## 🚀 Main Features

### 🔐 Authentication & Authorization
- JWT-based authentication with refresh tokens
- Role-based access control (Admin, Dispatcher, Fleet Manager, Driver, Customer)
- Google OAuth 2.0 integration
- Two-factor authentication support
- Password reset and email verification

### 📦 Order Management System
- Complete order lifecycle management
- Real-time order status tracking
- Bulk order processing capabilities
- Order validation and business rules
- Integration with delivery tracking

### 🚚 Fleet & Vehicle Management
- Vehicle registration and maintenance tracking
- Driver assignment and scheduling
- Real-time vehicle telemetry
- Maintenance request system
- Fleet performance analytics

### 🗺️ Delivery & Route Optimization
- Advanced routing algorithms
- GPS-based delivery tracking
- Proof of delivery management
- Route optimization with multiple stops
- Geolocation services integration

### 💰 Billing & Payment Processing
- Automated shipping cost calculation
- Invoice generation and management
- Multiple payment methods support
- Electronic invoice system
- Financial reporting and analytics

### 📊 Analytics & Reporting
- Comprehensive dashboard APIs
- Performance metrics and KPIs
- Custom report generation
- Data export capabilities
- Real-time monitoring endpoints

## ️ Project Structure

```
src/main/java/ktc/spring_project/
├── SpringProjectApplication.java    # Main application entry point
│
├── config/                         # Configuration classes
│   ├── AppConfig.java              # General application configuration
│   ├── JwtAuthenticationFilter.java # JWT authentication filter
│   ├── JwtTokenProvider.java       # JWT token utilities
│   ├── SecurityConfig.java         # Spring Security configuration
│   ├── WebConfig.java              # Web MVC configuration
│   └── OpenApiConfig.java          # Swagger/OpenAPI configuration
│
├── controllers/                    # REST API controllers
│   ├── AuthController.java         # Authentication endpoints
│   ├── UserController.java         # User management endpoints
│   ├── OrderController.java        # Order management endpoints
│   ├── DeliveryController.java     # Delivery management endpoints
│   ├── VehicleController.java      # Vehicle management endpoints
│   ├── DashboardController.java    # Dashboard analytics endpoints
│   └── ...                        # Other specialized controllers
│
├── entities/                       # JPA entities
│   ├── User.java                   # User entity
│   ├── Order.java                  # Order entity
│   ├── Delivery.java               # Delivery entity
│   ├── Vehicle.java                # Vehicle entity
│   └── ...                        # Other domain entities
│
├── repositories/                   # JPA repositories
│   ├── UserRepository.java         # User data access
│   ├── OrderRepository.java        # Order data access
│   └── ...                        # Other repositories
│
├── services/                       # Business logic services
│   ├── AuthService.java            # Authentication business logic
│   ├── UserService.java            # User management business logic
│   ├── OrderService.java           # Order processing business logic
│   └── ...                        # Other business services
│
├── dtos/                          # Data Transfer Objects
│   ├── auth/                      # Authentication DTOs
│   ├── user/                      # User management DTOs
│   ├── order/                     # Order management DTOs
│   └── ...                       # Other DTO packages
│
├── enums/                         # Enumeration classes
│   ├── StatusType.java            # Order/delivery status types
│   ├── PaymentMethod.java         # Payment method types
│   └── ...                       # Other enums
│
└── exceptions/                    # Custom exception classes
    └── ...                       # Global exception handlers
```

## 🛠️ Tech Stack

### Core Technologies

- **Framework**: Spring Boot 3.5.4
- **Language**: Java 21 (LTS)
- **Build Tool**: Gradle 8.14.3
- **Database**: PostgreSQL 15+

### Spring Framework Modules

- **Spring Data JPA**: Database operations and ORM
- **Spring Security**: Authentication and authorization
- **Spring Web**: RESTful web services
- **Spring Validation**: Input validation
- **Spring Mail**: Email functionality

### Additional Libraries

- **JWT**: io.jsonwebtoken:jjwt-api 0.11.5
- **Google Auth**: com.warrenstrange:googleauth 1.5.0
- **PDF Generation**: com.github.librepdf:openpdf 1.3.30
- **API Documentation**: springdoc-openapi-starter-webmvc-ui 2.2.0
- **Database Migration**: Flyway Core (optional)

### Development Tools

- **Testing**: JUnit 5, Spring Boot Test
- **Database**: H2 (testing), PostgreSQL (production)
- **Containerization**: Docker, Docker Compose
- **API Testing**: Postman Collection included

### Security Features

- **JWT Authentication**: Stateless authentication
- **CORS Configuration**: Cross-origin resource sharing
- **Input Validation**: Bean validation with custom validators
- **SQL Injection Protection**: JPA/Hibernate parameterized queries
- **XSS Protection**: Input sanitization and encoding

## 📄 License & Contact

Copyright © 2025 KTC Logistics. All rights reserved.

For technical issues or support:
- **Backend Team**: backend-team@ktclogistics.com
- **Project Lead**: backend-lead@ktclogistics.com
- **API Support**: api-support@ktclogistics.com
- Open an issue in the repository for quick assistance

### Documentation Resources

- **API Documentation**: [Swagger UI](http://localhost:8080/swagger-ui.html)
- **Postman Collection**: [FastRoute.postman_collection.json](FastRoute.postman_collection.json)
- **Database Schema**: [docs/schemaDB/](docs/schemaDB/)
- **OpenAPI Spec**: [http://localhost:8080/v3/api-docs](http://localhost:8080/v3/api-docs)