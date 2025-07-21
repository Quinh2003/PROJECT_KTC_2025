# KTC Spring Boot Project

Một dự án Spring Boot cơ bản với REST API để quản lý người dùng.

## 🚀 Tính năng

- **RESTful API** cho quản lý người dùng (CRUD operations)
- **Spring Data JPA** với H2 Database (development) và MySQL (production)
- **Validation** cho dữ liệu đầu vào
- **Exception handling** cơ bản
- **DevTools** để hot reload trong development

## 📋 Yêu cầu hệ thống

- Java 17 hoặc cao hơn
- Maven 3.6 hoặc cao hơn
- IDE hỗ trợ Spring Boot (IntelliJ IDEA, VS Code với Spring Boot Extension)

## 🛠️ Cài đặt và chạy

### 1. Clone dự án
```bash
cd springboot-project
```

### 2. Build dự án
```bash
mvn clean install
```

### 3. Chạy ứng dụng
```bash
mvn spring-boot:run
```

Hoặc chạy từ file JAR:
```bash
java -jar target/springboot-project-0.0.1-SNAPSHOT.jar
```

### 4. Truy cập ứng dụng
- **API Base URL:** http://localhost:8080/api
- **H2 Console:** http://localhost:8080/api/h2-console
- **Health Check:** http://localhost:8080/api/health

## 📚 API Endpoints

### Home & Health
- `GET /api/` - Welcome message
- `GET /api/health` - Health check

### User Management
- `GET /api/users` - Lấy danh sách tất cả người dùng
- `GET /api/users/{id}` - Lấy thông tin người dùng theo ID
- `GET /api/users/email/{email}` - Lấy thông tin người dùng theo email
- `POST /api/users` - Tạo người dùng mới
- `PUT /api/users/{id}` - Cập nhật thông tin người dùng
- `DELETE /api/users/{id}` - Xóa người dùng
- `GET /api/users/search?name={name}` - Tìm kiếm người dùng theo tên
- `GET /api/users/check-email?email={email}` - Kiểm tra email có tồn tại không

## 📝 Ví dụ sử dụng API

### Tạo người dùng mới
```bash
curl -X POST http://localhost:8080/api/users \
  -H "Content-Type: application/json" \
  -d '{
    "firstName": "Nguyen",
    "lastName": "Van A",
    "email": "nguyenvana@example.com",
    "password": "password123"
  }'
```

### Lấy danh sách người dùng
```bash
curl http://localhost:8080/api/users
```

### Tìm kiếm người dùng
```bash
curl http://localhost:8080/api/users/search?name=Nguyen
```

## 🗂️ Cấu trúc dự án

```
src/
├── main/
│   ├── java/com/ktc/springbootproject/
│   │   ├── SpringBootProjectApplication.java  # Main class
│   │   ├── controller/                        # REST Controllers
│   │   │   ├── HomeController.java
│   │   │   └── UserController.java
│   │   ├── service/                          # Business Logic
│   │   │   └── UserService.java
│   │   ├── repository/                       # Data Access Layer
│   │   │   └── UserRepository.java
│   │   ├── model/                           # Entity Classes
│   │   │   └── User.java
│   │   └── dto/                             # Data Transfer Objects
│   │       └── UserDTO.java
│   └── resources/
│       └── application.properties            # Configuration
└── test/
    └── java/com/ktc/springbootproject/
        └── SpringBootProjectApplicationTests.java
```

## 🔧 Configuration

Các cấu hình quan trọng trong `application.properties`:

- **Server Port:** 8080
- **Context Path:** /api
- **Database:** H2 (in-memory) cho development
- **JPA:** Hibernate với auto DDL

## 📊 Database Schema

### Users Table
| Column     | Type         | Constraints              |
|------------|--------------|--------------------------|
| id         | BIGINT       | PRIMARY KEY, AUTO_INCREMENT |
| first_name | VARCHAR(50)  | NOT NULL                 |
| last_name  | VARCHAR(50)  | NOT NULL                 |
| email      | VARCHAR(100) | NOT NULL, UNIQUE         |
| password   | VARCHAR(255) | NOT NULL                 |
| created_at | DATETIME     | NOT NULL                 |
| updated_at | DATETIME     | NOT NULL                 |

## 🧪 Testing

Chạy tests:
```bash
mvn test
```

## 🚀 Deployment

### Development
- Database: H2 (in-memory)
- Profiles: default

### Production
1. Thay đổi database configuration trong `application.properties`
2. Sử dụng MySQL hoặc PostgreSQL
3. Cấu hình environment variables cho production

## 🤝 Contributing

1. Fork dự án
2. Tạo feature branch (`git checkout -b feature/amazing-feature`)
3. Commit changes (`git commit -m 'Add some amazing feature'`)
4. Push to branch (`git push origin feature/amazing-feature`)
5. Tạo Pull Request

## 📄 License

Dự án này được cấp phép dưới MIT License - xem file [LICENSE](LICENSE) để biết thêm chi tiết.
