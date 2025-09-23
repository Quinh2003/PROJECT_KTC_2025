# 🚀 KTC Project Automation Scripts

Các script tự động để chạy backend Spring Boot và frontend Next.js/React mà không cần chạy thủ công.

## 📋 Yêu cầu hệ thống

### Bắt buộc
- **Java 21+** (cho Spring Boot backend)
- **pnpm** (cho frontend projects)
- **Node.js** (được cài cùng với pnpm)

### Cài đặt pnpm (nếu chưa có)
```bash
npm install -g pnpm
```

## 📁 Cấu trúc Scripts

```
PROJECT_KTC_2025/
├── start-backend.sh      # Chạy chỉ Spring Boot backend
├── start-nextjs.sh       # Chạy chỉ Next.js frontend
├── start-react.sh        # Chạy chỉ React frontend
├── start-all.sh          # Chạy tất cả services (Linux/macOS)
├── start-all.bat         # Chạy tất cả services (Windows)
├── stop-all.sh           # Dừng tất cả services
└── SCRIPTS_README.md     # File hướng dẫn này
```

## 🎯 Cách sử dụng

### Linux/macOS

1. **Cấp quyền thực thi cho scripts:**
   ```bash
   chmod +x *.sh
   ```

2. **Chạy tất cả services:**
   ```bash
   ./start-all.sh
   ```

3. **Chạy từng service riêng lẻ:**
   ```bash
   ./start-backend.sh    # Chỉ backend
   ./start-nextjs.sh     # Chỉ Next.js
   ./start-react.sh      # Chỉ React
   ```

4. **Dừng tất cả services:**
   ```bash
   ./stop-all.sh
   ```

### Windows

1. **Chạy tất cả services:**
   ```cmd
   start-all.bat
   ```

2. **Chạy từng service:** Sử dụng Git Bash hoặc WSL để chạy các file .sh

## 🌐 Service URLs

Sau khi chạy thành công, các services sẽ có sẵn tại:

| Service | URL | Mô tả |
|---------|-----|-------|
| Spring Boot Backend | http://localhost:8080 | REST API backend |
| Next.js Frontend | http://localhost:3000 | Next.js web application |
| React Frontend | http://localhost:5173 | React web application (Vite) |

## 📊 Logs

Khi chạy `start-all.sh`, logs sẽ được lưu tại:

```
logs/
├── backend.log       # Spring Boot logs
├── nextjs.log        # Next.js logs
└── react.log         # React logs
```

**Xem logs realtime:**
```bash
tail -f logs/backend.log    # Backend logs
tail -f logs/nextjs.log     # Next.js logs
tail -f logs/react.log      # React logs
```

## 🛠️ Troubleshooting

### Lỗi thường gặp

1. **"Permission denied"**
   ```bash
   chmod +x *.sh
   ```

2. **"Java not found"**
   - Cài đặt Java 21+ và đảm bảo `JAVA_HOME` được set đúng

3. **"pnpm not found"**
   ```bash
   npm install -g pnpm
   ```

4. **Port đã được sử dụng**
   - Chạy `./stop-all.sh` để dừng các services cũ
   - Hoặc kill process theo port:
     ```bash
     lsof -ti:8080 | xargs kill  # Kill backend
     lsof -ti:3000 | xargs kill  # Kill Next.js
     lsof -ti:5173 | xargs kill  # Kill React
     ```

### Kiểm tra services đang chạy

```bash
# Kiểm tra ports
lsof -i :8080  # Backend
lsof -i :3000  # Next.js
lsof -i :5173  # React

# Kiểm tra processes
ps aux | grep java     # Backend
ps aux | grep node     # Frontend
```

## 💡 Tips

1. **Chạy trong background:**
   ```bash
   nohup ./start-all.sh &
   ```

2. **Auto-restart khi file thay đổi:**
   - Spring Boot: Đã có spring-boot-devtools
   - Next.js: Đã có hot reload
   - React: Đã có Vite hot reload

3. **Chạy production build:**
   ```bash
   # Next.js
   cd nextjs-project && pnpm build && pnpm start
   
   # React
   cd reactjs-project && pnpm build && pnpm preview
   
   # Spring Boot
   cd spring-project && ./gradlew bootJar
   java -jar build/libs/*.jar
   ```

## 🔧 Customization

### Thay đổi ports

1. **Spring Boot:** Sửa `application.properties` hoặc `application.yml`
2. **Next.js:** Sửa trong `package.json` scripts: `"dev": "next dev -p 3001"`
3. **React:** Sửa trong `vite.config.ts`: `server: { port: 5174 }`

### Thêm environment variables

Tạo file `.env` trong mỗi project directory:

```bash
# spring-project/.env
SPRING_PROFILES_ACTIVE=dev
DATABASE_URL=jdbc:mysql://localhost:3306/ktc_db

# nextjs-project/.env.local
NEXT_PUBLIC_API_URL=http://localhost:8080
NEXTAUTH_SECRET=your-secret

# reactjs-project/.env
VITE_API_URL=http://localhost:8080
```

## 📞 Support

Nếu gặp vấn đề, hãy:
1. Kiểm tra logs trong thư mục `logs/`
2. Đảm bảo tất cả dependencies đã được cài đặt
3. Kiểm tra ports không bị conflict
4. Restart lại các services bằng `./stop-all.sh` và `./start-all.sh`
