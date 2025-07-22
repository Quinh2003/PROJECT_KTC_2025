# 📁 File: 02-auth-page-stories.md

## 🔐 Trang Đăng nhập / Đăng xuất

---

### ✅ US-AUTH-01: User Login

- **As a User**  
  I want to login with email and password  
  So that I can access the system with appropriate permissions  
- **🎯 Priority:** High  
- **🧮 Story Points:** 3  

#### 🎯 Tiêu chí chấp nhận:

- Input validation cho định dạng email và yêu cầu password  
- Checkbox "Remember me"  
- Link "Forgot password"  
- Chuyển hướng đến dashboard sau khi đăng nhập thành công  
- Hiển thị thông báo lỗi nếu thông tin đăng nhập sai

---

### ✅ US-AUTH-02: Password Reset

- **As a User**  
  I want to reset my password when I forget it  
  So that I can regain access to my account  
- **🎯 Priority:** Medium  
- **🧮 Story Points:** 3  

#### 🎯 Tiêu chí chấp nhận:
- Nhập email để nhận liên kết đặt lại mật khẩu  
- Liên kết reset có hiệu lực trong 24 giờ  
- Form để đổi mật khẩu mới  
- Gửi email xác nhận sau khi đổi mật khẩu thành công

---

### ✅ US-AUTH-03: User Registration

- **As a New User**  
  I want to register with my personal information  
  So that I can create a new account and access the system  
- **🎯 Priority:** Medium  
- **🧮 Story Points:** 4  

#### 🎯 Tiêu chí chấp nhận:

- Form đăng ký gồm: Họ tên, email, password, xác nhận password  
- Xác thực định dạng email, password tối thiểu 8 ký tự  
- Kiểm tra password và confirm password phải khớp nhau  
- Checkbox chấp nhận điều khoản dịch vụ  
- Hiển thị thông báo thành công và chuyển hướng sang trang đăng nhập hoặc dashboard  
- Hiển thị lỗi rõ ràng nếu đăng ký không thành công (ví dụ: email đã tồn tại)
