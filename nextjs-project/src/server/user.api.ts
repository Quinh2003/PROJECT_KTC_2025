// src/server/user.api.ts
// Các hàm gọi API liên quan đến user (đăng ký, quên mật khẩu)

export async function registerUserApi(email: string, password: string, name: string, phone: string) {
  const res = await fetch("http://localhost:8080/api/auth/users", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ email, password, fullName: name, phone }),
  });
  return res;
}

export async function forgotPasswordApi(email: string) {
  const res = await fetch("http://localhost:8080/api/auth/forgot-password", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ email }),
  });
  return res;
}
