// src/server/auth.api.ts
// Các hàm gọi API liên quan đến authentication

export async function loginApi(email: string, password: string) {
  const res = await fetch("http://localhost:8080/api/auth/login", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ email, password }),
  });
  return res;
}

export async function googleLoginApi(accessToken: string) {
  const res = await fetch("http://localhost:8080/api/auth/google-login", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ accessToken }),
  });
  return res;
}
