// Service to fetch users from API
export async function fetchUsers() {
  const res = await fetch("http://localhost:8080/api/auth/users");
  if (!res.ok) throw new Error("Failed to fetch users");
  return res.json();
}
