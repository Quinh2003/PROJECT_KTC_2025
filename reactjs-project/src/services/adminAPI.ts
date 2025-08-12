// Service to add a new user to API
export async function addUser(user) {
  const token = localStorage.getItem("token");
  const res = await fetch("http://localhost:8080/api/auth/users", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${token}`
    },
    body: JSON.stringify(user),
  });
  if (!res.ok) throw new Error("Failed to add user");
  return res.json();
}

// Service to edit user (update)
export async function editUser(id, user) {
  const token = localStorage.getItem("token");
  const res = await fetch(`http://localhost:8080/api/auth/users/${id}`, {
    method: "PATCH",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${token}`
    },
    body: JSON.stringify(user),
  });
  if (!res.ok) throw new Error("Failed to update user");
  return res.json();
}

// Service to delete user
export async function deleteUser(id) {
  const token = localStorage.getItem("token");
  const res = await fetch(`http://localhost:8080/api/auth/users/${id}`, {
    method: "DELETE",
    headers: {
      "Authorization": `Bearer ${token}`
    },
  });
  if (!res.ok) throw new Error("Failed to delete user");
  return true;
}
// Service to fetch users from API
export async function fetchUsers() {
  const res = await fetch("http://localhost:8080/api/auth/users");
  if (!res.ok) throw new Error("Failed to fetch users");
  return res.json();
}
