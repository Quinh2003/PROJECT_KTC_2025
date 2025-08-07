import { useState } from "react";
import { BrowserRouter, Routes, Route, Navigate } from "react-router";
import LoginForm from "./components/LoginForm";
import Dashboard from "./pages/Dashboard";
import type { User } from "./types/User";

function App() {
  const [user, setUser] = useState<User | null>(() => {
    return null;
  });

  const handleLogin = (user: User) => {
    console.log("Đăng nhập thành công:", user); // Debug
    setUser(user);
    localStorage.setItem("user", JSON.stringify(user));
  };

  const handleLogout = () => {
    console.log("Đăng xuất"); // Debug
    setUser(null);
    localStorage.removeItem("user");
  };

  console.log("Current user:", user); // Debug

  return (
    <BrowserRouter>
      <Routes>
        <Route
          path="/"
          element={
            !user ? (
              <LoginForm onLogin={handleLogin} />
            ) : (
              <Navigate to="/dashboard" replace />
            )
          }
        />
        <Route
          path="/dashboard"
          element={
            user ? (
              <Dashboard user={user} onLogout={handleLogout} />
            ) : (
              <Navigate to="/" replace />
            )
          }
        />
      </Routes>
    </BrowserRouter>
  );
}

export default App;