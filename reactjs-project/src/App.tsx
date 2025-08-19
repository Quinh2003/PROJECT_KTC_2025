import { useState } from "react";
import { BrowserRouter, Routes, Route, Navigate } from "react-router";
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import LoginForm from "./components/LoginForm";
import Dashboard from "./pages/Dashboard";
import type { User } from "./types/User";

// Tạo QueryClient instance
const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      staleTime: 5 * 60 * 1000, // 5 phút
      retry: 1,
      refetchOnWindowFocus: false,
    },
  },
});

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
    <QueryClientProvider client={queryClient}>
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
    </QueryClientProvider>
  );
}

export default App;