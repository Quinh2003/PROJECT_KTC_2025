// Authentication utility functions

export const getAuthToken = (): string | null => {
  if (typeof window === "undefined") return null;
  return localStorage.getItem("token");
};

export const getAuthHeaders = (): HeadersInit => {
  const token = getAuthToken();
  return {
    "Content-Type": "application/json",
    ...(token && { Authorization: `Bearer ${token}` }),
  };
};

export const isTokenExpired = (token: string): boolean => {
  try {
    const payload = JSON.parse(atob(token.split(".")[1]));
    return payload.exp * 1000 < Date.now();
  } catch {
    return true;
  }
};

export const debugAuth = (): void => {
  const token = getAuthToken();
  console.log("=== AUTH DEBUG ===");
  console.log("Token exists:", !!token);
  console.log("Token length:", token?.length || 0);
  if (token) {
    console.log("Token expired:", isTokenExpired(token));
    console.log("Token preview:", token.substring(0, 50) + "...");
  }
  console.log("User in localStorage:", localStorage.getItem("user"));
  console.log("==================");
};
