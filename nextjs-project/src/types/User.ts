export interface User {
  id?: number;
  email: string;
  fullName: string;
  role?: string;
  token?: string;
}

export interface LoginCredentials {
  email: string;
  password: string;
}

export interface RegisterData {
  email: string;
  password: string;
  fullName: string;
}

export interface AuthResponse {
  user: User;
  token: string;
}
