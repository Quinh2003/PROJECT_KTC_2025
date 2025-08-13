export type Role = "ADMIN" | "DISPATCHER" | "FLEET_MANAGER" | "DRIVER" | "OPERATIONS_MANAGER";

export interface User {
  id?: string;
  email: string;
  password?: string; // Optional for responses
  role: Role;
  name: string;
  phone?: string;
  avatar?: string;
  createdAt?: string;
  updatedAt?: string;
  isActive?: boolean;
}

export interface AuthUser {
  id: string;
  email: string;
  role: Role;
  name: string;
  token: string;
}

export interface LoginRequest {
  email: string;
  password: string;
}

export interface LoginResponse {
  success: boolean;
  user: AuthUser;
  message?: string;
}

export interface RegisterRequest {
  email: string;
  password: string;
  name: string;
  role: Role;
  phone?: string;
}

export interface UserApiResponse {
  success: boolean;
  data: User | User[];
  message?: string;
}
