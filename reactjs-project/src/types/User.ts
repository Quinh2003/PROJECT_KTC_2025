import type { ReactNode } from "react";

export type Role = "ADMIN" | "DISPATCHER" | "FLEET" | "DRIVER" | "OPERATIONS";

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
  fullName: ReactNode;
  status: string;
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
  fullName: ReactNode;
  status: string;
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