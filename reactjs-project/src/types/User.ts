<<<<<<< HEAD
// import type { ReactNode } from "react";


export interface Role {
  id: number;
  roleName: string;
  permission?: any;
  description?: string;
  isActive?: boolean;
  createdAt?: string;
  updatedAt?: string;
}

// export interface User {
//   fullName: ReactNode;
//   email: string;
//   password: string;
//   role: Role;
//   name: string;
// }



export interface User {
  id?: string | number;
  email: string;
  password?: string;
  role: Role;
  name?: string;
  fullName?: string;
  username?: string;
  phone?: string;
  avatar?: string;
  createdAt?: string;
  updatedAt?: string;
  isActive?: boolean;
  notes?: string | null;
  googleId?: string | null;
  status?: {
    id: number;
    statusType: string;
    name: string;
    description: string;
    createdAt?: string;
    updatedAt?: string;
  };
}

export interface AuthUser {
  id: string;
  email: string;
  role: Role;
  name: string;
  token: string;
=======
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
>>>>>>> 042a7c16d89d185c6e74a32de79f098e8a6971b5
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