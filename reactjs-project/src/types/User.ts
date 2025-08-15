export type Role = "ADMIN" | "DISPATCHER" | "FLEET" | "DRIVER" | "OPERATIONS" | "CUSTOMER";

export interface User {
  id: number;
  name: string;
  email: string;
  password: string;
  role: string;
  status: string;
  lastLogin: string;
  phone: string;
}
