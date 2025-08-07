export type Role = "ADMIN" | "DISPATCHER" | "FLEET_MANAGER" | "DRIVER" | "OPERATIONS_MANAGER";

export interface User {
  email: string;
  password: string;
  role: Role;
  name: string;
}
