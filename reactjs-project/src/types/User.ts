export type Role = "ADMIN" | "DISPATCHER" | "FLEET" | "DRIVER" | "OPERATIONS";

export interface User {
  email: string;
  password: string;
  role: Role;
  name: string;
}
