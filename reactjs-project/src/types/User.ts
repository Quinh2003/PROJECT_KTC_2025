export type Role = "ADMIN" | "DISPATCHER" | "FLEET" | "DRIVER" | "OPERATIONS";

export interface User {
  fullName: ReactNode;
  status: string;
  email: string;
  password: string;
  role: Role;
  name: string;
}
