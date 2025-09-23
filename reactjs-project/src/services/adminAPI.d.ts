import type { User } from "../types/User";
export declare function fetchUsers(): Promise<User[]>;
export declare function fetchDrivers(): Promise<User[]>;
export declare function addUser(user: Partial<User>): Promise<User>;
export declare function editUser(id: string | number, user: User): Promise<User>;
export declare function deleteUser(id: string | number): Promise<boolean>;
export declare function updateUserStatus(userId: string | number, status: string): Promise<User>;
export type ActivityLog = {
    id: number;
    time: string;
    user: string;
    action: string;
    detail: string;
    status: string;
    role?: string;
};
export declare function fetchActivityLogs(params?: {
    dateFrom?: string;
    dateTo?: string;
    actionType?: string;
    userId?: number;
    page?: number;
    size?: number;
}, retries?: number): Promise<ActivityLog[]>;
