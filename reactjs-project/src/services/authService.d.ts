import type { LoginRequest, RegisterRequest, AuthUser } from '../types/User';
export declare class AuthService {
    private static instance;
    static getInstance(): AuthService;
    login(credentials: LoginRequest): Promise<AuthUser>;
    register(userData: RegisterRequest): Promise<AuthUser>;
    logout(): Promise<void>;
    getCurrentUser(): Promise<AuthUser | null>;
    refreshToken(): Promise<string | null>;
    isAuthenticated(): boolean;
    getStoredUser(): AuthUser | null;
    getToken(): string | null;
}
export declare const authService: AuthService;
