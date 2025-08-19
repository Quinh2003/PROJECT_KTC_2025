import type { 
  LoginRequest, 
  LoginResponse, 
  RegisterRequest, 
  AuthUser, 
  UserApiResponse 
} from '../types/User';

const API_BASE_URL = 'http://localhost:8080/api';

export class AuthService {
  private static instance: AuthService;

  public static getInstance(): AuthService {
    if (!AuthService.instance) {
      AuthService.instance = new AuthService();
    }
    return AuthService.instance;
  }

  async login(credentials: LoginRequest): Promise<AuthUser> {
    try {
      const response = await fetch(`${API_BASE_URL}/auth/login`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(credentials),
      });

      if (!response.ok) {
        throw new Error(`Login failed: ${response.status}`);
      }

      const result: LoginResponse = await response.json();
      
      if (result.success && result.user) {
        // Store token in localStorage
        localStorage.setItem('token', result.user.token);
        localStorage.setItem('user', JSON.stringify(result.user));
        return result.user;
      } else {
        throw new Error(result.message || 'Login failed');
      }
    } catch (error) {
      console.error('Login error:', error);
      throw new Error(error instanceof Error ? error.message : 'Login failed');
    }
  }

  async register(userData: RegisterRequest): Promise<AuthUser> {
    try {
      const response = await fetch(`${API_BASE_URL}/auth/register`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(userData),
      });

      if (!response.ok) {
        throw new Error(`Registration failed: ${response.status}`);
      }

      const result: LoginResponse = await response.json();
      
      if (result.success && result.user) {
        // Store token in localStorage
        localStorage.setItem('token', result.user.token);
        localStorage.setItem('user', JSON.stringify(result.user));
        return result.user;
      } else {
        throw new Error(result.message || 'Registration failed');
      }
    } catch (error) {
      console.error('Registration error:', error);
      throw new Error(error instanceof Error ? error.message : 'Registration failed');
    }
  }

  async logout(): Promise<void> {
    try {
      const token = localStorage.getItem('token');
      
      if (token) {
        await fetch(`${API_BASE_URL}/auth/logout`, {
          method: 'POST',
          headers: {
            'Authorization': `Bearer ${token}`,
            'Content-Type': 'application/json',
          },
        });
      }
    } catch (error) {
      console.error('Logout error:', error);
    } finally {
      // Always clear local storage
      localStorage.removeItem('token');
      localStorage.removeItem('user');
    }
  }

  async getCurrentUser(): Promise<AuthUser | null> {
    try {
      const token = localStorage.getItem('token');
      
      if (!token) {
        return null;
      }

      const response = await fetch(`${API_BASE_URL}/auth/me`, {
        method: 'GET',
        headers: {
          'Authorization': `Bearer ${token}`,
          'Content-Type': 'application/json',
        },
      });

      if (!response.ok) {
        // Token might be expired
        this.logout();
        return null;
      }

      const result: UserApiResponse = await response.json();
      
      if (result.success && result.data) {
        return result.data as AuthUser;
      }
      
      return null;
    } catch (error) {
      console.error('Get current user error:', error);
      this.logout();
      return null;
    }
  }

  async refreshToken(): Promise<string | null> {
    try {
      const token = localStorage.getItem('token');
      
      if (!token) {
        return null;
      }

      const response = await fetch(`${API_BASE_URL}/auth/refresh`, {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${token}`,
          'Content-Type': 'application/json',
        },
      });

      if (!response.ok) {
        this.logout();
        return null;
      }

      const result: { token: string } = await response.json();
      
      if (result.token) {
        localStorage.setItem('token', result.token);
        return result.token;
      }
      
      return null;
    } catch (error) {
      console.error('Refresh token error:', error);
      this.logout();
      return null;
    }
  }

  isAuthenticated(): boolean {
    const token = localStorage.getItem('token');
    return !!token;
  }

  getStoredUser(): AuthUser | null {
    try {
      const userStr = localStorage.getItem('user');
      return userStr ? JSON.parse(userStr) : null;
    } catch (error) {
      console.error('Error parsing stored user:', error);
      return null;
    }
  }

  getToken(): string | null {
    return localStorage.getItem('token');
  }
}

// Export singleton instance
export const authService = AuthService.getInstance();