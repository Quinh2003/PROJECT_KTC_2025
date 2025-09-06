import Cookies from 'js-cookie';

// Lưu token vào cookie
export function setTokenCookie(token: string) {
	if (typeof document !== 'undefined') {
		document.cookie = `access_token=${token}; path=/; max-age=86400; SameSite=Lax; Secure`;
	}
}

// Lấy token từ cookie
export function getTokenCookie(): string | undefined {
	return Cookies.get('access_token');
}

// Xóa token khỏi cookie
export function removeTokenCookie() {
	Cookies.remove('access_token');
}

// Lưu refresh token vào cookie
export function setRefreshTokenCookie(refreshToken: string) {
	Cookies.set('refresh_token', refreshToken, { expires: 7, secure: true });
}

// Lấy refresh token từ cookie
export function getRefreshTokenCookie(): string | undefined {
	return Cookies.get('refresh_token');
}

// Xóa refresh token khỏi cookie
export function removeRefreshTokenCookie() {
	Cookies.remove('refresh_token');
}

// Hàm tự động refresh token khi hết hạn
export async function autoRefreshToken(apiRefreshUrl: string): Promise<string | null> {
	const refreshToken = getRefreshTokenCookie();
	if (!refreshToken) return null;
	try {
		const res = await fetch(apiRefreshUrl, {
			method: 'POST',
			headers: { 'Content-Type': 'application/json' },
			body: JSON.stringify({ refreshToken })
		});
		if (!res.ok) return null;
		const data = await res.json();
		if (data.token) {
			setTokenCookie(data.token);
			return data.token;
		}
		return null;
	} catch {
		return null;
	}
}
// Hàm kiểm tra token có tồn tại không
export function isAuthenticated(): boolean {
	if (typeof window === 'undefined') return false;
	const token = localStorage.getItem('token');
	return !!token;
}

// Hàm lấy token từ localStorage
export function getToken(): string | null {
	if (typeof window === 'undefined') return null;
	return localStorage.getItem('token');
}

// Hàm decode JWT (chỉ lấy payload, không xác thực signature)
export function decodeJWT(token: string): any {
	if (!token) return null;
	try {
		const payload = token.split('.')[1];
		// Dùng Buffer cho môi trường server-side (Node.js)
		const decoded = Buffer.from(payload, 'base64').toString('utf-8');
		return JSON.parse(decoded);
	} catch {
		return null;
	}
}

// Hàm kiểm tra token hết hạn
export function isTokenExpired(token: string): boolean {
	const payload = decodeJWT(token);
	if (!payload || !payload.exp) return true;
	const now = Math.floor(Date.now() / 1000);
	return payload.exp < now;
}

// Hàm bảo vệ route (dùng trong component/page)
export function requireAuth(redirectUrl: string = '/login') {
	if (!isAuthenticated() || isTokenExpired(getToken()!)) {
		if (typeof window !== 'undefined') {
			window.location.href = redirectUrl;
		}
		return false;
	}
	return true;
}
