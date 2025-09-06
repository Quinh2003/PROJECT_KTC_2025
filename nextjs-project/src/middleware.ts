import { NextResponse } from 'next/server';
import type { NextRequest } from 'next/server';
import { decodeJWT } from './lib/auth';

// Các route cần bảo vệ
const protectedRoutes = [
  '/account',
  '/(dashboard)',
];

export function middleware(request: NextRequest) {
  const { pathname } = request.nextUrl;
  // Kiểm tra nếu truy cập route cần bảo vệ
  if (protectedRoutes.some(route => pathname.startsWith(route))) {
    const token = request.cookies.get('access_token')?.value;
    console.log('[Middleware] Path:', pathname);
    console.log('[Middleware] Token:', token);
    if (!token) {
      console.log('[Middleware] Không có token, chuyển hướng về /login');
      return NextResponse.redirect(new URL('/login', request.url));
    }
    const payload = decodeJWT(token);
    console.log('[Middleware] Payload:', payload);
    if (!payload || !payload.exp || payload.exp < Math.floor(Date.now() / 1000)) {
      console.log('[Middleware] Token hết hạn hoặc không hợp lệ, chuyển hướng về /login');
      return NextResponse.redirect(new URL('/login', request.url));
    }
    console.log('[Middleware] Token hợp lệ, cho phép truy cập');
  }
  return NextResponse.next();
}

export const config = {
  matcher: ['/account', '/(dashboard)/:path*'],
};
