import { format } from 'date-fns';
import { vi, enUS } from 'date-fns/locale';

export function formatDate(date: Date | string | number, locale: string = 'en', formatStr: string = 'PP'): string {
  const dateObj = typeof date === 'string' || typeof date === 'number' ? new Date(date) : date;
  
  return format(dateObj, formatStr, {
    locale: locale === 'vi' ? vi : enUS
  });
}

export function formatDateTime(date: Date | string | number, locale: string = 'en'): string {
  return formatDate(date, locale, 'PPpp');
}

export function formatTime(date: Date | string | number, locale: string = 'en'): string {
  return formatDate(date, locale, 'p');
}

export function formatRelativeTime(date: Date | string | number, locale: string = 'en'): string {
  const dateObj = typeof date === 'string' || typeof date === 'number' ? new Date(date) : date;
  const now = new Date();
  const diffInMs = now.getTime() - dateObj.getTime();
  const diffInMinutes = Math.floor(diffInMs / (1000 * 60));
  const diffInHours = Math.floor(diffInMinutes / 60);
  const diffInDays = Math.floor(diffInHours / 24);

  if (locale === 'vi') {
    if (diffInMinutes < 1) return 'Vừa xong';
    if (diffInMinutes < 60) return `${diffInMinutes} phút trước`;
    if (diffInHours < 24) return `${diffInHours} giờ trước`;
    if (diffInDays < 7) return `${diffInDays} ngày trước`;
    return formatDate(date, locale, 'dd/MM/yyyy');
  } else {
    if (diffInMinutes < 1) return 'Just now';
    if (diffInMinutes < 60) return `${diffInMinutes} minutes ago`;
    if (diffInHours < 24) return `${diffInHours} hours ago`;
    if (diffInDays < 7) return `${diffInDays} days ago`;
    return formatDate(date, locale, 'MM/dd/yyyy');
  }
}

export function formatCurrency(amount: number, locale: string = 'en'): string {
  if (locale === 'vi') {
    return new Intl.NumberFormat('vi-VN', {
      style: 'currency',
      currency: 'VND'
    }).format(amount);
  } else {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD'
    }).format(amount);
  }
}

export function formatNumber(number: number, locale: string = 'en'): string {
  if (locale === 'vi') {
    return new Intl.NumberFormat('vi-VN').format(number);
  } else {
    return new Intl.NumberFormat('en-US').format(number);
  }
}

