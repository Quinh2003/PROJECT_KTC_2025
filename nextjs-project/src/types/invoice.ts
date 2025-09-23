/**
 * Invoice-related TypeScript interfaces
 * Based on backend DTOs and API responses
 */

export interface CreateInvoiceRequest {
	orderId: number
	customerEmail?: string
	customerName?: string
	notes?: string
}

export interface CancelInvoiceRequest {
	cancellationReason: string
}

export interface InvoiceEligibilityResponse {
	orderId: number
	eligible: boolean
	message: string
}

export type InvoiceStatus = 'CREATED' | 'SENT' | 'DELIVERED' | 'CANCELLED' | 'EXPIRED'

export interface InvoiceResponse {
	id: number
	invoiceNumber: string
	invoiceStatus: InvoiceStatus
	invoiceStatusDisplay: string
	orderId: number
	deliveryId?: number
	totalAmount: number
	totalAmountFormatted: string
	taxAmount: number
	taxAmountFormatted: string
	netAmount: number
	netAmountFormatted: string
	customerEmail?: string
	customerName?: string
	issuedAt: string
	issuedAtFormatted: string
	pdfFilePath?: string
	pdfFileName?: string
	emailSentAt?: string
	emailSentAtFormatted?: string
	notes?: string
	createdByName: string
	createdByEmail?: string
	createdAt: string
	createdAtFormatted: string
	updatedAt?: string
	cancelledAt?: string
	cancelledAtFormatted?: string
	cancelledByName?: string
	cancellationReason?: string
	orderDescription?: string
	orderStatusName?: string
	storeInformation?: string
}

export interface ApiResponse<T> {
	success: boolean
	message: string
	data: T
}

export interface InvoiceFilters {
	status?: string
	orderId?: number
	customerEmail?: string
}

export interface OrderNeedingInvoice {
	id: number
	description: string
	totalAmount: number
	createdAt: string
	status?: string
}

/**
 * UI-specific interfaces
 */
export interface InvoiceButtonProps {
	orderId: number
	orderStatus?: string
	onInvoiceCreated?: (invoice: InvoiceResponse) => void
	size?: 'small' | 'middle' | 'large'
	type?: 'default' | 'primary' | 'text'
	disabled?: boolean
}

export interface InvoiceModalState {
	isCreateModalVisible: boolean
	isEmailModalVisible: boolean
	isActionsModalVisible: boolean
}

export interface InvoiceFormValues {
	customerEmail?: string
	customerName?: string
	notes?: string
}

export interface EmailFormValues {
	email: string
}

/**
 * Status color mapping for UI
 */
export const INVOICE_STATUS_COLORS: Record<InvoiceStatus, string> = {
	CREATED: 'blue',
	SENT: 'orange',
	DELIVERED: 'green',
	CANCELLED: 'red',
	EXPIRED: 'gray',
}

/**
 * Status display names in Vietnamese
 */
export const INVOICE_STATUS_DISPLAY: Record<InvoiceStatus, string> = {
	CREATED: 'Đã tạo',
	SENT: 'Đã gửi',
	DELIVERED: 'Đã giao',
	CANCELLED: 'Đã hủy',
	EXPIRED: 'Đã hết hạn',
}

/**
 * Business logic constants
 */
export const INVOICE_BUSINESS_RULES = {
	MAX_CUSTOMER_NAME_LENGTH: 255,
	MAX_EMAIL_LENGTH: 255,
	MAX_NOTES_LENGTH: 1000,
	CREATION_DEADLINE_DAYS: 365,
	TAX_RATE: 0.1, // 10% VAT
} as const

/**
 * API endpoints constants
 */
export const INVOICE_ENDPOINTS = {
	CHECK_ELIGIBILITY: (orderId: number) => `/invoices/check-eligibility/${orderId}`,
	CREATE_INVOICE: '/invoices',
	GET_ALL_INVOICES: '/invoices',
	GET_INVOICE_BY_ID: (id: number) => `/invoices/${id}`,
	GET_INVOICE_BY_ORDER: (orderId: number) => `/invoices/by-order/${orderId}`,
	SEND_EMAIL: (id: number) => `/invoices/${id}/send-email`,
	CANCEL_INVOICE: (id: number) => `/invoices/${id}/cancel`,
	GENERATE_PDF: (id: number) => `/invoices/${id}/generate-pdf`,
	DOWNLOAD_PDF: (id: number) => `/invoices/${id}/download-pdf`,
	ORDERS_NEEDING_INVOICE: '/invoices/orders-needing-invoice',
} as const
