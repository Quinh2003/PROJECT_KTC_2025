import axios from 'axios'
import type {
	CreateInvoiceRequest,
	CancelInvoiceRequest,
	InvoiceEligibilityResponse,
	InvoiceResponse,
	ApiResponse,
	InvoiceFilters,
	InvoiceStatus,
	INVOICE_STATUS_COLORS,
	OrderNeedingInvoice,
} from '@/types/invoice'

// Create axios instance with base configuration
const api = axios.create({
	baseURL: 'http://localhost:8080/api',
	timeout: 30000, // 30 seconds for PDF generation
})

// Add request interceptor to include JWT token
api.interceptors.request.use(
	(config) => {
		const token = localStorage.getItem('token')
		if (token) {
			config.headers.Authorization = `Bearer ${token}`
		}
		return config
	},
	(error) => {
		return Promise.reject(error)
	}
)

// Add response interceptor for error handling
api.interceptors.response.use(
	(response) => response,
	(error) => {
		if (error.response?.status === 401) {
			// Handle unauthorized - redirect to login
			localStorage.removeItem('token')
			localStorage.removeItem('user')
			window.location.href = '/login'
		}
		return Promise.reject(error)
	}
)

export const invoiceService = {
	/**
	 * Check if an order is eligible for invoice creation
	 */
	checkEligibility: async (orderId: number): Promise<InvoiceEligibilityResponse> => {
		const { data } = await api.get<ApiResponse<InvoiceEligibilityResponse>>(
			`/invoices/check-eligibility/${orderId}`
		)
		return data.data
	},

	/**
	 * Create a new electronic invoice
	 */
	createInvoice: async (request: CreateInvoiceRequest): Promise<InvoiceResponse> => {
		const { data } = await api.post<ApiResponse<InvoiceResponse>>('/invoices', request)
		return data.data
	},

	/**
	 * Get all invoices with optional filters
	 */
	getAllInvoices: async (params?: InvoiceFilters): Promise<InvoiceResponse[]> => {
		const { data } = await api.get<ApiResponse<InvoiceResponse[]>>('/invoices', {
			params,
		})
		return data.data
	},

	/**
	 * Get invoice by ID
	 */
	getInvoiceById: async (invoiceId: number): Promise<InvoiceResponse> => {
		const { data } = await api.get<ApiResponse<InvoiceResponse>>(`/invoices/${invoiceId}`)
		return data.data
	},

	/**
	 * Get invoice by order ID
	 */
	getInvoiceByOrderId: async (orderId: number): Promise<InvoiceResponse | null> => {
		try {
			const { data } = await api.get<ApiResponse<InvoiceResponse>>(
				`/invoices/by-order/${orderId}`
			)
			return data.data
		} catch (error: unknown) {
			const axiosError = error as { response?: { status?: number } }
			if (axiosError.response?.status === 404) {
				return null // No invoice exists for this order
			}
			throw error
		}
	},

	/**
	 * Send invoice via email
	 */
	sendInvoiceByEmail: async (invoiceId: number, emailAddress: string): Promise<string> => {
		const { data } = await api.post<ApiResponse<string>>(
			`/invoices/${invoiceId}/send-email?emailAddress=${encodeURIComponent(emailAddress)}`
		)
		return data.data
	},

	/**
	 * Cancel an invoice (Admin only)
	 */
	cancelInvoice: async (
		invoiceId: number,
		request: CancelInvoiceRequest
	): Promise<string> => {
		const { data } = await api.post<ApiResponse<string>>(
			`/invoices/${invoiceId}/cancel`,
			request
		)
		return data.data
	},

	/**
	 * Generate PDF for an invoice
	 */
	generatePdf: async (invoiceId: number): Promise<string> => {
		const { data } = await api.post<ApiResponse<string>>(
			`/invoices/${invoiceId}/generate-pdf`
		)
		return data.data
	},

	/**
	 * Download invoice PDF
	 */
	downloadPdf: async (invoiceId: number): Promise<Blob> => {
		const response = await api.get(`/invoices/${invoiceId}/download-pdf`, {
			responseType: 'blob',
		})
		return response.data
	},

	/**
	 * Get orders needing invoice
	 */
	getOrdersNeedingInvoice: async (): Promise<OrderNeedingInvoice[]> => {
		const { data } = await api.get<ApiResponse<OrderNeedingInvoice[]>>('/invoices/orders-needing-invoice')
		return data.data
	},

	/**
	 * Download invoice PDF with proper filename handling
	 */
	downloadInvoicePdf: async (invoiceId: number, invoiceNumber?: string): Promise<void> => {
		try {
			const blob = await invoiceService.downloadPdf(invoiceId)
			
			// Create download link
			const url = window.URL.createObjectURL(blob)
			const link = document.createElement('a')
			link.href = url
			
			// Set filename
			const filename = invoiceNumber 
				? `HoaDon_${invoiceNumber}.pdf`
				: `HoaDon_${invoiceId}.pdf`
			link.download = filename
			
			// Trigger download
			document.body.appendChild(link)
			link.click()
			
			// Cleanup
			document.body.removeChild(link)
			window.URL.revokeObjectURL(url)
		} catch (error) {
			console.error('Error downloading PDF:', error)
			throw error
		}
	},

	/**
	 * Utility function to format currency
	 */
	formatCurrency: (amount: number): string => {
		return amount.toLocaleString('vi-VN', {
			style: 'currency',
			currency: 'VND',
		})
	},

	/**
	 * Utility function to format date
	 */
	formatDate: (dateString: string): string => {
		return new Date(dateString).toLocaleString('vi-VN', {
			year: 'numeric',
			month: '2-digit',
			day: '2-digit',
			hour: '2-digit',
			minute: '2-digit',
		})
	},

	/**
	 * Check if invoice can be cancelled
	 */
	canCancelInvoice: (invoice: InvoiceResponse): boolean => {
		return invoice.invoiceStatus === 'CREATED' || invoice.invoiceStatus === 'EXPIRED'
	},

	/**
	 * Check if invoice email can be sent
	 */
	canSendEmail: (invoice: InvoiceResponse): boolean => {
		return invoice.invoiceStatus !== 'CANCELLED'
	},

	/**
	 * Get invoice status color for UI
	 */
	getStatusColor: (status: InvoiceStatus): string => {
		return INVOICE_STATUS_COLORS[status] || 'default'
	},
}

export default invoiceService

// Re-export types for convenience
export type {
	CreateInvoiceRequest,
	CancelInvoiceRequest,
	InvoiceEligibilityResponse,
	InvoiceResponse,
	ApiResponse,
	InvoiceFilters,
	InvoiceStatus,
}
