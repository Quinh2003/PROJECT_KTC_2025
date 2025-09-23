'use client'

import { useState } from 'react'
import {
	Button,
	Tooltip,
	Modal,
	Form,
	Input,
	Space,
	message,
	Typography,
	Divider,
	Tag,
} from 'antd'
import {
	FilePdfOutlined,
	MailOutlined,
	DownloadOutlined,
} from '@ant-design/icons'
import { invoiceService } from '@/services/invoiceService'
import type { InvoiceResponse, InvoiceButtonProps } from '@/types/invoice'

const { Text } = Typography
const { TextArea } = Input

// InvoiceButtonProps is now imported from types

export default function InvoiceButton({
	orderId,
	orderStatus,
	onInvoiceCreated,
	size = 'middle',
	type = 'text',
	disabled = false,
}: InvoiceButtonProps) {
	const [loading, setLoading] = useState(false)
	const [invoice, setInvoice] = useState<InvoiceResponse | null>(null)
	const [isModalVisible, setIsModalVisible] = useState(false)
	const [isEmailModalVisible, setIsEmailModalVisible] = useState(false)
	const [lastClickTime, setLastClickTime] = useState<number>(0)
	const [form] = Form.useForm()
	const [emailForm] = Form.useForm()
	const [messageApi, contextHolder] = message.useMessage()


	// Handle button click - Auto create and download invoice
	const handleClick = async () => {
		// Prevent rapid clicks (debounce for 2 seconds)
		const currentTime = Date.now()
		if (currentTime - lastClickTime < 2000) {
			messageApi.warning('Vui lòng đợi một chút trước khi thử lại')
			return
		}
		setLastClickTime(currentTime)

		setLoading(true)
		try {
			// Always check for existing invoice first
			messageApi.loading('Đang kiểm tra hóa đơn...', 0)
			
			const existingInvoice = await invoiceService.getInvoiceByOrderId(orderId)
			if (existingInvoice) {
				// Invoice exists, download directly
				setInvoice(existingInvoice)
				messageApi.destroy()
				messageApi.success(`Tìm thấy hóa đơn ${existingInvoice.invoiceNumber}!`)
				
				// Download immediately
				try {
					await invoiceService.downloadInvoicePdf(existingInvoice.id, existingInvoice.invoiceNumber)
					messageApi.success('Đang tải xuống hóa đơn PDF...')
				} catch (error: unknown) {
					console.error('Error downloading PDF:', error)
					messageApi.error('Không thể tải xuống hóa đơn PDF')
				}
				return
			}

			// No existing invoice, check eligibility and create new one
			messageApi.loading('Đang kiểm tra điều kiện...', 0)
			const eligibility = await invoiceService.checkEligibility(orderId)
			
			if (!eligibility.eligible) {
				messageApi.destroy()
				messageApi.warning('Đơn hàng chưa đủ điều kiện để xuất hóa đơn')
				return
			}

			// Create new invoice
			messageApi.loading('Đang tạo hóa đơn...', 0)
			const newInvoice = await invoiceService.createInvoice({
				orderId,
				customerEmail: undefined,
				customerName: undefined,
				notes: `Hóa đơn tự động cho đơn hàng #${orderId}`,
			})

			setInvoice(newInvoice)
			messageApi.destroy()
			messageApi.success(`Hóa đơn ${newInvoice.invoiceNumber} đã được tạo thành công!`)

			// Notify parent component
			onInvoiceCreated?.(newInvoice)

			// Download immediately after creation
			try {
				await invoiceService.downloadInvoicePdf(newInvoice.id, newInvoice.invoiceNumber)
				messageApi.success('Đang tải xuống hóa đơn PDF...')
			} catch (error: unknown) {
				console.error('Error downloading PDF:', error)
				messageApi.error('Không thể tải xuống hóa đơn PDF')
			}

		} catch (error: unknown) {
			messageApi.destroy()
			console.error('Error in handleClick:', error)
			const errorMessage = (error as { response?: { data?: { message?: string } } })
				.response?.data?.message || 'Không thể xử lý yêu cầu'
			
			// Handle specific case where invoice was created by another process
			if (errorMessage.includes('đã có hóa đơn') || errorMessage.includes('already has')) {
				try {
					const existingInvoice = await invoiceService.getInvoiceByOrderId(orderId)
					if (existingInvoice) {
						setInvoice(existingInvoice)
						messageApi.success(`Đã tìm thấy hóa đơn ${existingInvoice.invoiceNumber}!`)
						
						// Download the existing invoice
						try {
							await invoiceService.downloadInvoicePdf(existingInvoice.id, existingInvoice.invoiceNumber)
							messageApi.success('Đang tải xuống hóa đơn PDF...')
						} catch (downloadError: unknown) {
							console.error('Error downloading PDF:', downloadError)
							messageApi.error('Không thể tải xuống hóa đơn PDF')
						}
						return
					}
				} catch (fetchError: unknown) {
					console.error('Error fetching existing invoice:', fetchError)
				}
			}
			
			messageApi.error(`Lỗi hệ thống: ${errorMessage}`)
		} finally {
			setLoading(false)
		}
	}


	// Show invoice actions modal
	const showInvoiceActions = () => {
		Modal.info({
			title: 'Hóa đơn điện tử',
			width: 600,
			content: (
				<div style={{ padding: '16px 0' }}>
					<Space direction="vertical" style={{ width: '100%' }} size="middle">
						<div>
							<Text strong>Số hóa đơn: </Text>
							<Text code>{invoice?.invoiceNumber}</Text>
						</div>
						<div>
							<Text strong>Trạng thái: </Text>
							<Tag color={invoiceService.getStatusColor(invoice!.invoiceStatus)}>
								{invoice?.invoiceStatusDisplay}
							</Tag>
						</div>
						<div>
							<Text strong>Tổng tiền: </Text>
							<Text>{invoice?.totalAmountFormatted}</Text>
						</div>
						<div>
							<Text strong>Ngày xuất: </Text>
							<Text>{invoice?.issuedAtFormatted}</Text>
						</div>
						
						<Divider />
						
						<Space wrap>
							<Button
								type="primary"
								icon={<DownloadOutlined />}
								onClick={() => handleDownloadPdf()}
								loading={loading}
							>
								Tải PDF
							</Button>
							<Button
								icon={<MailOutlined />}
								onClick={() => showEmailModal()}
								disabled={!invoiceService.canSendEmail(invoice!)}
							>
								Gửi Email
							</Button>
						</Space>
					</Space>
				</div>
			),
		})
	}

	// Handle create invoice
	const handleCreateInvoice = async (values: {
		customerEmail?: string
		customerName?: string
		notes?: string
	}) => {
		setLoading(true)
		try {
			const newInvoice = await invoiceService.createInvoice({
				orderId,
				customerEmail: values.customerEmail?.trim(),
				customerName: values.customerName?.trim(),
				notes: values.notes?.trim(),
			})

			setInvoice(newInvoice)
			setIsModalVisible(false)
			form.resetFields()
			messageApi.success('Tạo hóa đơn điện tử thành công!')
			
			if (onInvoiceCreated) {
				onInvoiceCreated(newInvoice)
			}

			// Auto show actions modal
			setTimeout(() => showInvoiceActions(), 1000)
		} catch (error: unknown) {
			console.error('Error creating invoice:', error)
			const errorMessage = (error as { response?: { data?: { message?: string } } })
				.response?.data?.message || 'Không thể tạo hóa đơn điện tử'
			messageApi.error(errorMessage)
		} finally {
			setLoading(false)
		}
	}

	// Handle download PDF
	const handleDownloadPdf = async () => {
		if (!invoice) return

		setLoading(true)
		try {
			await invoiceService.downloadInvoicePdf(invoice.id, invoice.invoiceNumber)
			messageApi.success('Đang tải xuống hóa đơn PDF...')
		} catch (error: unknown) {
			console.error('Error downloading PDF:', error)
			messageApi.error('Không thể tải xuống hóa đơn PDF')
		} finally {
			setLoading(false)
		}
	}

	// Show email modal
	const showEmailModal = () => {
		setIsEmailModalVisible(true)
		// Pre-fill with customer email if available
		if (invoice?.customerEmail) {
			emailForm.setFieldsValue({ email: invoice.customerEmail })
		}
	}

	// Handle send email
	const handleSendEmail = async (values: { email: string }) => {
		if (!invoice) return

		setLoading(true)
		try {
			await invoiceService.sendInvoiceByEmail(invoice.id, values.email.trim())
			setIsEmailModalVisible(false)
			emailForm.resetFields()
			messageApi.success('Đã gửi hóa đơn qua email thành công!')
		} catch (error: unknown) {
			console.error('Error sending email:', error)
			const errorMessage = (error as { response?: { data?: { message?: string } } })
				.response?.data?.message || 'Không thể gửi hóa đơn qua email'
			messageApi.error(errorMessage)
		} finally {
			setLoading(false)
		}
	}

	// Don't show button if order is not in appropriate status
	const shouldShowButton = () => {
		if (disabled) return false
		// Show button for completed/delivered/shipped orders or if invoice already exists
		return invoice || 
			   orderStatus === 'DELIVERED' || 
			   orderStatus === 'COMPLETED' || 
			   orderStatus === 'Completed' ||  // Database có trạng thái 'Completed' 
			   orderStatus === 'SHIPPED' ||
			   orderStatus === 'Shipped' ||
			   orderStatus === 'PROCESSED' ||
			   orderStatus === 'Processed'
	}

	if (!shouldShowButton()) {
		return null
	}

	return (
		<>
			{contextHolder}
			<Tooltip title="Hóa đơn điện tử">
				<Button
					type={type}
					size={size}
					icon={<FilePdfOutlined />}
					onClick={handleClick}
					disabled={disabled}
					loading={loading}
				/>
			</Tooltip>

			{/* Create Invoice Modal */}
			<Modal
				title={
					<Space>
						<FilePdfOutlined />
						<span>Tạo hóa đơn điện tử</span>
					</Space>
				}
				open={isModalVisible}
				onCancel={() => {
					setIsModalVisible(false)
					form.resetFields()
				}}
				footer={null}
				width={600}
			>
				<Form
					form={form}
					layout="vertical"
					onFinish={handleCreateInvoice}
					style={{ marginTop: 16 }}
				>
					<Form.Item
						label="Mã đơn hàng"
						style={{ marginBottom: 16 }}
					>
						<Input value={orderId} disabled />
					</Form.Item>

					<Form.Item
						name="customerEmail"
						label="Email khách hàng"
						rules={[
							{
								type: 'email',
								message: 'Vui lòng nhập email hợp lệ!',
							},
						]}
					>
						<Input placeholder="customer@example.com" />
					</Form.Item>

					<Form.Item
						name="customerName"
						label="Tên khách hàng"
						rules={[
							{
								max: 255,
								message: 'Tên khách hàng không được vượt quá 255 ký tự!',
							},
						]}
					>
						<Input placeholder="Nguyễn Văn A" />
					</Form.Item>

					<Form.Item
						name="notes"
						label="Ghi chú"
						rules={[
							{
								max: 1000,
								message: 'Ghi chú không được vượt quá 1000 ký tự!',
							},
						]}
					>
						<TextArea
							rows={3}
							placeholder="Ghi chú đặc biệt cho hóa đơn..."
						/>
					</Form.Item>

					<div style={{ textAlign: 'right', marginTop: 24 }}>
						<Space>
							<Button onClick={() => setIsModalVisible(false)}>
								Hủy
							</Button>
							<Button type="primary" htmlType="submit" loading={loading}>
								Tạo hóa đơn
							</Button>
						</Space>
					</div>
				</Form>
			</Modal>

			{/* Send Email Modal */}
			<Modal
				title={
					<Space>
						<MailOutlined />
						<span>Gửi hóa đơn qua email</span>
					</Space>
				}
				open={isEmailModalVisible}
				onCancel={() => {
					setIsEmailModalVisible(false)
					emailForm.resetFields()
				}}
				footer={null}
				width={500}
			>
				<Form
					form={emailForm}
					layout="vertical"
					onFinish={handleSendEmail}
					style={{ marginTop: 16 }}
				>
					<Form.Item
						name="email"
						label="Địa chỉ email"
						rules={[
							{
								required: true,
								message: 'Vui lòng nhập địa chỉ email!',
							},
							{
								type: 'email',
								message: 'Vui lòng nhập email hợp lệ!',
							},
						]}
					>
						<Input placeholder="customer@example.com" />
					</Form.Item>

					<div style={{ textAlign: 'right', marginTop: 24 }}>
						<Space>
							<Button onClick={() => setIsEmailModalVisible(false)}>
								Hủy
							</Button>
							<Button type="primary" htmlType="submit" loading={loading}>
								Gửi email
							</Button>
						</Space>
					</div>
				</Form>
			</Modal>
		</>
	)
}
