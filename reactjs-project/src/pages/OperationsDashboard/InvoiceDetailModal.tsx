import React from 'react';
import { useTranslation } from 'react-i18next';
import { FiX, FiFileText, FiUser, FiShoppingBag, FiDollarSign } from 'react-icons/fi';
import { type Invoice } from '../../services/invoiceAPI';
import { invoiceAPI } from '../../services/invoiceAPI';

interface InvoiceDetailModalProps {
  invoice: Invoice;
  onClose: () => void;
}

const InvoiceDetailModal: React.FC<InvoiceDetailModalProps> = ({ invoice, onClose }) => {
  const { t } = useTranslation();

  // Handle click outside modal
  const handleBackdropClick = (e: React.MouseEvent) => {
    if (e.target === e.currentTarget) {
      onClose();
    }
  };

  return (
    <div
      className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4"
      onClick={handleBackdropClick}
    >
      <div className="bg-white rounded-xl shadow-2xl max-w-4xl w-full max-h-[90vh] overflow-hidden">
        {/* Header */}
        <div className="flex items-center justify-between p-6 border-b border-gray-200">
          <div>
            <h2 className="text-2xl font-bold text-gray-800">
              {t('dashboard.operations.invoices.detail.title')}
            </h2>
            <p className="text-gray-600 mt-1">
              {invoice.invoiceNumber}
            </p>
          </div>
          <button
            onClick={onClose}
            className="text-gray-400 hover:text-gray-600 transition-colors"
          >
            <FiX className="w-6 h-6" />
          </button>
        </div>

        {/* Content */}
        <div className="p-6 overflow-y-auto max-h-[calc(90vh-120px)]">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            
            {/* Invoice Information */}
            <div className="bg-gray-50 rounded-lg p-4">
              <div className="flex items-center space-x-2 mb-4">
                <FiFileText className="text-blue-500" />
                <h3 className="text-lg font-semibold text-gray-800">
                  {t('dashboard.operations.invoices.detail.invoiceInfo')}
                </h3>
              </div>
              
              <div className="space-y-3">
                <div className="flex justify-between">
                  <span className="text-gray-600">{t('dashboard.operations.invoices.table.headers.invoiceNumber')}:</span>
                  <span className="font-medium">{invoice.invoiceNumber}</span>
                </div>
                
                <div className="flex justify-between">
                  <span className="text-gray-600">{t('dashboard.operations.invoices.table.headers.status')}:</span>
                  <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${invoiceAPI.getStatusColor(invoice.invoiceStatus)}`}>
                    {t(`dashboard.operations.invoices.status.${invoice.invoiceStatus.toLowerCase()}`)}
                  </span>
                </div>
                
                <div className="flex justify-between">
                  <span className="text-gray-600">{t('dashboard.operations.invoices.table.headers.issuedDate')}:</span>
                  <span className="font-medium">{invoiceAPI.formatDate(invoice.issuedAt)}</span>
                </div>
                
                {invoice.sentAt && (
                  <div className="flex justify-between">
                    <span className="text-gray-600">Ngày gửi:</span>
                    <span className="font-medium">{invoiceAPI.formatDate(invoice.sentAt)}</span>
                  </div>
                )}
                
                {invoice.deliveredAt && (
                  <div className="flex justify-between">
                    <span className="text-gray-600">Ngày giao:</span>
                    <span className="font-medium">{invoiceAPI.formatDate(invoice.deliveredAt)}</span>
                  </div>
                )}
                
                {invoice.createdBy && (
                  <div className="flex justify-between">
                    <span className="text-gray-600">Người tạo:</span>
                    <span className="font-medium">{invoice.createdBy.username}</span>
                  </div>
                )}
              </div>
            </div>

            {/* Customer Information */}
            <div className="bg-gray-50 rounded-lg p-4">
              <div className="flex items-center space-x-2 mb-4">
                <FiUser className="text-green-500" />
                <h3 className="text-lg font-semibold text-gray-800">
                  {t('dashboard.operations.invoices.table.headers.customer')}
                </h3>
              </div>
              
              <div className="space-y-3">
                <div className="flex justify-between">
                  <span className="text-gray-600">Tên khách hàng:</span>
                  <span className="font-medium">{invoice.customerName || '-'}</span>
                </div>
                
                <div className="flex justify-between">
                  <span className="text-gray-600">Email:</span>
                  <span className="font-medium">{invoice.customerEmail || '-'}</span>
                </div>
              </div>
            </div>

            {/* Order Information */}
            {invoice.order && (
              <div className="bg-gray-50 rounded-lg p-4">
                <div className="flex items-center space-x-2 mb-4">
                  <FiShoppingBag className="text-purple-500" />
                  <h3 className="text-lg font-semibold text-gray-800">
                    {t('dashboard.operations.invoices.detail.orderInfo')}
                  </h3>
                </div>
                
                <div className="space-y-3">
                  <div className="flex justify-between">
                    <span className="text-gray-600">{t('dashboard.operations.invoices.table.headers.orderId')}:</span>
                    <span className="font-medium">#{invoice.order.id}</span>
                  </div>
                  
                  <div className="flex justify-between">
                    <span className="text-gray-600">Mô tả:</span>
                    <span className="font-medium">{invoice.order.description || '-'}</span>
                  </div>
                  
                  <div className="flex justify-between">
                    <span className="text-gray-600">Ngày tạo:</span>
                    <span className="font-medium">{invoiceAPI.formatDate(invoice.order.createdAt)}</span>
                  </div>
                  
                  {invoice.order.status && (
                    <div className="flex justify-between">
                      <span className="text-gray-600">Trạng thái đơn hàng:</span>
                      <span className="font-medium">{invoice.order.status.name}</span>
                    </div>
                  )}
                  
                  {invoice.order.pickupAddress && (
                    <div className="flex justify-between">
                      <span className="text-gray-600">Địa chỉ lấy hàng:</span>
                      <span className="font-medium text-right max-w-xs">{invoice.order.pickupAddress}</span>
                    </div>
                  )}
                  
                  {invoice.order.deliveryAddress && (
                    <div className="flex justify-between">
                      <span className="text-gray-600">Địa chỉ giao hàng:</span>
                      <span className="font-medium text-right max-w-xs">{invoice.order.deliveryAddress}</span>
                    </div>
                  )}
                </div>
              </div>
            )}

            {/* Store Information */}
            {invoice.order?.store && (
              <div className="bg-gray-50 rounded-lg p-4">
                <div className="flex items-center space-x-2 mb-4">
                  <FiShoppingBag className="text-orange-500" />
                  <h3 className="text-lg font-semibold text-gray-800">
                    {t('dashboard.operations.invoices.detail.storeInfo')}
                  </h3>
                </div>
                
                <div className="space-y-3">
                  <div className="flex justify-between">
                    <span className="text-gray-600">Tên cửa hàng:</span>
                    <span className="font-medium">{invoice.order.store.name}</span>
                  </div>
                  
                  <div className="flex justify-between">
                    <span className="text-gray-600">Email:</span>
                    <span className="font-medium">{invoice.order.store.email}</span>
                  </div>
                  
                  {invoice.order.store.phone && (
                    <div className="flex justify-between">
                      <span className="text-gray-600">Số điện thoại:</span>
                      <span className="font-medium">{invoice.order.store.phone}</span>
                    </div>
                  )}
                  
                  {invoice.order.store.address && (
                    <div className="flex justify-between">
                      <span className="text-gray-600">Địa chỉ:</span>
                      <span className="font-medium text-right max-w-xs">{invoice.order.store.address}</span>
                    </div>
                  )}
                  
                  <div className="flex justify-between">
                    <span className="text-gray-600">Ngày tạo:</span>
                    <span className="font-medium">{invoiceAPI.formatDate(invoice.order.store.createdAt)}</span>
                  </div>
                </div>
              </div>
            )}

            {/* Financial Summary */}
            <div className="bg-gradient-to-r from-blue-50 to-indigo-50 rounded-lg p-4 lg:col-span-2">
              <div className="flex items-center space-x-2 mb-4">
                <FiDollarSign className="text-blue-500" />
                <h3 className="text-lg font-semibold text-gray-800">
                  {t('dashboard.operations.invoices.detail.summary')}
                </h3>
              </div>
              
              <div className="grid grid-cols-1 sm:grid-cols-3 gap-4">
                <div className="text-center">
                  <div className="text-2xl font-bold text-gray-800">
                    {invoiceAPI.formatCurrency(invoice.totalAmount)}
                  </div>
                  <div className="text-sm text-gray-600">Tổng tiền (bao gồm VAT)</div>
                </div>
                
                <div className="text-center">
                  <div className="text-2xl font-bold text-gray-800">
                    {invoiceAPI.formatCurrency(invoice.taxAmount)}
                  </div>
                  <div className="text-sm text-gray-600">Thuế VAT</div>
                </div>
                
                <div className="text-center">
                  <div className="text-2xl font-bold text-gray-800">
                    {invoiceAPI.formatCurrency(invoice.netAmount)}
                  </div>
                  <div className="text-sm text-gray-600">Tiền trước thuế</div>
                </div>
              </div>
            </div>
          </div>
        </div>

        {/* Footer */}
        <div className="flex justify-end space-x-3 p-6 border-t border-gray-200 bg-gray-50">
          <button
            onClick={onClose}
            className="px-4 py-2 text-gray-700 bg-white border border-gray-300 rounded-lg hover:bg-gray-50 transition-colors"
          >
            {t('common.close')}
          </button>
        </div>
      </div>
    </div>
  );
};

export default InvoiceDetailModal;


