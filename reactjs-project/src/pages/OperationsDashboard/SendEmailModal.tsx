import React, { useState, useEffect } from 'react';
import { useTranslation } from 'react-i18next';
import { FiX, FiMail, FiAlertCircle, FiLoader } from 'react-icons/fi';
import { type Invoice } from '../../services/invoiceAPI';
import { invoiceAPI } from '../../services/invoiceAPI';

interface SendEmailModalProps {
  invoice: Invoice;
  onClose: () => void;
  onEmailSent: () => void;
  setSendingId: (id: number | null) => void;
}

const SendEmailModal: React.FC<SendEmailModalProps> = ({ 
  invoice, 
  onClose, 
  onEmailSent,
  setSendingId 
}) => {
  const { t } = useTranslation();
  
  const [email, setEmail] = useState('');
  const [isValidEmail, setIsValidEmail] = useState(false);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState('');

  // Initialize email from customer email hoặc store email
  useEffect(() => {
    if (invoice.customerEmail) {
      setEmail(invoice.customerEmail);
    } else if (invoice.order?.store?.email) {
      setEmail(invoice.order.store.email);
    }
  }, [invoice]);

  // Validate email
  useEffect(() => {
    const emailRegex = /^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$/;
    setIsValidEmail(emailRegex.test(email.trim()));
    setError(''); // Clear error when email changes
  }, [email]);

  // Handle send email
  const handleSendEmail = async () => {
    if (!isValidEmail) {
      setError(t('validation.invalidEmail'));
      return;
    }

    try {
      setIsLoading(true);
      setSendingId(invoice.id);
      setError('');

      await invoiceAPI.sendInvoiceByEmail(invoice.id, email.trim());
      
      // Success - close modal and refresh parent
      onEmailSent();
      
      // TODO: Show success notification
      console.log('Email sent successfully');
      
    } catch (error) {
      console.error('Error sending email:', error);
      setError(error instanceof Error ? error.message : 'Unknown error occurred');
    } finally {
      setIsLoading(false);
      setSendingId(null);
    }
  };

  // Handle click outside modal
  const handleBackdropClick = (e: React.MouseEvent) => {
    if (e.target === e.currentTarget && !isLoading) {
      onClose();
    }
  };

  // Handle key press
  const handleKeyPress = (e: React.KeyboardEvent) => {
    if (e.key === 'Enter' && isValidEmail && !isLoading) {
      handleSendEmail();
    }
  };

  return (
    <div
      className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4"
      onClick={handleBackdropClick}
    >
      <div className="bg-white rounded-xl shadow-2xl max-w-md w-full">
        {/* Header */}
        <div className="flex items-center justify-between p-6 border-b border-gray-200">
          <div className="flex items-center space-x-2">
            <FiMail className="text-purple-500 w-5 h-5" />
            <h2 className="text-xl font-bold text-gray-800">
              {t('dashboard.operations.invoices.modal.sendEmail.title')}
            </h2>
          </div>
          {!isLoading && (
            <button
              onClick={onClose}
              className="text-gray-400 hover:text-gray-600 transition-colors"
            >
              <FiX className="w-5 h-5" />
            </button>
          )}
        </div>

        {/* Content */}
        <div className="p-6">
          <p className="text-gray-600 mb-4">
            {t('dashboard.operations.invoices.modal.sendEmail.subtitle')}
          </p>

          {/* Invoice Info */}
          <div className="bg-gray-50 rounded-lg p-4 mb-4">
            <div className="text-sm text-gray-600">Hóa đơn:</div>
            <div className="font-medium text-gray-800">{invoice.invoiceNumber}</div>
            <div className="text-sm text-gray-600 mt-1">
              Đơn hàng #{invoice.orderId} - {invoiceAPI.formatCurrency(invoice.totalAmount)}
            </div>
          </div>

          {/* Email Input */}
          <div className="mb-4">
            <label htmlFor="email" className="block text-sm font-medium text-gray-700 mb-2">
              {t('dashboard.operations.invoices.modal.sendEmail.emailLabel')}
            </label>
            <input
              id="email"
              type="email"
              value={email}
              onChange={(e) => setEmail(e.target.value)}
              onKeyPress={handleKeyPress}
              placeholder={t('dashboard.operations.invoices.modal.sendEmail.emailPlaceholder')}
              disabled={isLoading}
              className={`w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-purple-500 focus:border-purple-500 disabled:bg-gray-100 disabled:cursor-not-allowed ${
                email && !isValidEmail ? 'border-red-500' : 'border-gray-300'
              }`}
            />
            {email && !isValidEmail && (
              <p className="text-red-500 text-sm mt-1">
                {t('validation.invalidEmail')}
              </p>
            )}
          </div>

          {/* Message */}
          <div className="bg-blue-50 border border-blue-200 rounded-lg p-3 mb-4">
            <p className="text-blue-800 text-sm">
              {t('dashboard.operations.invoices.modal.sendEmail.message')}
            </p>
          </div>

          {/* Error Message */}
          {error && (
            <div className="bg-red-50 border border-red-200 rounded-lg p-3 mb-4">
              <div className="flex items-center space-x-2">
                <FiAlertCircle className="text-red-500 w-4 h-4" />
                <p className="text-red-800 text-sm">{error}</p>
              </div>
            </div>
          )}
        </div>

        {/* Footer */}
        <div className="flex justify-end space-x-3 p-6 border-t border-gray-200 bg-gray-50">
          <button
            onClick={onClose}
            disabled={isLoading}
            className="px-4 py-2 text-gray-700 bg-white border border-gray-300 rounded-lg hover:bg-gray-50 transition-colors disabled:bg-gray-100 disabled:cursor-not-allowed"
          >
            {t('dashboard.operations.invoices.modal.sendEmail.cancelButton')}
          </button>
          <button
            onClick={handleSendEmail}
            disabled={!isValidEmail || isLoading}
            className="px-4 py-2 bg-purple-600 text-white rounded-lg hover:bg-purple-700 transition-colors disabled:bg-gray-400 disabled:cursor-not-allowed flex items-center space-x-2"
          >
            {isLoading ? (
              <>
                <FiLoader className="w-4 h-4 animate-spin" />
                <span>{t('dashboard.operations.invoices.actions.sending')}</span>
              </>
            ) : (
              <>
                <FiMail className="w-4 h-4" />
                <span>{t('dashboard.operations.invoices.modal.sendEmail.confirmButton')}</span>
              </>
            )}
          </button>
        </div>
      </div>
    </div>
  );
};

export default SendEmailModal;


