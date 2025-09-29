import { useTranslation } from 'react-i18next';
import LanguageSwitcher from '../components/LanguageSwitcher';
import { formatDate, formatDateTime, formatTime, formatRelativeTime, formatCurrency, formatNumber } from '../utils/dateFormat';

export default function I18nDemo() {
  const { t, i18n } = useTranslation();
  const currentLocale = i18n.language;
  const now = new Date();
  const sampleDate = new Date('2025-01-15T14:30:00');

  return (
    <div className="min-h-screen bg-gray-50 p-8">
      <div className="max-w-4xl mx-auto">
        <div className="bg-white rounded-lg shadow-lg p-8">
          {/* Header */}
          <div className="flex justify-between items-center mb-8">
            <h1 className="text-3xl font-bold text-gray-900">
              {t('common.language')} Demo
            </h1>
            <LanguageSwitcher />
          </div>

          {/* Current Language Info */}
          <div className="mb-8 p-4 bg-blue-50 rounded-lg">
            <h2 className="text-xl font-semibold mb-2">
              Current Language: {i18n.language}
            </h2>
            <p className="text-gray-700">
              {t('common.welcome')} to the i18n demonstration!
            </p>
          </div>

          {/* Common Translations */}
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-8">
            <div className="p-4 bg-green-50 rounded-lg">
              <h3 className="text-lg font-semibold mb-3 text-green-800">
                {t('common.language')} Actions
              </h3>
              <div className="space-y-2">
                <button className="w-full px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700">
                  {t('common.add')}
                </button>
                <button className="w-full px-4 py-2 bg-green-600 text-white rounded hover:bg-green-700">
                  {t('common.save')}
                </button>
                <button className="w-full px-4 py-2 bg-red-600 text-white rounded hover:bg-red-700">
                  {t('common.delete')}
                </button>
                <button className="w-full px-4 py-2 bg-gray-600 text-white rounded hover:bg-gray-700">
                  {t('common.cancel')}
                </button>
              </div>
            </div>

            <div className="p-4 bg-yellow-50 rounded-lg">
              <h3 className="text-lg font-semibold mb-3 text-yellow-800">
                Navigation
              </h3>
              <div className="space-y-2">
                <div className="px-3 py-2 bg-white rounded border">
                  {t('navigation.dashboard')}
                </div>
                <div className="px-3 py-2 bg-white rounded border">
                  {t('navigation.orders')}
                </div>
                <div className="px-3 py-2 bg-white rounded border">
                  {t('navigation.profile')}
                </div>
                <div className="px-3 py-2 bg-white rounded border">
                  {t('navigation.logout')}
                </div>
              </div>
            </div>
          </div>

          {/* Auth Demo */}
          <div className="p-4 bg-purple-50 rounded-lg">
            <h3 className="text-lg font-semibold mb-3 text-purple-800">
              {t('auth.login.title')}
            </h3>
            <p className="text-gray-700 mb-3">
              {t('auth.login.subtitle')}
            </p>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-1">
                  {t('auth.login.email')}
                </label>
                <input 
                  type="email" 
                  className="w-full px-3 py-2 border border-gray-300 rounded-md"
                  placeholder={t('auth.login.email')}
                />
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-1">
                  {t('auth.login.password')}
                </label>
                <input 
                  type="password" 
                  className="w-full px-3 py-2 border border-gray-300 rounded-md"
                  placeholder={t('auth.login.password')}
                />
              </div>
            </div>
            <button 
              className="mt-4 px-6 py-2 bg-purple-600 text-white rounded hover:bg-purple-700"
              onClick={() => {
                alert(t('notifications.loginSuccess'));
              }}
            >
              {t('auth.login.loginButton')}
            </button>
            
            <div className="mt-4 space-y-2">
              <h4 className="font-semibold">Validation Messages:</h4>
              <div className="text-sm text-red-600">
                <p>• {t('validation.required')}</p>
                <p>• {t('validation.invalidEmail')}</p>
                <p>• {t('validation.passwordTooShort')}</p>
              </div>
              
              <h4 className="font-semibold mt-3">Notifications:</h4>
              <div className="text-sm text-green-600">
                <p>• {t('notifications.loginSuccess')}</p>
                <p>• {t('notifications.saveSuccess')}</p>
                <p>• {t('notifications.updateSuccess')}</p>
              </div>
            </div>
          </div>

          {/* Date & Number Formatting */}
          <div className="mt-8 p-6 bg-blue-50 rounded-lg">
            <h3 className="text-xl font-semibold mb-4 text-blue-800">
              Date & Number Formatting
            </h3>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              <div>
                <h4 className="font-semibold mb-3">Date Formats:</h4>
                <div className="space-y-2 text-sm">
                  <div>
                    <span className="font-medium">Date:</span>
                    <span className="ml-2">{formatDate(sampleDate, currentLocale)}</span>
                  </div>
                  <div>
                    <span className="font-medium">Date & Time:</span>
                    <span className="ml-2">{formatDateTime(sampleDate, currentLocale)}</span>
                  </div>
                  <div>
                    <span className="font-medium">Time:</span>
                    <span className="ml-2">{formatTime(sampleDate, currentLocale)}</span>
                  </div>
                  <div>
                    <span className="font-medium">Relative:</span>
                    <span className="ml-2">{formatRelativeTime(new Date(now.getTime() - 2 * 60 * 60 * 1000), currentLocale)}</span>
                  </div>
                </div>
              </div>
              <div>
                <h4 className="font-semibold mb-3">Number Formats:</h4>
                <div className="space-y-2 text-sm">
                  <div>
                    <span className="font-medium">Number:</span>
                    <span className="ml-2">{formatNumber(1234567.89, currentLocale)}</span>
                  </div>
                  <div>
                    <span className="font-medium">Currency:</span>
                    <span className="ml-2">{formatCurrency(1234567.89, currentLocale)}</span>
                  </div>
                  <div>
                    <span className="font-medium">Large Number:</span>
                    <span className="ml-2">{formatNumber(9876543210, currentLocale)}</span>
                  </div>
                </div>
              </div>
            </div>
          </div>

          {/* Status */}
          <div className="mt-8 p-4 bg-gray-50 rounded-lg">
            <h3 className="text-lg font-semibold mb-3">
              Status Examples
            </h3>
            <div className="flex flex-wrap gap-2">
              <span className="px-3 py-1 bg-yellow-200 text-yellow-800 rounded-full text-sm">
                {t('common.pending')}
              </span>
              <span className="px-3 py-1 bg-green-200 text-green-800 rounded-full text-sm">
                {t('common.completed')}
              </span>
              <span className="px-3 py-1 bg-red-200 text-red-800 rounded-full text-sm">
                {t('common.cancelled')}
              </span>
              <span className="px-3 py-1 bg-blue-200 text-blue-800 rounded-full text-sm">
                {t('common.active')}
              </span>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
