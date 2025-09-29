import { useState } from "react";
import { useTranslation } from 'react-i18next';

export default function SystemConfigForm() {
  const { t } = useTranslation();
  const [config, setConfig] = useState({
    systemName: "KTC Logistics 2025",
    timezone: "Asia/Ho_Chi_Minh",
    language: "English",
    googleApiKey: "",
    smsGateway: "",
    smtp: "smtp.gmail.com",
  });

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const handleChange = (key: keyof typeof config, value: any) => {
    setConfig(prev => ({ ...prev, [key]: value }));
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    // Handle saving config here, e.g.:
    // alert("Settings saved!");
  };

  return (
    <form className="w-full" onSubmit={handleSubmit}>
      <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
        {/* General Settings */}
        <div className="bg-white rounded-2xl shadow border border-gray-100 p-8">
          <h2 className="text-2xl font-bold mb-6">General Settings</h2>
          <div className="mb-5">
            <label className="block font-semibold mb-1">System Name</label>
            <input
              className="w-full border rounded-lg px-4 py-3 bg-gray-50 focus:outline-none focus:ring-2 focus:ring-teal-200"
              value={config.systemName}
              onChange={e => handleChange("systemName", e.target.value)}
            />
          </div>
          <div className="mb-5">
            <label className="block font-semibold mb-1">Timezone</label>
            <select
              className="w-full border rounded-lg px-4 py-3 bg-gray-50"
              value={config.timezone}
              onChange={e => handleChange("timezone", e.target.value)}
            >
              <option value="Asia/Ho_Chi_Minh">Asia/Ho_Chi_Minh</option>
              <option value="Asia/Bangkok">Asia/Bangkok</option>
              <option value="Asia/Tokyo">Asia/Tokyo</option>
              <option value="UTC">UTC</option>
            </select>
          </div>
          <div className="mb-5">
            <label className="block font-semibold mb-1">Default Language</label>
            <select
              className="w-full border rounded-lg px-4 py-3 bg-gray-50"
              value={config.language}
              onChange={e => handleChange("language", e.target.value)}
            >
              <option value="English">English</option>
              <option value="Vietnamese">Vietnamese</option>
              <option value="Japanese">Japanese</option>
            </select>
          </div>
        </div>
        {/* API Integration */}
        <div className="bg-white rounded-2xl shadow border border-gray-100 p-8">
          <h2 className="text-2xl font-bold mb-6">API Integration</h2>
          <div className="mb-5">
            <label className="block font-semibold mb-1">Google Maps API Key</label>
            <input
              className="w-full border rounded-lg px-4 py-3 bg-gray-50 focus:outline-none focus:ring-2 focus:ring-teal-200"
              placeholder={t('admin.settings.apiKeyPlaceholder', 'Enter API key')}
              value={config.googleApiKey}
              onChange={e => handleChange("googleApiKey", e.target.value)}
            />
          </div>
          <div className="mb-5">
            <label className="block font-semibold mb-1">SMS Gateway</label>
            <input
              className="w-full border rounded-lg px-4 py-3 bg-gray-50"
              placeholder={t('admin.settings.urlPlaceholder', 'URL endpoint')}
              value={config.smsGateway}
              onChange={e => handleChange("smsGateway", e.target.value)}
            />
          </div>
          <div className="mb-5">
            <label className="block font-semibold mb-1">Email SMTP</label>
            <input
              className="w-full border rounded-lg px-4 py-3 bg-gray-50"
              placeholder="smtp.gmail.com"
              value={config.smtp}
              onChange={e => handleChange("smtp", e.target.value)}
            />
          </div>
        </div>
      </div>
      <div className="flex justify-end mt-8">
        <button
          type="submit"
          className="px-6 py-3 rounded-lg bg-teal-600 text-white font-bold hover:bg-teal-700 transition"
        >
          Save Settings
        </button>
      </div>
    </form>
  );
}