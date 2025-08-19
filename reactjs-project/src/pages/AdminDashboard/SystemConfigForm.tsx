import { useState, useEffect } from "react";

export default function SystemConfigForm() {
  const [config, setConfig] = useState({
    systemName: "KTC Logistics 2025",
    timezone: "Asia/Ho_Chi_Minh",
    language: "English",
    googleApiKey: "",
    smsGateway: "",
    smtp: "smtp.gmail.com",
    theme: "light",
  });
  const [showTzDropdown, setShowTzDropdown] = useState(false);
  const [showLangDropdown, setShowLangDropdown] = useState(false);

  // Toggle light/dark mode for the system
  useEffect(() => {
    if (config.theme === "dark") {
      document.documentElement.classList.add("dark");
    } else {
      document.documentElement.classList.remove("dark");
    }
  }, [config.theme]);

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
        <div className="bg-white border border-red-100 shadow-lg rounded-2xl p-8">
          <h2 className="text-2xl font-bold mb-6 text-gray-800 drop-shadow-sm">General Settings</h2>
          <div className="mb-5">
            <label className="block font-semibold mb-1 text-gray-600">System Name</label>
            <input
              className="w-full border border-red-200 rounded-lg px-4 py-3 bg-gray-50 text-gray-700 placeholder:text-gray-400 focus:outline-none focus:ring-2 focus:ring-red-300 focus:border-red-500"
              value={config.systemName}
              onChange={e => handleChange("systemName", e.target.value)}
            />
          </div>
          <div className="mb-5 relative">
            <label className="block font-semibold mb-1 text-gray-600">Timezone</label>
            <button
              type="button"
              className="w-full border border-red-200 rounded-lg px-4 py-3 bg-gray-50 text-gray-700 flex justify-between items-center focus:outline-none hover:bg-red-50"
              onClick={() => setShowTzDropdown((v: boolean) => !v)}
            >
              {config.timezone}
              <svg className="ml-2 w-4 h-4 text-red-500" fill="none" stroke="currentColor" strokeWidth="2" viewBox="0 0 24 24"><path strokeLinecap="round" strokeLinejoin="round" d="M19 9l-7 7-7-7" /></svg>
            </button>
            {showTzDropdown && (
              <div className="absolute left-0 mt-2 w-full rounded-lg shadow-lg z-10 bg-white border border-red-200">
                {["Asia/Ho_Chi_Minh", "Asia/Bangkok", "Asia/Tokyo", "UTC"].map(tz => (
                  <button
                    key={tz}
                    type="button"
                    className={`w-full text-left px-4 py-3 text-gray-700 hover:bg-red-50 ${config.timezone === tz ? 'bg-red-50' : ''}`}
                    onClick={() => { handleChange("timezone", tz); setShowTzDropdown(false); }}
                  >
                    {tz}
                  </button>
                ))}
              </div>
            )}
          </div>
          <div className="mb-5 relative">
            <label className="block font-semibold mb-1 text-gray-600">Default Language</label>
            <button
              type="button"
              className="w-full border border-red-200 rounded-lg px-4 py-3 bg-gray-50 text-gray-700 flex justify-between items-center focus:outline-none hover:bg-red-50"
              onClick={() => setShowLangDropdown((v: boolean) => !v)}
            >
              {config.language}
              <svg className="ml-2 w-4 h-4 text-red-500" fill="none" stroke="currentColor" strokeWidth="2" viewBox="0 0 24 24"><path strokeLinecap="round" strokeLinejoin="round" d="M19 9l-7 7-7-7" /></svg>
            </button>
            {showLangDropdown && (
              <div className="absolute left-0 mt-2 w-full rounded-lg shadow-lg z-10 bg-white border border-red-200">
                {["English", "Vietnamese", "Japanese"].map(lang => (
                  <button
                    key={lang}
                    type="button"
                    className={`w-full text-left px-4 py-3 text-gray-700 hover:bg-red-50 ${config.language === lang ? 'bg-red-50' : ''}`}
                    onClick={() => { handleChange("language", lang); setShowLangDropdown(false); }}
                  >
                    {lang}
                  </button>
                ))}
              </div>
            )}
          </div>
        
        </div>
        {/* API Integration */}
        <div className="bg-white border border-red-100 shadow-lg rounded-2xl p-8">
          <h2 className="text-2xl font-bold mb-6 text-gray-800 drop-shadow-sm">API Integration</h2>
          <div className="mb-5">
            <label className="block font-semibold mb-1 text-gray-600">Google Maps API Key</label>
            <input
              className="w-full border border-red-200 rounded-lg px-4 py-3 bg-gray-50 text-gray-700 placeholder:text-gray-400 focus:outline-none focus:ring-2 focus:ring-red-300 focus:border-red-500"
              placeholder="Enter API key"
              value={config.googleApiKey}
              onChange={e => handleChange("googleApiKey", e.target.value)}
            />
          </div>
          <div className="mb-5">
            <label className="block font-semibold mb-1 text-gray-600">SMS Gateway</label>
            <input
              className="w-full border border-red-200 rounded-lg px-4 py-3 bg-gray-50 text-gray-700 placeholder:text-gray-400 focus:outline-none focus:ring-2 focus:ring-red-300 focus:border-red-500"
              placeholder="URL endpoint"
              value={config.smsGateway}
              onChange={e => handleChange("smsGateway", e.target.value)}
            />
          </div>
          <div className="mb-5">
            <label className="block font-semibold mb-1 text-gray-600">Email SMTP</label>
            <input
              className="w-full border border-red-200 rounded-lg px-4 py-3 bg-gray-50 text-gray-700 placeholder:text-gray-400 focus:outline-none focus:ring-2 focus:ring-red-300 focus:border-red-500"
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
          className="px-8 py-3 rounded-lg bg-gradient-to-br from-red-500 to-red-600 text-white font-bold hover:from-red-600 hover:to-red-700 shadow-lg transition-all duration-300 hover:shadow-xl"
        >
          Save Settings
        </button>
      </div>
    </form>
  );
}