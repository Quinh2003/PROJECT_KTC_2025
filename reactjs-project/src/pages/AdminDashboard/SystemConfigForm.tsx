import { useState, useEffect } from "react";
import { FiSun, FiMoon } from "react-icons/fi";

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
        <div className="bg-white dark:bg-gray-900 rounded-2xl shadow border border-gray-100 dark:border-gray-700 p-8">
          <h2 className="text-2xl font-bold mb-6 dark:text-white">General Settings</h2>
          <div className="mb-5">
            <label className="block font-semibold mb-1 dark:text-white">System Name</label>
            <input
              className="w-full border rounded-lg px-4 py-3 bg-gray-50 dark:bg-gray-800 dark:text-white focus:outline-none focus:ring-2 focus:ring-teal-200"
              value={config.systemName}
              onChange={e => handleChange("systemName", e.target.value)}
            />
          </div>
          <div className="mb-5">
            <label className="block font-semibold mb-1 dark:text-white">Timezone</label>
            <select
              className="w-full border rounded-lg px-4 py-3 bg-gray-50 dark:bg-gray-800 dark:text-white"
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
            <label className="block font-semibold mb-1 dark:text-white">Default Language</label>
            <select
              className="w-full border rounded-lg px-4 py-3 bg-gray-50 dark:bg-gray-800 dark:text-white"
              value={config.language}
              onChange={e => handleChange("language", e.target.value)}
            >
              <option value="English">English</option>
              <option value="Vietnamese">Vietnamese</option>
              <option value="Japanese">Japanese</option>
            </select>
          </div>
          <div className="mb-5">
            <label className="block font-semibold mb-1 dark:text-white">Theme</label>
            <div className="flex gap-6 mt-2">
              <label className={`flex items-center gap-2 cursor-pointer px-3 py-2 rounded-lg border ${config.theme === "light" ? "border-teal-500 bg-teal-50" : "border-gray-200 dark:border-gray-700"}`}>
                <input
                  type="radio"
                  name="theme"
                  value="light"
                  checked={config.theme === "light"}
                  onChange={() => handleChange("theme", "light")}
                  className="accent-teal-500"
                />
                <FiSun className="text-yellow-400" />
                Light
              </label>
              <label className={`flex items-center gap-2 cursor-pointer px-3 py-2 rounded-lg border ${config.theme === "dark" ? "border-teal-500 bg-teal-50 dark:bg-teal-900" : "border-gray-200 dark:border-gray-700"}`}>
                <input
                  type="radio"
                  name="theme"
                  value="dark"
                  checked={config.theme === "dark"}
                  onChange={() => handleChange("theme", "dark")}
                  className="accent-teal-500"
                />
                <FiMoon className="text-gray-700 dark:text-yellow-200" />
                Dark
              </label>
            </div>
          </div>
        </div>
        {/* API Integration */}
        <div className="bg-white dark:bg-gray-900 rounded-2xl shadow border border-gray-100 dark:border-gray-700 p-8">
          <h2 className="text-2xl font-bold mb-6 dark:text-white">API Integration</h2>
          <div className="mb-5">
            <label className="block font-semibold mb-1 dark:text-white">Google Maps API Key</label>
            <input
              className="w-full border rounded-lg px-4 py-3 bg-gray-50 dark:bg-gray-800 dark:text-white focus:outline-none focus:ring-2 focus:ring-teal-200"
              placeholder="Enter API key"
              value={config.googleApiKey}
              onChange={e => handleChange("googleApiKey", e.target.value)}
            />
          </div>
          <div className="mb-5">
            <label className="block font-semibold mb-1 dark:text-white">SMS Gateway</label>
            <input
              className="w-full border rounded-lg px-4 py-3 bg-gray-50 dark:bg-gray-800 dark:text-white"
              placeholder="URL endpoint"
              value={config.smsGateway}
              onChange={e => handleChange("smsGateway", e.target.value)}
            />
          </div>
          <div className="mb-5">
            <label className="block font-semibold mb-1 dark:text-white">Email SMTP</label>
            <input
              className="w-full border rounded-lg px-4 py-3 bg-gray-50 dark:bg-gray-800 dark:text-white"
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