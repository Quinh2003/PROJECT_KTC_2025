import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useState } from "react";
export default function SystemConfigForm() {
    const [config, setConfig] = useState({
        systemName: "KTC Logistics 2025",
        timezone: "Asia/Ho_Chi_Minh",
        language: "English",
        googleApiKey: "",
        smsGateway: "",
        smtp: "smtp.gmail.com",
    });
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const handleChange = (key, value) => {
        setConfig(prev => ({ ...prev, [key]: value }));
    };
    const handleSubmit = (e) => {
        e.preventDefault();
        // Handle saving config here, e.g.:
        // alert("Settings saved!");
    };
    return (_jsxs("form", { className: "w-full", onSubmit: handleSubmit, children: [_jsxs("div", { className: "grid grid-cols-1 md:grid-cols-2 gap-8", children: [_jsxs("div", { className: "bg-white rounded-2xl shadow border border-gray-100 p-8", children: [_jsx("h2", { className: "text-2xl font-bold mb-6", children: "General Settings" }), _jsxs("div", { className: "mb-5", children: [_jsx("label", { className: "block font-semibold mb-1", children: "System Name" }), _jsx("input", { className: "w-full border rounded-lg px-4 py-3 bg-gray-50 focus:outline-none focus:ring-2 focus:ring-teal-200", value: config.systemName, onChange: e => handleChange("systemName", e.target.value) })] }), _jsxs("div", { className: "mb-5", children: [_jsx("label", { className: "block font-semibold mb-1", children: "Timezone" }), _jsxs("select", { className: "w-full border rounded-lg px-4 py-3 bg-gray-50", value: config.timezone, onChange: e => handleChange("timezone", e.target.value), children: [_jsx("option", { value: "Asia/Ho_Chi_Minh", children: "Asia/Ho_Chi_Minh" }), _jsx("option", { value: "Asia/Bangkok", children: "Asia/Bangkok" }), _jsx("option", { value: "Asia/Tokyo", children: "Asia/Tokyo" }), _jsx("option", { value: "UTC", children: "UTC" })] })] }), _jsxs("div", { className: "mb-5", children: [_jsx("label", { className: "block font-semibold mb-1", children: "Default Language" }), _jsxs("select", { className: "w-full border rounded-lg px-4 py-3 bg-gray-50", value: config.language, onChange: e => handleChange("language", e.target.value), children: [_jsx("option", { value: "English", children: "English" }), _jsx("option", { value: "Vietnamese", children: "Vietnamese" }), _jsx("option", { value: "Japanese", children: "Japanese" })] })] })] }), _jsxs("div", { className: "bg-white rounded-2xl shadow border border-gray-100 p-8", children: [_jsx("h2", { className: "text-2xl font-bold mb-6", children: "API Integration" }), _jsxs("div", { className: "mb-5", children: [_jsx("label", { className: "block font-semibold mb-1", children: "Google Maps API Key" }), _jsx("input", { className: "w-full border rounded-lg px-4 py-3 bg-gray-50 focus:outline-none focus:ring-2 focus:ring-teal-200", placeholder: "Enter API key", value: config.googleApiKey, onChange: e => handleChange("googleApiKey", e.target.value) })] }), _jsxs("div", { className: "mb-5", children: [_jsx("label", { className: "block font-semibold mb-1", children: "SMS Gateway" }), _jsx("input", { className: "w-full border rounded-lg px-4 py-3 bg-gray-50", placeholder: "URL endpoint", value: config.smsGateway, onChange: e => handleChange("smsGateway", e.target.value) })] }), _jsxs("div", { className: "mb-5", children: [_jsx("label", { className: "block font-semibold mb-1", children: "Email SMTP" }), _jsx("input", { className: "w-full border rounded-lg px-4 py-3 bg-gray-50", placeholder: "smtp.gmail.com", value: config.smtp, onChange: e => handleChange("smtp", e.target.value) })] })] })] }), _jsx("div", { className: "flex justify-end mt-8", children: _jsx("button", { type: "submit", className: "px-6 py-3 rounded-lg bg-teal-600 text-white font-bold hover:bg-teal-700 transition", children: "Save Settings" }) })] }));
}
