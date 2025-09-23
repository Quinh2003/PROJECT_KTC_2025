import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useState, useEffect } from "react";
const roles = [
    { label: "Admin", value: "Admin", icon: null },
    { label: "Dispatcher", value: "Dispatcher", icon: null },
    { label: "Fleet Manager", value: "Fleet Manager", icon: null },
    { label: "Driver", value: "Driver", icon: null },
    { label: "Operations Manager", value: "Operations Manager", icon: null },
    { label: "Customer", value: "Customer", icon: null },
];
export default function UserForm({ onAdd, onClose, user }) {
    const [name, setName] = useState(user?.name || "");
    const [email, setEmail] = useState(user?.email || "");
    const [emailError, setEmailError] = useState("");
    const [phoneError, setPhoneError] = useState("");
    const [role, setRole] = useState(user?.role || roles[0].value);
    const [status, setStatus] = useState(user?.status === "inactive"
        ? "inactive"
        : user?.status === "suspended"
            ? "suspended"
            : "active");
    const [password, setPassword] = useState(user?.password || "");
    const [phone, setPhone] = useState(user?.phone || "");
    // Format số điện thoại
    const formatPhoneNumber = (value) => {
        // Chỉ giữ lại số
        const phoneNumber = value.replace(/[^\d]/g, '');
        // Format theo pattern: 0123 456 789
        if (phoneNumber.length <= 4) {
            return phoneNumber;
        }
        else if (phoneNumber.length <= 7) {
            return `${phoneNumber.slice(0, 4)} ${phoneNumber.slice(4)}`;
        }
        else {
            return `${phoneNumber.slice(0, 4)} ${phoneNumber.slice(4, 7)} ${phoneNumber.slice(7, 10)}`;
        }
    };
    useEffect(() => {
        setName(user?.name || "");
        setEmail(user?.email || "");
        setRole(user?.role || roles[0].value);
        // Fix status mapping
        if (user?.status === "inactive") {
            setStatus("inactive");
        }
        else if (user?.status === "suspended") {
            setStatus("suspended");
        }
        else {
            setStatus("active");
        }
        setPassword(user?.password || "");
        // Format số điện thoại khi load
        if (user?.phone) {
            setPhone(formatPhoneNumber(user.phone));
        }
        else {
            setPhone("");
        }
        setEmailError("");
        setPhoneError("");
    }, [user]);
    // Kiểm tra email
    const handleEmailChange = (e) => {
        const value = e.target.value;
        setEmail(value);
        if (value && !value.includes('@')) {
            setEmailError(`Vui lòng bao gồm '@' trong địa chỉ email. '${value}' bị thiếu '@'.`);
        }
        else {
            setEmailError("");
        }
    };
    // Xử lý số điện thoại
    const handlePhoneChange = (e) => {
        const rawValue = e.target.value;
        // Chỉ giữ lại số từ input
        const phoneDigits = rawValue.replace(/[^\d]/g, '');
        // Giới hạn ở 10 chữ số
        const limitedDigits = phoneDigits.slice(0, 10);
        // Format theo pattern: 0123 456 789
        let formattedValue = '';
        if (limitedDigits.length <= 4) {
            formattedValue = limitedDigits;
        }
        else if (limitedDigits.length <= 7) {
            formattedValue = `${limitedDigits.slice(0, 4)} ${limitedDigits.slice(4)}`;
        }
        else {
            formattedValue = `${limitedDigits.slice(0, 4)} ${limitedDigits.slice(4, 7)} ${limitedDigits.slice(7)}`;
        }
        // Luôn cập nhật state phone để có thể nhập từng chữ số
        setPhone(formattedValue);
        // Hiển thị lỗi nếu không đủ 10 chữ số
        if (formattedValue.trim() !== "" && limitedDigits.length !== 10) {
            setPhoneError("Số điện thoại phải đúng 10 chữ số!");
        }
        else {
            setPhoneError("");
        }
    };
    // Sửa validate trong handleSubmit
    const handleSubmit = (e) => {
        e.preventDefault();
        // Validate phone required
        const phoneDigits = phone.replace(/[^\d]/g, '');
        if (!phone.trim()) {
            setPhoneError("Số điện thoại không được để trống!");
            return;
        }
        else if (phoneDigits.length !== 10) {
            setPhoneError("Số điện thoại phải đúng 10 chữ số!");
            return;
        }
        // Validate email
        if (!email.includes('@')) {
            setEmailError(`Vui lòng bao gồm '@' trong địa chỉ email. '${email}' bị thiếu '@'.`);
            return;
        }
        // Kiểm tra lỗi trước khi submit
        if (emailError || phoneError) {
            return;
        }
        console.log("[UserForm] handleSubmit with role:", role);
        const selectedRole = roles.find(r => r.value === role) || roles[0];
        // Lưu số điện thoại chỉ là số, bỏ định dạng khoảng trắng
        const cleanPhone = phone.replace(/\s/g, '');
        const userToSubmit = {
            id: user?.id || undefined,
            name,
            email,
            role,
            roleIcon: selectedRole.icon,
            status,
            lastLogin: user?.lastLogin || "-",
            password,
            phone: cleanPhone,
        };
        console.log("[UserForm] Submitting user:", userToSubmit);
        onAdd(userToSubmit);
        onClose();
    };
    return (_jsx("div", { className: "fixed inset-0 bg-black/30 flex items-center justify-center z-50", children: _jsxs("form", { className: "bg-white rounded-xl shadow-lg p-8 w-full max-w-md space-y-4", onSubmit: handleSubmit, children: [_jsx("h2", { className: "text-xl font-bold mb-2", children: user ? "Edit User" : "Add New User" }), _jsxs("div", { children: [_jsx("label", { className: "block mb-1 font-semibold", children: "Full Name" }), _jsx("input", { className: "border rounded px-3 py-2 w-full", value: name, onChange: e => setName(e.target.value), required: true })] }), _jsxs("div", { children: [_jsx("label", { className: "block mb-1 font-semibold", children: "Phone" }), _jsx("input", { className: `border rounded px-3 py-2 w-full ${phoneError ? 'border-red-500' : ''}`, type: "text", value: phone, onChange: handlePhoneChange, placeholder: "0123 456 789", maxLength: 12, required: true }), phoneError && (_jsxs("div", { className: "bg-orange-100 border border-orange-400 text-orange-700 px-3 py-2 rounded mt-2 text-sm flex items-center", children: [_jsx("span", { className: "text-orange-500 mr-2", children: "\u26A0" }), phoneError] }))] }), _jsxs("div", { children: [_jsx("label", { className: "block mb-1 font-semibold", children: "Email" }), _jsx("input", { className: `border rounded px-3 py-2 w-full ${emailError ? 'border-red-500' : ''}`, type: "text", value: email, onChange: handleEmailChange, required: true, disabled: !!user }), emailError && (_jsxs("div", { className: "bg-orange-100 border border-orange-400 text-orange-700 px-3 py-2 rounded mt-2 text-sm flex items-center", children: [_jsx("span", { className: "text-orange-500 mr-2", children: "\u26A0" }), emailError] }))] }), _jsxs("div", { children: [_jsx("label", { className: "block mb-1 font-semibold", children: "Password" }), _jsx("div", { className: "relative", children: _jsx("input", { className: "border rounded px-3 py-2 w-full pr-10", type: "text", value: password, onChange: e => setPassword(e.target.value), required: true }) })] }), _jsxs("div", { children: [_jsx("label", { className: "block mb-1 font-semibold", children: "Role" }), _jsx("select", { className: "border rounded px-3 py-2 w-full", value: role, onChange: e => setRole(e.target.value), children: roles.map(r => (_jsx("option", { value: r.value, children: r.label }, r.value))) })] }), _jsxs("div", { children: [_jsx("label", { className: "block mb-1 font-semibold", children: "Status" }), _jsxs("select", { className: "border rounded px-3 py-2 w-full", value: status, onChange: e => setStatus(e.target.value), children: [_jsx("option", { value: "active", children: "Active" }), _jsx("option", { value: "inactive", children: "Inactive" }), _jsx("option", { value: "suspended", children: "Suspended" })] })] }), _jsxs("div", { className: "flex gap-2 justify-end pt-2", children: [_jsx("button", { type: "button", className: "px-4 py-2 rounded bg-gray-200 hover:bg-gray-300", onClick: onClose, children: "Cancel" }), _jsx("button", { type: "submit", className: "px-4 py-2 rounded bg-teal-600 text-white font-bold hover:bg-blue-700", children: user ? "Save" : "Add" })] })] }) }));
}
