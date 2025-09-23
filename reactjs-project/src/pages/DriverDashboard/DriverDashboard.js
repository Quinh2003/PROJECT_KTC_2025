import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
export default function DriverDashboard({ user, onLogout }) {
    return (_jsxs("div", { children: [_jsx("h2", { children: "Driver Dashboard" }), _jsxs("p", { children: ["Xin ch\u00E0o ", user.name, " (", user.email, ")"] }), _jsx("button", { onClick: onLogout, children: "\u0110\u0103ng xu\u1EA5t" })] }));
}
