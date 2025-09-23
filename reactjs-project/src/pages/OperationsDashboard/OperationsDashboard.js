import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useState } from 'react';
import Sidebar, {} from '../../components/Sidebar';
import Navbar from '../../components/Navbar';
import ResourceMonitoring from './ResourceMonitoring';
import PerformanceAnalytics from './PerformanceAnalytics';
import StaffManagement from './StaffManagement';
import OperationsOverview from './OperationsOverview';
export default function OperationsDashboard({ user, onLogout }) {
    const [tab, setTab] = useState("overview");
    return (_jsxs("div", { className: "min-h-screen flex bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100", children: [_jsx(Sidebar, { activeTab: tab, onTabChange: tab => setTab(tab), role: "operations" }), _jsxs("div", { className: "flex-1 flex flex-col", children: [_jsx(Navbar, { user: user, onLogout: onLogout, title: "Operations Manager Dashboard", subtitle: "" }), _jsxs("main", { className: "flex-1 p-6", children: [tab === "overview" && _jsx(OperationsOverview, {}), tab === "monitoring" && _jsx(ResourceMonitoring, {}), tab === "performance" && _jsx(PerformanceAnalytics, {}), tab === "staff" && _jsx(StaffManagement, {})] })] })] }));
}
