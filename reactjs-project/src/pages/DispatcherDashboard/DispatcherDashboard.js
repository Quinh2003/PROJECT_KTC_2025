import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useState } from "react";
import OrderOverview from "./OrderOverview";
import OrderList from "./OrderList";
import ResourceOverview from "./ResourceOverview";
import OrderAssignment from "./OrderAssignment";
import Sidebar, {} from "../../components/Sidebar";
import Navbar from "../../components/Navbar";
import MapboxTrackingMap from "./MapboxTrackingMap";
import MapErrorBoundary from '../../components/MapErrorBoundary';
import VehicleList from "./VehicleList";
import DriverList from "./DriverList";
import { DispatcherProvider } from "../../contexts/DispatcherContext";
export default function DispatcherDashboard({ user, onLogout, }) {
    // Thêm "assignment" vào state tab
    const [tab, setTab] = useState("orders");
    return (_jsx(DispatcherProvider, { children: _jsxs("div", { className: "min-h-screen flex bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100", children: [_jsx(Sidebar, { activeTab: tab, onTabChange: tab => setTab(tab), role: "dispatcher" }), _jsxs("div", { className: " flex-1 flex flex-col", children: [_jsx(Navbar, { user: user, onLogout: onLogout, title: "Dispatcher Dashboard", subtitle: "" }), _jsxs("main", { className: "flex-1 p-6", children: [_jsxs("div", { className: tab === "orders" ? "block" : "hidden", children: [_jsx(OrderOverview, {}), _jsxs("div", { className: "mt-6 grid grid-cols-1 xl:grid-cols-2 gap-6", children: [_jsx(OrderList, {}), _jsx(MapErrorBoundary, { children: _jsx(MapboxTrackingMap, {}) })] })] }), _jsx("div", { className: tab === "resources" ? "block" : "hidden", children: _jsxs("div", { className: "grid grid-cols-1 xl:grid-cols-2 gap-6", children: [_jsx(VehicleList, {}), _jsx(DriverList, {})] }) }), _jsx("div", { className: tab === "assignment" ? "block" : "hidden", children: _jsx(OrderAssignment, {}) })] })] })] }) }));
}
