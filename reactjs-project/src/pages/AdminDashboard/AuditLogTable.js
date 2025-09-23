import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useState, useEffect } from "react";
import { fetchActivityLogs } from "../../services/adminAPI";
export default function AuditLogTable({ onAuditCountUpdate }) {
    const [logs, setLogs] = useState([]);
    const [filteredLogs, setFilteredLogs] = useState([]);
    const [dateFilter, setDateFilter] = useState("today");
    const [showAllLogs, setShowAllLogs] = useState(false);
    const [customStartDate, setCustomStartDate] = useState("");
    const [customEndDate, setCustomEndDate] = useState("");
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState(null);
    const [lastRefresh, setLastRefresh] = useState(new Date());
    const [previousLogCount, setPreviousLogCount] = useState(0);
    const [newLogsCount, setNewLogsCount] = useState(0);
    // Fetch logs from API - only on mount
    useEffect(() => {
        const loadLogs = async () => {
            try {
                setLoading(true);
                setError(null);
                const data = await fetchActivityLogs();
                setLogs(data);
                setPreviousLogCount(data.length);
                setLastRefresh(new Date());
                // Update audit count in parent component
                if (onAuditCountUpdate) {
                    onAuditCountUpdate(data.length);
                }
                console.log(`Initial load: ${data.length} logs loaded`);
            }
            catch (err) {
                console.error("Failed to fetch activity logs:", err);
                setError("Failed to load activity logs");
                setLogs([]);
            }
            finally {
                setLoading(false);
            }
        };
        // Initial load only - no auto-refresh
        loadLogs();
    }, [onAuditCountUpdate]);
    const filterLogsByDate = (logs, filter, startDate, endDate) => {
        const now = new Date();
        const today = new Date(now.getFullYear(), now.getMonth(), now.getDate());
        return logs.filter(log => {
            const logDate = new Date(log.time);
            switch (filter) {
                case "today":
                    return logDate >= today;
                case "last7days": {
                    const sevenDaysAgo = new Date(today);
                    sevenDaysAgo.setDate(today.getDate() - 7);
                    return logDate >= sevenDaysAgo;
                }
                case "last30days": {
                    const thirtyDaysAgo = new Date(today);
                    thirtyDaysAgo.setDate(today.getDate() - 30);
                    return logDate >= thirtyDaysAgo;
                }
                case "custom": {
                    if (startDate && endDate) {
                        const start = new Date(startDate);
                        const end = new Date(endDate);
                        end.setHours(23, 59, 59, 999); // Include the entire end date
                        return logDate >= start && logDate <= end;
                    }
                    return true;
                }
                default:
                    return true;
            }
        });
    };
    useEffect(() => {
        const filtered = filterLogsByDate(logs, dateFilter, customStartDate, customEndDate);
        setFilteredLogs(filtered);
    }, [logs, dateFilter, customStartDate, customEndDate]);
    const displayedLogs = showAllLogs ? filteredLogs : filteredLogs.slice(0, 10);
    const handleDateFilterChange = (filter) => {
        setDateFilter(filter);
        setShowAllLogs(false);
    };
    const manualRefresh = async () => {
        setLoading(true);
        try {
            const data = await fetchActivityLogs();
            // Check for new logs
            if (previousLogCount > 0 && data.length > previousLogCount) {
                const newCount = data.length - previousLogCount;
                setNewLogsCount(newCount);
                console.log(`🆕 ${newCount} new logs detected!`);
                // Clear the "new logs" indicator after 5 seconds
                setTimeout(() => setNewLogsCount(0), 5000);
            }
            setLogs(data);
            setPreviousLogCount(data.length);
            setLastRefresh(new Date());
            setError(null);
            // Update audit count in parent component
            if (onAuditCountUpdate) {
                onAuditCountUpdate(data.length);
            }
            console.log("Manual refresh completed - audit count updated");
        }
        catch (err) {
            console.error("Manual refresh failed:", err);
            setError("Failed to refresh logs");
        }
        finally {
            setLoading(false);
        }
    };
    return (_jsxs("div", { className: "bg-white dark:bg-gray-900 rounded-2xl shadow border border-gray-100 dark:border-gray-700 p-8", children: [_jsxs("div", { className: "flex justify-between items-center mb-6", children: [_jsxs("div", { children: [_jsxs("h2", { className: "text-2xl font-bold mb-2 dark:text-white flex items-center gap-2", children: [_jsx("span", { role: "img", "aria-label": "log", children: "\uD83D\uDCDD" }), " System Logs", newLogsCount > 0 && (_jsxs("span", { className: "text-xs bg-orange-100 text-orange-800 px-2 py-1 rounded-full animate-pulse", children: ["\u2728 +", newLogsCount, " new"] }))] }), _jsxs("p", { className: "text-xs text-gray-400 dark:text-gray-500 mt-1", children: ["Last updated: ", lastRefresh.toLocaleTimeString()] }), error && (_jsxs("p", { className: "text-sm text-yellow-600 dark:text-yellow-400 mt-1", children: ["\u26A0\uFE0F ", error, " - Using sample data"] }))] }), _jsxs("div", { className: "flex items-center gap-4", children: [_jsx("div", { className: "flex gap-2", children: _jsx("button", { onClick: manualRefresh, disabled: loading, className: "px-3 py-2 rounded-lg text-xs font-medium bg-blue-500 text-white hover:bg-blue-600 disabled:opacity-50 disabled:cursor-not-allowed transition-colors", title: "Refresh logs", children: loading ? "⏳" : "🔄 Refresh" }) }), _jsxs("div", { className: "flex gap-2", children: [_jsx("button", { onClick: () => handleDateFilterChange("today"), className: `px-3 py-2 rounded-lg text-sm font-medium transition-colors ${dateFilter === "today"
                                            ? "bg-blue-500 text-white"
                                            : "bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 hover:bg-gray-200 dark:hover:bg-gray-600"}`, children: "Today" }), _jsx("button", { onClick: () => handleDateFilterChange("last7days"), className: `px-3 py-2 rounded-lg text-sm font-medium transition-colors ${dateFilter === "last7days"
                                            ? "bg-blue-500 text-white"
                                            : "bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 hover:bg-gray-200 dark:hover:bg-gray-600"}`, children: "Last 7 Days" }), _jsx("button", { onClick: () => handleDateFilterChange("last30days"), className: `px-3 py-2 rounded-lg text-sm font-medium transition-colors ${dateFilter === "last30days"
                                            ? "bg-blue-500 text-white"
                                            : "bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 hover:bg-gray-200 dark:hover:bg-gray-600"}`, children: "Last 30 Days" }), _jsx("button", { onClick: () => handleDateFilterChange("custom"), className: `px-3 py-2 rounded-lg text-sm font-medium transition-colors ${dateFilter === "custom"
                                            ? "bg-blue-500 text-white"
                                            : "bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 hover:bg-gray-200 dark:hover:bg-gray-600"}`, children: "Custom" })] })] })] }), dateFilter === "custom" && (_jsx("div", { className: "mb-6 p-4 bg-gray-50 dark:bg-gray-800 rounded-lg", children: _jsxs("div", { className: "flex gap-4 items-center", children: [_jsxs("div", { children: [_jsx("label", { className: "block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1", children: "Start Date" }), _jsx("input", { type: "date", value: customStartDate, onChange: (e) => setCustomStartDate(e.target.value), className: "border border-gray-300 dark:border-gray-600 rounded-lg px-3 py-2 bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100" })] }), _jsxs("div", { children: [_jsx("label", { className: "block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1", children: "End Date" }), _jsx("input", { type: "date", value: customEndDate, onChange: (e) => setCustomEndDate(e.target.value), className: "border border-gray-300 dark:border-gray-600 rounded-lg px-3 py-2 bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100" })] })] }) })), _jsxs("div", { className: "mb-4 flex justify-between items-center", children: [_jsxs("p", { className: "text-sm text-gray-600 dark:text-gray-400", children: ["Showing ", displayedLogs.length, " of ", filteredLogs.length, " logs"] }), filteredLogs.length > 10 && !showAllLogs && (_jsxs("button", { onClick: () => setShowAllLogs(true), className: "text-blue-500 hover:text-blue-700 text-sm font-medium", children: ["View All (", filteredLogs.length, ")"] })), showAllLogs && (_jsx("button", { onClick: () => setShowAllLogs(false), className: "text-blue-500 hover:text-blue-700 text-sm font-medium", children: "Show Less" }))] }), _jsx("div", { className: "overflow-x-auto", children: _jsxs("table", { className: "min-w-full bg-white dark:bg-gray-800 rounded-xl", children: [_jsx("thead", { children: _jsxs("tr", { children: [_jsx("th", { className: "px-4 py-3 text-left font-semibold text-gray-700 dark:text-gray-300", children: "Time" }), _jsx("th", { className: "px-4 py-3 text-left font-semibold text-gray-700 dark:text-gray-300", children: "User" }), _jsx("th", { className: "px-4 py-3 text-left font-semibold text-gray-700 dark:text-gray-300", children: "Action" }), _jsx("th", { className: "px-4 py-3 text-left font-semibold text-gray-700 dark:text-gray-300", children: "Status" })] }) }), _jsx("tbody", { children: loading ? (_jsx("tr", { children: _jsx("td", { colSpan: 4, className: "px-4 py-8 text-center text-gray-500 dark:text-gray-400", children: _jsxs("div", { className: "flex items-center justify-center gap-2", children: [_jsx("div", { className: "animate-spin h-5 w-5 border-2 border-blue-500 border-t-transparent rounded-full" }), "Loading logs..."] }) }) })) : displayedLogs.length === 0 ? (_jsx("tr", { children: _jsx("td", { colSpan: 4, className: "px-4 py-8 text-center text-gray-500 dark:text-gray-400", children: "No logs found for the selected time period" }) })) : (displayedLogs.map((log) => (_jsxs("tr", { className: "border-t border-gray-100 dark:border-gray-700 hover:bg-gray-50 dark:hover:bg-gray-800", children: [_jsx("td", { className: "px-4 py-3 text-sm text-gray-900 dark:text-gray-100", children: new Date(log.time).toLocaleString() }), _jsx("td", { className: "px-4 py-3 text-sm text-gray-900 dark:text-gray-100", children: _jsxs("div", { children: [_jsx("div", { className: "font-medium", children: log.user }), log.role && (_jsx("div", { className: "text-xs text-gray-500 dark:text-gray-400", children: log.role }))] }) }), _jsx("td", { className: "px-4 py-3 text-sm text-gray-900 dark:text-gray-100", children: _jsxs("div", { children: [_jsx("div", { className: "font-medium", children: log.action }), _jsx("div", { className: "text-gray-500 dark:text-gray-400 text-xs", children: log.detail && log.detail !== "No details" ? log.detail :
                                                        log.action === "LOGIN" ? `Successful login at ${new Date(log.time).toLocaleTimeString()}` :
                                                            log.action === "LOGOUT" ? `User logged out at ${new Date(log.time).toLocaleTimeString()}` :
                                                                log.action === "CREATE" ? `Created new resource by ${log.user}` :
                                                                    log.action === "UPDATE" ? `Updated resource by ${log.user}` :
                                                                        log.action === "DELETE" ? `Deleted resource by ${log.user}` :
                                                                            `${log.action} operation performed by ${log.user}` })] }) }), _jsx("td", { className: "px-4 py-3", children: log.status === "success" ? (_jsx("span", { className: "inline-block px-3 py-1 rounded-full bg-green-500 dark:bg-green-700 text-white text-xs font-semibold", children: "Success" })) : (_jsx("span", { className: "inline-block px-3 py-1 rounded-full bg-red-500 dark:bg-red-700 text-white text-xs font-semibold", children: "Error" })) })] }, log.id)))) })] }) })] }));
}
