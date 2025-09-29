import { useState, useEffect } from "react";
import { useTranslation } from 'react-i18next';
import { fetchActivityLogs, type ActivityLog } from "../../services/adminAPI";

type DateFilter = "today" | "last7days" | "last30days" | "custom";

interface AuditLogTableProps {
  onAuditCountUpdate?: (count: number) => void;
}

export default function AuditLogTable({ onAuditCountUpdate }: AuditLogTableProps) {
  const { t } = useTranslation();
  const [logs, setLogs] = useState<ActivityLog[]>([]);
  const [filteredLogs, setFilteredLogs] = useState<ActivityLog[]>([]);
  const [dateFilter, setDateFilter] = useState<DateFilter>("today");
  const [showAllLogs, setShowAllLogs] = useState(false);
  const [customStartDate, setCustomStartDate] = useState("");
  const [customEndDate, setCustomEndDate] = useState("");
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [lastRefresh, setLastRefresh] = useState<Date>(new Date());
  const [previousLogCount, setPreviousLogCount] = useState<number>(0);
  const [newLogsCount, setNewLogsCount] = useState<number>(0);

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
      } catch (err) {
        console.error("Failed to fetch activity logs:", err);
        setError(t('errors.loadingData', 'Failed to load data'));
        setLogs([]);
      } finally {
        setLoading(false);
      }
    };

    // Initial load only - no auto-refresh
    loadLogs();
  }, [onAuditCountUpdate]);

  const filterLogsByDate = (logs: ActivityLog[], filter: DateFilter, startDate?: string, endDate?: string) => {
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

  const handleDateFilterChange = (filter: DateFilter) => {
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
    } catch (err) {
      console.error("Manual refresh failed:", err);
      setError(t('errors.refreshFailed', 'Failed to refresh'));
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="bg-white dark:bg-gray-900 rounded-2xl shadow border border-gray-100 dark:border-gray-700 p-8">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h2 className="text-2xl font-bold mb-2 dark:text-white flex items-center gap-2">
            <span role="img" aria-label="log">📝</span> System Logs
            {newLogsCount > 0 && (
              <span className="text-xs bg-orange-100 text-orange-800 px-2 py-1 rounded-full animate-pulse">
                ✨ +{newLogsCount} new
              </span>
            )}
          </h2>
        
          <p className="text-xs text-gray-400 dark:text-gray-500 mt-1">
            Last updated: {lastRefresh.toLocaleTimeString()}
          </p>
          {error && (
            <p className="text-sm text-yellow-600 dark:text-yellow-400 mt-1">
              ⚠️ {error} - Using sample data
            </p>
          )}
        </div>
        <div className="flex items-center gap-4">
          {/* Refresh button */}
          <div className="flex gap-2">
            <button
              onClick={manualRefresh}
              disabled={loading}
              className="px-3 py-2 rounded-lg text-xs font-medium bg-blue-500 text-white hover:bg-blue-600 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
              title="Refresh logs"
            >
              {loading ? "⏳" : "🔄 Refresh"}
            </button>
          </div>
          <div className="flex flex-wrap gap-2">
            <button
              onClick={() => handleDateFilterChange("today")}
              className={`px-3 py-2 rounded-lg text-sm font-medium transition-colors ${
                dateFilter === "today"
                  ? "bg-blue-500 text-white"
                  : "bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 hover:bg-gray-200 dark:hover:bg-gray-600"
              }`}
            >
              <span className="hidden sm:inline">Today</span>
              <span className="sm:hidden">1D</span>
            </button>
            <button
              onClick={() => handleDateFilterChange("last7days")}
              className={`px-3 py-2 rounded-lg text-sm font-medium transition-colors ${
                dateFilter === "last7days"
                  ? "bg-blue-500 text-white"
                  : "bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 hover:bg-gray-200 dark:hover:bg-gray-600"
              }`}
            >
              <span className="hidden sm:inline">Last 7 Days</span>
              <span className="sm:hidden">7D</span>
            </button>
            <button
              onClick={() => handleDateFilterChange("last30days")}
              className={`px-3 py-2 rounded-lg text-sm font-medium transition-colors ${
                dateFilter === "last30days"
                  ? "bg-blue-500 text-white"
                  : "bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 hover:bg-gray-200 dark:hover:bg-gray-600"
              }`}
            >
              <span className="hidden sm:inline">Last 30 Days</span>
              <span className="sm:hidden">30D</span>
            </button>
            <button
              onClick={() => handleDateFilterChange("custom")}
              className={`px-3 py-2 rounded-lg text-sm font-medium transition-colors ${
                dateFilter === "custom"
                  ? "bg-blue-500 text-white"
                  : "bg-gray-100 dark:bg-gray-700 text-gray-700 dark:text-gray-300 hover:bg-gray-200 dark:hover:bg-gray-600"
              }`}
            >
              Custom
            </button>
          </div>
        </div>
      </div>

      {/* Custom Date Range Picker */}
      {dateFilter === "custom" && (
        <div className="mb-6 p-4 bg-gray-50 dark:bg-gray-800 rounded-lg">
          <div className="flex flex-col sm:flex-row gap-4 items-start sm:items-center">
            <div>
              <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                Start Date
              </label>
              <input
                type="date"
                value={customStartDate}
                onChange={(e) => setCustomStartDate(e.target.value)}
                className="border border-gray-300 dark:border-gray-600 rounded-lg px-3 py-2 bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100 w-full sm:w-auto"
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                End Date
              </label>
              <input
                type="date"
                value={customEndDate}
                onChange={(e) => setCustomEndDate(e.target.value)}
                className="border border-gray-300 dark:border-gray-600 rounded-lg px-3 py-2 bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100 w-full sm:w-auto"
              />
            </div>
          </div>
        </div>
      )}

      {/* Results Summary */}
      <div className="mb-4 flex justify-between items-center">
        <p className="text-sm text-gray-600 dark:text-gray-400">
          Showing {displayedLogs.length} of {filteredLogs.length} logs
        </p>
        {filteredLogs.length > 10 && !showAllLogs && (
          <button
            onClick={() => setShowAllLogs(true)}
            className="text-blue-500 hover:text-blue-700 text-sm font-medium"
          >
            View All ({filteredLogs.length})
          </button>
        )}
        {showAllLogs && (
          <button
            onClick={() => setShowAllLogs(false)}
            className="text-blue-500 hover:text-blue-700 text-sm font-medium"
          >
            Show Less
          </button>
        )}
      </div>

      <div className="overflow-x-auto">
        <table className="min-w-full bg-white dark:bg-gray-800 rounded-xl">
          <thead>
            <tr>
              <th className="px-4 py-3 text-left font-semibold text-gray-700 dark:text-gray-300">Time</th>
              <th className="px-4 py-3 text-left font-semibold text-gray-700 dark:text-gray-300">User</th>
              <th className="px-4 py-3 text-left font-semibold text-gray-700 dark:text-gray-300 hidden sm:table-cell">Action</th>
              <th className="px-4 py-3 text-left font-semibold text-gray-700 dark:text-gray-300">Status</th>
            </tr>
          </thead>
          <tbody>
            {loading ? (
              <tr>
                <td colSpan={4} className="px-4 py-8 text-center text-gray-500 dark:text-gray-400">
                  <div className="flex items-center justify-center gap-2">
                    <div className="animate-spin h-5 w-5 border-2 border-blue-500 border-t-transparent rounded-full"></div>
                    {t('common.loading', 'Loading')}...
                  </div>
                </td>
              </tr>
            ) : displayedLogs.length === 0 ? (
              <tr>
                <td colSpan={4} className="px-4 py-8 text-center text-gray-500 dark:text-gray-400">
                  No logs found for the selected time period
                </td>
              </tr>
            ) : (
              displayedLogs.map((log) => (
                <tr key={log.id} className="border-t border-gray-100 dark:border-gray-700 hover:bg-gray-50 dark:hover:bg-gray-800">
                  <td className="px-4 py-3 text-sm text-gray-900 dark:text-gray-100">
                    <div className="truncate max-w-24 sm:max-w-none">
                      {new Date(log.time).toLocaleString()}
                    </div>
                  </td>
                  <td className="px-4 py-3 text-sm text-gray-900 dark:text-gray-100">
                    <div>
                      <div className="font-medium truncate max-w-20 sm:max-w-none">{log.user}</div>
                      {log.role && (
                        <div className="text-xs text-gray-500 dark:text-gray-400 hidden sm:block">{log.role}</div>
                      )}
                    </div>
                  </td>
                  <td className="px-4 py-3 text-sm text-gray-900 dark:text-gray-100 hidden sm:table-cell">
                    <div>
                      <div className="font-medium">{log.action}</div>
                      <div className="text-gray-500 dark:text-gray-400 text-xs">
                        {log.detail && log.detail !== "No details" ? log.detail : 
                         log.action === "LOGIN" ? `Successful login at ${new Date(log.time).toLocaleTimeString()}` :
                         log.action === "LOGOUT" ? `User logged out at ${new Date(log.time).toLocaleTimeString()}` :
                         log.action === "CREATE" ? `Created new resource by ${log.user}` :
                         log.action === "UPDATE" ? `Updated resource by ${log.user}` :
                         log.action === "DELETE" ? `Deleted resource by ${log.user}` :
                         `${log.action} operation performed by ${log.user}`
                        }
                      </div>
                    </div>
                  </td>
                  <td className="px-4 py-3">
                    {log.status === "success" ? (
                      <span className="inline-block px-2 sm:px-3 py-1 rounded-full bg-green-500 dark:bg-green-700 text-white text-xs font-semibold">
                        <span className="hidden sm:inline">Success</span>
                        <span className="sm:hidden">✓</span>
                      </span>
                    ) : (
                      <span className="inline-block px-2 sm:px-3 py-1 rounded-full bg-red-500 dark:bg-red-700 text-white text-xs font-semibold">
                        <span className="hidden sm:inline">Error</span>
                        <span className="sm:hidden">✗</span>
                      </span>
                    )}
                  </td>
                </tr>
              ))
            )}
          </tbody>
        </table>
      </div>
    </div>
  );
}