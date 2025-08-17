import type { Log } from "../../types/dashboard";

const logs: Log[] = [
  {
    time: "2024-01-15 10:30:25",
    user: "nguyenvana@company.com",
    action: "Create shipment order",
    detail: "Created order ORD001 for customer ABC",
    ip: "192.168.1.100",
    status: "success",
  },
  {
    time: "2024-01-15 10:25:12",
    user: "tranthib@company.com",
    action: "Update vehicle",
    detail: "Updated status for vehicle 29A-12345",
    ip: "192.168.1.101",
    status: "success",
  },
  {
    time: "2024-01-15 10:20:45",
    user: "system",
    action: "Login failed",
    detail: "Login failed from IP 192.168.1.200",
    ip: "192.168.1.200",
    status: "error",
  },
];

export default function AuditLogTable() {
  return (
    <div className="bg-white dark:bg-gray-900 rounded-2xl shadow border border-gray-100 dark:border-gray-700 p-8">
      <h2 className="text-2xl font-bold mb-2 dark:text-white flex items-center gap-2">
        <span role="img" aria-label="log">📝</span> System Logs
      </h2>
      <p className="text-gray-500 dark:text-gray-300 mb-6">
        Track all user activities (logs) in the system
      </p>
      <div className="overflow-x-auto">
        <table className="min-w-full bg-white dark:bg-gray-800 rounded-xl">
          <thead>
            <tr>
              <th className="px-4 py-2 text-left font-semibold text-gray-700 dark:text-gray-300">Time</th>
              <th className="px-4 py-2 text-left font-semibold text-gray-700 dark:text-gray-300">User</th>
              <th className="px-4 py-2 text-left font-semibold text-gray-700 dark:text-gray-300">Action</th>
              <th className="px-4 py-2 text-left font-semibold text-gray-700 dark:text-gray-300">Detail</th>
              <th className="px-4 py-2 text-left font-semibold text-gray-700 dark:text-gray-300">IP Address</th>
              <th className="px-4 py-2 text-left font-semibold text-gray-700 dark:text-gray-300">Status</th>
            </tr>
          </thead>
          <tbody>
            {logs.map((log, idx) => (
              <tr key={idx} className="border-t border-gray-100 dark:border-gray-700">
                <td className="px-4 py-2">{log.time}</td>
                <td className="px-4 py-2">{log.user}</td>
                <td className="px-4 py-2">{log.action}</td>
                <td className="px-4 py-2">{log.detail}</td>
                <td className="px-4 py-2">{log.ip}</td>
                <td className="px-4 py-2">
                  {log.status === "success" ? (
                    <span className="inline-block px-3 py-1 rounded-full bg-green-500 dark:bg-green-700 text-white text-sm font-semibold">
                      Success
                    </span>
                  ) : (
                    <span className="inline-block px-3 py-1 rounded-full bg-red-500 dark:bg-red-700 text-white text-sm font-semibold">
                      Error
                    </span>
                  )}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}