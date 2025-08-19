interface Log {
  time: string;
  user: string;
  action: string;
  detail: string;
  ip: string;
  status: "success" | "error";
}

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
    <div className="bg-white border border-red-100 shadow-lg rounded-2xl p-8">
      <h2 className="text-2xl font-bold mb-2 text-gray-800 drop-shadow-sm flex items-center gap-2">
          <span role="img" aria-label="log">📝</span> System Logs
      </h2>
        <p className="text-gray-600 mb-6">Track all user activities (logs) in the system</p>
      <div className="overflow-x-auto">
        <table className="min-w-full bg-transparent rounded-xl">
          <thead>
            <tr className="border-b border-red-100">
              <th className="px-4 py-2 text-left font-semibold text-gray-700">Time</th>
              <th className="px-4 py-2 text-left font-semibold text-gray-700">User</th>
              <th className="px-4 py-2 text-left font-semibold text-gray-700">Action</th>
              <th className="px-4 py-2 text-left font-semibold text-gray-700">Detail</th>
              <th className="px-4 py-2 text-left font-semibold text-gray-700">IP Address</th>
              <th className="px-4 py-2 text-left font-semibold text-gray-700">Status</th>
            </tr>
          </thead>
          <tbody>
            {logs.map((log, idx) => (
              <tr key={idx} className="border-t border-red-50">
                <td className="px-4 py-2 text-gray-700">{log.time}</td>
                <td className="px-4 py-2 text-gray-700">{log.user}</td>
                <td className="px-4 py-2 text-gray-700">{log.action}</td>
                <td className="px-4 py-2 text-gray-700">{log.detail}</td>
                <td className="px-4 py-2 text-gray-700">{log.ip}</td>
                <td className="px-4 py-2">
                  {log.status === "success" ? (
                    <span className="inline-block px-3 py-1 rounded-full bg-red-600 text-white text-sm font-semibold shadow">Success</span>
                  ) : (
                    <span className="inline-block px-3 py-1 rounded-full bg-gray-600 text-white text-sm font-semibold shadow">Error</span>
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