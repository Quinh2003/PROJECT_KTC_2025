import { useState } from "react";
import { FaCrown, FaTools, FaChartBar, FaTruck } from "react-icons/fa";
import { FaBellConcierge } from "react-icons/fa6";

const rolesData = [
	{
		key: "admin",
		name: "Admin",
		icon: <FaCrown className="text-red-600" />,
		permissions: [
			"All permissions",
			"User management",
			"System configuration",
			"View logs",
		],
	},
	{
		key: "dispatcher",
		name: "Dispatcher",
		icon: <FaBellConcierge className="text-red-500" />,
		permissions: [
			"Create order",
			"Assign vehicle & driver",
			"View map",
			"Order management",
		],
	},
	{
		key: "fleetManager",
		name: "Fleet Manager",
		icon: <FaTools className="text-red-600" />,
		permissions: [
			"Vehicle management",
			"Schedule maintenance",
			"View vehicle reports",
			"Update vehicle status",
		],
	},
	{
		key: "operationsManager",
		name: "Operations Manager",
		icon: <FaChartBar className="text-red-500" />,
		permissions: ["View reports", "Data analysis", "Export reports", "Monitor KPI"],
	},
	{
		key: "driver",
		name: "Driver",
		icon: <FaTruck className="text-red-600" />,
		permissions: [
			"Receive new order",
			"Update delivery status",
			"View route",
			"Send receipt image",
			"E-signature",
		],
	},
];

const allPermissions = Array.from(
	new Set(rolesData.flatMap((r) => r.permissions))
);

// Permission descriptions in English
const permissionDescriptions: Record<string, string> = {
	"All permissions": "Full system administration rights.",
	"User management": "Create, edit, and delete user accounts.",
	"System configuration": "Change system configuration parameters.",
	"View logs": "View system activity history.",
	"Create order": "Create a new transport order.",
	"Assign vehicle & driver": "Assign vehicle and driver to an order.",
	"View map": "View transport route map.",
	"Order management": "Manage transport orders.",
	"Vehicle management": "Manage vehicle list and status.",
	"Schedule maintenance": "Schedule vehicle maintenance.",
	"View vehicle reports": "View vehicle-related reports.",
	"Update vehicle status": "Update vehicle operational status.",
	"View reports": "View summary reports.",
	"Data analysis": "Analyze operational data.",
	"Export reports": "Export reports to file.",
	"Monitor KPI": "Monitor key performance indicators.",
	"Receive new order": "Receive new delivery orders.",
	"Update delivery status": "Update delivery order status.",
	"View route": "View delivery route.",
	"Send receipt image": "Send receipt image upon delivery.",
	"E-signature": "Electronically sign for delivery confirmation.",
};

export default function RoleTable() {
	const [roles, setRoles] = useState(rolesData);
	const [editingRole, setEditingRole] = useState<null | typeof roles[0]>(null);
	const [tempPermissions, setTempPermissions] = useState<string[]>([]);
	const [selectedPermission, setSelectedPermission] = useState<string | null>(null);

	const handlePermissionClick = (_roleName: string, permission: string) => {
		setSelectedPermission(permission);
	};

	const handleEditRole = (role: typeof roles[0]) => {
		setEditingRole(role);
		setTempPermissions(role.permissions);
	};

	const handleTogglePermission = (perm: string) => {
		setTempPermissions((prev) =>
			prev.includes(perm)
				? prev.filter((p) => p !== perm)
				: [...prev, perm]
		);
	};

	const handleSavePermissions = () => {
		if (!editingRole) return;
		setRoles((prev) =>
			prev.map((r) =>
				r.key === editingRole.key ? { ...r, permissions: tempPermissions } : r
			)
		);
		setEditingRole(null);
		setTempPermissions([]);
	};

	return (
		<div className="bg-white border border-red-100 shadow-lg rounded-xl p-6 transition-all duration-200">
			<div className="flex justify-between items-center mb-4">
				<h2 className="text-2xl font-bold text-gray-800 drop-shadow-sm">
					Role List
				</h2>
			</div>
			{/* Table */}
			<div className="overflow-x-auto rounded-lg">
				<table className="min-w-full text-gray-700">
					<thead>
						<tr className="font-semibold">
							<th className="px-6 py-4 text-left ">Role</th>
							<th className="px-6 py-4 text-left ">Permissions</th>
							<th className="px-6 py-4 text-center ">Actions</th>
						</tr>
					</thead>
					<tbody className="divide-y divide-red-50">
						{roles.map((role, idx) => (
							<tr key={idx} className="hover:bg-gray-25 transition-colors">
								<td className="px-6 py-4">
									<div className="flex items-center gap-3">
										<div className="flex-shrink-0 w-10 h-10 bg-red-50 rounded-full flex items-center justify-center">
											<span className="text-xl">{role.icon}</span>
										</div>
										<div>
											<div className="font-semibold text-gray-900">{role.name}</div>
											<div className="text-sm text-gray-500">System Role</div>
										</div>
									</div>
								</td>
								<td className="px-6 py-4">
									<div className="flex flex-wrap gap-2 max-w-md">
										{role.permissions.slice(0, 2).map((perm, i) => (
											<span
												key={i}
												className="inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-red-100 text-red-700 hover:bg-red-200 cursor-pointer transition-colors"
												onClick={() => handlePermissionClick(role.name, perm)}
											>
												{perm}
											</span>
										))}
										{role.permissions.length > 2 && (
											<span 
												className="inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-gray-100 text-gray-600 hover:bg-gray-200 cursor-pointer transition-colors"
												onClick={() => handleEditRole(role)}
											>
												+{role.permissions.length - 2} more
											</span>
										)}
									</div>
								</td>
								<td className="px-6 py-4 text-center">
									<button
										className="inline-flex items-center px-4 py-2 text-sm font-medium bg-red-600 text-white rounded-lg hover:bg-red-700 focus:ring-2 focus:ring-red-500 focus:ring-offset-1 transition-all duration-200"
										onClick={() => handleEditRole(role)}
									>
										<svg className="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
											<path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z" />
										</svg>
										Edit
									</button>
								</td>
							</tr>
						))}
					</tbody>
				</table>
			</div>
			
			{/* Permission Detail Modal */}
			{selectedPermission && (
				<div className="fixed inset-0 z-[9999] flex items-center justify-center">
					<div className="absolute inset-0 bg-black/60 backdrop-blur-sm"></div>
					<div className="relative z-10 bg-white border border-red-200 shadow-2xl rounded-2xl p-8 w-[400px] space-y-3 overflow-hidden">
						{/* Loang effect inside form */}
						<div className="absolute top-0 left-0 w-[350px] h-[120px] bg-gradient-to-br from-red-100/30 via-red-50/40 to-white/20 rounded-3xl blur-2xl pointer-events-none animate-[loangMove_6s_ease-in-out_infinite]" style={{zIndex:0}}></div>
						<div className="absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[200px] h-[200px] bg-gradient-to-br from-red-50/40 via-white/30 to-red-25/20 rounded-full blur-3xl pointer-events-none animate-[loangFade_8s_ease-in-out_infinite]" style={{zIndex:0}}></div>
						
						<div className="relative z-10">
							<div className="flex items-center gap-3 mb-4">
								<div className="w-12 h-12 bg-red-100 rounded-full flex items-center justify-center">
									<svg className="w-6 h-6 text-red-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
										<path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z" />
									</svg>
								</div>
								<div>
									<h3 className="text-xl font-bold text-gray-800 drop-shadow-sm">{selectedPermission}</h3>
									<p className="text-sm text-gray-600">Permission Details</p>
								</div>
							</div>
							<div className="bg-gray-50 rounded-lg p-4 mb-6">
								<p className="text-gray-700 leading-relaxed">
									{permissionDescriptions[selectedPermission] || "No description available for this permission."}
								</p>
							</div>
							<div className="flex justify-end">
								<button
									className="px-4 py-2 rounded-lg bg-gradient-to-br from-red-500 to-red-600 text-white font-bold shadow hover:opacity-90 transition"
									onClick={() => setSelectedPermission(null)}
								>
									Close
								</button>
							</div>
						</div>
					</div>
				</div>
			)}

			{/* Edit Permissions Modal */}
			{editingRole && (
				<div className="fixed inset-0 z-[9999] flex items-center justify-center">
					<div className="absolute inset-0 bg-black/60 backdrop-blur-sm"></div>
					<div className="relative z-10 bg-white border border-red-200 shadow-2xl rounded-2xl p-8 w-[600px] max-h-[90vh] overflow-y-auto space-y-3 overflow-hidden">
						{/* Loang effect inside form */}
						<div className="absolute top-0 left-0 w-[500px] h-[120px] bg-gradient-to-br from-red-100/30 via-red-50/40 to-white/20 rounded-3xl blur-2xl pointer-events-none animate-[loangMove_6s_ease-in-out_infinite]" style={{zIndex:0}}></div>
						<div className="absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[300px] h-[300px] bg-gradient-to-br from-red-50/40 via-white/30 to-red-25/20 rounded-full blur-3xl pointer-events-none animate-[loangFade_8s_ease-in-out_infinite]" style={{zIndex:0}}></div>
						
						<div className="relative z-10">
							<div className="flex items-center gap-3 mb-6">
								<div className="w-12 h-12 bg-red-100 rounded-full flex items-center justify-center">
									{editingRole.icon}
								</div>
								<div>
									<h2 className="text-xl font-bold text-gray-800 drop-shadow-sm">Edit Permissions</h2>
									<p className="text-gray-600">Configure permissions for {editingRole.name}</p>
								</div>
							</div>
							
							<div className="bg-gray-50 rounded-lg p-4 mb-6">
								<h3 className="font-semibold text-gray-700 mb-3">Available Permissions</h3>
								<div className="grid grid-cols-1 md:grid-cols-2 gap-3">
									{allPermissions.map((perm) => (
										<label
											key={perm}
											className="flex items-center p-3 bg-white rounded-lg border border-red-200 cursor-pointer hover:border-red-300 transition-colors"
										>
											<input
												type="checkbox"
												checked={tempPermissions.includes(perm)}
												onChange={() => handleTogglePermission(perm)}
												className="w-4 h-4 text-red-600 rounded border-red-300 focus:ring-red-500"
											/>
											<div className="ml-3 flex-1">
												<div className="text-sm font-medium text-gray-700">{perm}</div>
												<div className="text-xs text-gray-500">
													{permissionDescriptions[perm]?.substring(0, 60)}...
												</div>
											</div>
										</label>
									))}
								</div>
							</div>
							
							<div className="flex gap-2 justify-end pt-2">
								<button
									className="px-4 py-2 rounded-lg bg-gray-700 text-white font-semibold hover:bg-gray-600 transition"
									onClick={() => setEditingRole(null)}
								>
									Cancel
								</button>
								<button
									className="px-4 py-2 rounded-lg bg-gradient-to-br from-red-500 to-red-600 text-white font-bold shadow hover:opacity-90 transition"
									onClick={handleSavePermissions}
								>
									Save Changes
								</button>
							</div>
						</div>
					</div>
				</div>
			)}
		</div>
	);
}