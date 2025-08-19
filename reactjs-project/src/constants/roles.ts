<<<<<<< HEAD
// constants/roles.ts

export const ROLES = [
=======

import type { Role } from "../types/dashboard";

export const ROLES: Role[] = [
  {
    key: "admin",
    name: "Admin",
    icon: "FaCrown",
    permissions: [
      "All permissions",
      "User management",
      "System configuration",
      "View logs",
    ],
  },
>>>>>>> 042a7c16d89d185c6e74a32de79f098e8a6971b5
  {
    key: "dispatcher",
    name: "Dispatcher",
    icon: "FaBellConcierge",
<<<<<<< HEAD
    permissions: [
      "CREATE_ORDER",
      "ASSIGN_DRIVER",
      "VIEW_ORDER",
      "EDIT_ORDER"
    ]
  },
  {
    key: "fleet_manager",
    name: "Fleet Manager",
    icon: "FaTools",
    permissions: [
      "VIEW_VEHICLE",
      "EDIT_VEHICLE",
      "ASSIGN_VEHICLE"
    ]
=======
    permissions: ["Create order", "Assign vehicle & driver", "View map", "Order management"],
  },
  {
    key: "fleetManager",
    name: "Fleet Manager",
    icon: "FaTools",
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
    icon: "FaChartBar",
    permissions: [
      "View reports",
      "Data analysis",
      "Export reports",
      "Monitor KPI",
    ],
>>>>>>> 042a7c16d89d185c6e74a32de79f098e8a6971b5
  },
  {
    key: "driver",
    name: "Driver",
    icon: "FaTruck",
    permissions: [
<<<<<<< HEAD
      "VIEW_ORDER",
      "UPDATE_STATUS"
    ]
  },
  {
    key: "operations_manager",
    name: "Operations Manager",
    icon: "FaUserCog",
    permissions: [
      "VIEW_REPORTS",
      "EDIT_REPORTS"
    ]
  }
];

export const ALL_PERMISSIONS = [
  "CREATE_ORDER",
  "ASSIGN_DRIVER",
  "VIEW_ORDER",
  "EDIT_ORDER",
  "VIEW_VEHICLE",
  "EDIT_VEHICLE",
  "ASSIGN_VEHICLE",
  "UPDATE_STATUS",
  "VIEW_REPORTS",
  "EDIT_REPORTS"
];

export const PERMISSION_DESCRIPTIONS: Record<string, string> = {
  CREATE_ORDER: "Tạo đơn hàng mới.",
  ASSIGN_DRIVER: "Gán tài xế cho đơn hàng.",
  VIEW_ORDER: "Xem danh sách đơn hàng.",
  EDIT_ORDER: "Chỉnh sửa thông tin đơn hàng.",
  VIEW_VEHICLE: "Xem danh sách phương tiện.",
  EDIT_VEHICLE: "Chỉnh sửa thông tin phương tiện.",
  ASSIGN_VEHICLE: "Gán phương tiện cho tài xế.",
  UPDATE_STATUS: "Cập nhật trạng thái đơn hàng.",
  VIEW_REPORTS: "Xem báo cáo vận hành.",
  EDIT_REPORTS: "Chỉnh sửa báo cáo vận hành."
=======
      "Receive new order",
      "Update delivery status",
      "View route",
      "Send receipt image",
      "E-signature",
    ],
  },
];

export const ALL_PERMISSIONS = Array.from(
  new Set(ROLES.flatMap((r) => r.permissions))
);

export const PERMISSION_DESCRIPTIONS: Record<string, string> = {
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
>>>>>>> 042a7c16d89d185c6e74a32de79f098e8a6971b5
};
