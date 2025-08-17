
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
  {
    key: "dispatcher",
    name: "Dispatcher",
    icon: "FaBellConcierge",
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
  },
  {
    key: "driver",
    name: "Driver",
    icon: "FaTruck",
    permissions: [
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
};
