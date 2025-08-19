// constants/roles.ts

export const ROLES = [
  {
    key: "dispatcher",
    name: "Dispatcher",
    icon: "FaBellConcierge",
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
  },
  {
    key: "driver",
    name: "Driver",
    icon: "FaTruck",
    permissions: [
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
};
