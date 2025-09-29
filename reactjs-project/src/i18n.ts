import i18n from 'i18next';
import { initReactI18next } from 'react-i18next';
import LanguageDetector from 'i18next-browser-languagedetector';

const resources = {
  en: {
    translation: {
      "navigation": {
        "dashboard": "Dashboard",
        "orders": "Orders",
        "fleet": "Fleet",
        "operations": "Operations",
        "admin": "Admin",
        "profile": "Profile",
        "logout": "Logout",
        "login": "Login"
      },
      "common": {
        "loading": "Loading...",
        "error": "Error",
        "success": "Success",
        "cancel": "Cancel",
        "confirm": "Confirm",
        "save": "Save",
        "edit": "Edit",
        "delete": "Delete",
        "add": "Add",
        "update": "Update",
        "search": "Search",
        "filter": "Filter",
        "reset": "Reset",
        "submit": "Submit",
        "back": "Back",
        "next": "Next",
        "previous": "Previous",
        "close": "Close",
        "page": "Page",
        "refresh": "Refresh",
        "view": "View",
        "download": "Download",
        "upload": "Upload",
        "select": "Select",
        "choose": "Choose",
        "language": "Language",
        "english": "English",
        "vietnamese": "Tiếng Việt",
        "welcome": "Welcome",
        "total": "Total",
        "active": "Active",
        "inactive": "Inactive",
        "pending": "Pending",
        "completed": "Completed",
        "cancelled": "Cancelled",
        "from": "From",
        "to": "To",
        "distance": "Distance",
        "duration": "Duration",
        "unknown": "Unknown",
        "all": "All",
        "searching": "Searching...",
        "notAvailable": "Not available",
        "options": "Options",
        "today": "Today",
        "noName": "(No name)",
        "notUpdated": "Not updated",
        "noPhone": "No phone number",
        "yes": "Yes",
        "no": "No",
        "characters": "characters",
        "ofTotal": "of total",
        "timeFilters": {
          "24h": "24h",
          "7d": "7d",
          "1month": "1 month"
        }
      },
      "auth": {
        "login": {
          "title": "Login to Your Account",
          "subtitle": "Welcome back! Please enter your credentials",
          "email": "Email",
          "password": "Password",
          "rememberMe": "Remember me",
          "forgotPassword": "Forgot password?",
          "loginButton": "Sign In",
          "noAccount": "Don't have an account?",
          "signUp": "Sign up here",
          "invalidCredentials": "Invalid email or password",
          "loginSuccess": "Login successful!",
          "needHelp": "Need help accessing your account?",
          "contactAdmin": "Contact Administrator"
        }
      },
      "validation": {
        "required": "This field is required",
        "invalidEmail": "Please enter a valid email",
        "invalidPhone": "Please enter a valid phone number",
        "passwordTooShort": "Password must be at least 8 characters",
        "passwordsNotMatch": "Passwords do not match"
      },
      "notifications": {
        "loginSuccess": "Login successful!",
        "loginFailed": "Login failed!",
        "saveSuccess": "Saved successfully!",
        "deleteSuccess": "Deleted successfully!",
        "updateSuccess": "Updated successfully!",
        "error": "An error occurred",
        "networkError": "Network error occurred"
      },
      "orders": {
        "orderNumber": "Order"
      },
      "dashboard": {
        "dispatcher": {
          "title": "Dispatcher Dashboard",
          "subtitle": "Manage orders and fleet operations",
          "tabs": {
            "orders": "Orders",
            "resources": "Resources",
            "assignment": "Assignment"
          },
          "orders": {
            "title": "Order List",
            "overview": "Order Overview",
            "totalShipments": "Total shipments",
            "totalVehicles": "Total vehicles",
            "pendingPackages": "Pending orders",
            "packagesDelivered": "Orders delivered",
            "trackingTitle": "Live tracking with real-time location",
            "enterOrderId": "Enter order ID...",
            "search": "Search",
            "clickToViewMap": "Click on order to view route on map",
            "selectOrderToViewMap": "Select an order to view route on map",
            "customer": "Customer",
            "driver": "Driver",
            "vehicle": "Vehicle",
            "notAssigned": "Not assigned",
            "orderNotFound": "Order not found with the entered ID",
            "searchError": "Error searching for order"
          },
            "vehicles": {
              "title": "Vehicle List",
              "licensePlate": "License plate",
              "type": "Type",
              "driver": "Driver",
              "status": "Status",
              "weight": "Weight",
              "volume": "Volume",
              "maintenanceDate": "Maintenance date",
              "notScheduled": "Not scheduled",
              "available": "Available",
              "inUse": "In Use",
              "maintenance": "Maintenance",
              "maintenanceRequired": "Maintenance Required",
              "searchPlaceholder": "Search by license plate, vehicle type or driver...",
              "unassigned": "Unassigned",
              "noDriverAssigned": "No driver assigned",
              "total": "Total",
              "assignDriver": "Assign driver",
              "assignDriverToVehicle": "Assign driver to vehicle",
              "assignSuccess": "Driver assigned successfully!",
              "assignError": "Failed to assign driver",
              "selectDriver": "Select driver",
              "updated": "Updated",
              "weightUnit": "tons",
              "noVehiclesFound": "No vehicles found",
              "noFilterResults": "No vehicles match your filter criteria",
              "noVehiclesInSystem": "No vehicles in the system yet",
              "unassignSuccess": "Driver unassigned successfully!",
              "confirmAssign": "Confirm Assignment",
              "assigning": "Assigning..."
            },
          "drivers": {
            "title": "Driver List",
            "name": "Name",
            "phone": "Phone",
            "status": "Status",
            "currentOrder": "Current order",
            "rating": "Rating",
            "searchPlaceholder": "Search by name, email or phone...",
            "contactInfo": "Contact Info",
            "noDriversFound": "No drivers found",
            "noSearchResults": "No drivers match your search criteria",
            "noDriversInSystem": "No drivers in the system yet",
            "driverNotFound": "Driver not found"
          },
            "assignment": {
              "title": "Order Assignment Management",
              "noOrders": "No orders waiting for assignment",
              "products": "products",
              "selectVehicle": "Select vehicle...",
              "assignVehicle": "Assign vehicle",
              "updateVehicle": "Update vehicle",
              "headers": {
                "orderId": "Order ID",
                "products": "Products",
                "customer": "Customer",
                "route": "Route",
                "orderDetails": "Order Details",
                "createdDate": "Created Date",
                "vehicleDriver": "Vehicle & Driver",
                "actions": "Actions"
              },
              "edit": "Edit",
              "noPhone": "No phone",
              "unknownDriver": "Unknown driver",
              "assigned": "Assigned",
              "unassignVehicle": "Unassign vehicle",
              "vehicleAssigned": "Vehicle {{vehicle}} assigned successfully to order {{orderId}}! Delivery tracking auto-updated.",
              "vehicleUpdated": "Vehicle {{vehicle}} updated successfully for order {{orderId}}! Delivery tracking auto-updated.",
              "vehicleUnassigned": "Vehicle unassigned successfully from order {{orderId}}!",
              "assignError": "Failed to assign vehicle: ",
              "unassignError": "Failed to unassign vehicle: "
            },
          "pagination": {
            "showing": "Showing {{start}}-{{end}} of {{total}} orders",
            "page": "Page {{current}} / {{total}}"
          }
        },
        "errors": {
          "notLoggedIn": "Not logged in",
          "needLogin": "You need to login to access dashboard!",
          "invalidRole": "Invalid role",
          "cannotDetermineAccess": "Cannot determine your access rights!",
          "noPermission": "No permission or token expired!",
          "loadingData": "Failed to load data",
          "refreshFailed": "Failed to refresh",
          "mapUnavailable": "Map Temporarily Unavailable",
          "invalidApiResponse": "API did not return valid user data",
          "fetchUsersFailed": "Failed to fetch user data. Please try again or check backend.",
          "userNotFound": "User not found",
          "unknownError": "Unknown error"
        },
        "fleet": {
          "title": "Fleet Management Dashboard",
          "subtitle": "Manage vehicles and maintenance",
          "refreshVehicles": "Refresh vehicle list",
          "tabs": {
            "vehicles": "Vehicle Management",
            "maintenance": "Vehicle Maintenance",
            "schedule": "Maintenance Schedule"
          },
          "stats": {
            "totalVehicles": "Total Vehicles",
            "inUse": "In Use",
            "maintenance": "Under Maintenance",
            "needMaintenance": "Need Maintenance"
          },
          "status": {
            "available": "Available",
            "inUse": "In Use",
            "underMaintenance": "Under Maintenance",
            "needMaintenance": "Need Maintenance"
          },
          "vehicleType": "Vehicle Type",
          "capacity": "Capacity",
          "volume": "Volume",
          "driver": "Driver",
          "notAssigned": "Not Assigned",
          "maintenanceHistory": "Maintenance History",
          "scheduleMaintenance": "Schedule Maintenance",
          "selectVehicle": "Select vehicle",
          "selectType": "Select type",
          "maintenanceType": "Maintenance Type",
          "workDescription": "Work Description",
          "workDescriptionPlaceholder": "Describe the work to be performed in detail",
          "implementationDate": "Implementation Date",
          "estimatedCost": "Estimated Cost",
          "nextMaintenance": "Next Maintenance",
          "type": "Type",
          "maintenanceContent": "Maintenance Content",
          "cost": "Cost",
          "maintenanceDate": "Maintenance Date",
          "nextMaintenanceDate": "Next Maintenance Date",
          "notes": "Notes",
          "noNotes": "No notes",
          "loadingVehicles": "Loading vehicle list...",
          "notesPlaceholder": "Enter notes (if any)",
          "maintenanceTypes": {
            "scheduled": "Scheduled Maintenance",
            "repair": "Repair",
            "emergency": "Emergency Repair"
          },
          "maintenanceStatus": {
            "completed": "Completed",
            "inProgress": "In Progress",
            "pending": "Pending"
          },
          "errors": {
            "loadMaintenanceHistory": "Cannot load maintenance data.",
            "loadVehicles": "Cannot load vehicle list",
            "submitError": "Error occurred while {{action}} vehicle. Please try again."
          },
          "maintenance": {
            "scheduleSuccess": "Maintenance scheduled successfully!",
            "scheduleError": "Error saving maintenance schedule!",
            "overdue": "Overdue {{days}} days",
            "remaining": "{{days}} days remaining"
          },
          "vehicleTypes": {
            "truck": "Truck",
            "van": "Van", 
            "motorcycle": "Motorcycle",
            "car": "Car"
          },
          "emptyState": {
            "noResults": "No vehicles found",
            "noVehicles": "No vehicles yet",
            "tryFilters": "Try changing filters or search terms",
            "addFirst": "Add your first vehicle to the system"
          },
          "form": {
            "validForm": "Form is valid - ready to {{action}} vehicle"
          }
        },
        "admin": {
          "title": {
            "admin": "Admin Dashboard",
            "userManagement": "User Management Dashboard",
            "rolePermissions": "Role Permissions Dashboard", 
            "systemConfiguration": "System Configuration Dashboard",
            "systemLogs": "System Logs Dashboard"
          },
          "subtitle": "Manage system users, roles and configurations",
          "tabs": {
            "users": "Users",
            "roles": "Roles",
            "settings": "Settings",
            "logs": "Logs"
          },
          "stats": {
            "totalUsers": "Total Users",
            "totalRoles": "Total Roles",
            "systemConfig": "System Config",
            "auditEvents": "Audit Events"
          },
          "settings": {
            "apiKeyPlaceholder": "Enter API key",
            "urlPlaceholder": "URL endpoint"
          }
        },
        "operations": {
          "title": "Operations Manager Dashboard",
          "subtitle": "Monitor and manage operational metrics",
          "tabs": {
            "overview": "Overview",
            "performance": "Performance", 
            "monitoring": "Monitoring",
            "staff": "Staff Management"
          },
          "overview": {
            "todaysOrders": "Today's Orders",
            "activeVehicles": "Active Vehicles", 
            "todaysRevenue": "Today's Revenue",
            "completedOrders": "Completed Orders",
            "comparedToYesterday": "compared to yesterday",
            "ofTotalVehicles": "of total vehicles"
          },
          "performance": {
            "loadingData": "Loading performance data...",
            "metrics": {
              "deliverySuccessRate": "Delivery Success Rate",
              "avgDeliveryTime": "Average Delivery Time",
              "transportationCost": "Transportation Cost/km",
              "totalKmTransported": "Total km Transported"
            },
            "target": "Target",
            "recentOrders": "Recent Orders",
            "headers": {
              "orderId": "Order ID",
              "customer": "Customer",
              "route": "Route",
              "createdTime": "Created Time",
              "status": "Status"
            }
          },
          "monitoring": {
            "totalVehicles": "Total Vehicles",
            "maintenanceRequests": "Maintenance Requests",
            "resourceDetails": "Resource Details",
            "headers": {
              "vehicleName": "Vehicle Name",
              "type": "Type",
              "driver": "Driver", 
              "status": "Status",
              "createdDate": "Created Date"
            },
            "pagination": {
              "showing": "Showing {{start}}-{{end}} of {{total}} vehicles"
            }
          },
          "status": {
            "processing": "Processing",
            "shipped": "Shipped"
          },
          "pagination": {
            "showing": "Showing {{start}}-{{end}} of {{total}} orders",
            "previous": "Previous",
            "next": "Next"
          },
          "staff": {
            "metrics": {
              "totalStaff": "Total Staff",
              "working": "Working",
              "onLeave": "On Leave",
              "avgPerformance": "Avg Performance"
            },
            "staffList": "Staff List",
            "departments": {
              "transportation": "Transportation",
              "dispatch": "Dispatch",
              "maintenance": "Maintenance"
            },
            "roles": {
              "admin": "Admin",
              "driver": "Driver",
              "dispatcher": "Dispatcher",
              "fleetManager": "Fleet Manager",
              "customer": "Customer",
              "operationsManager": "Operations Manager"
            },
            "status": {
              "working": "Working",
              "onLeave": "On Leave",
              "sickLeave": "Sick Leave",
              "terminated": "Terminated"
            },
            "headers": {
              "name": "Name",
              "role": "Role",
              "department": "Department",
              "status": "Status",
              "contact": "Contact"
            }
          },
          "chart": {
            "monthlyRevenue": "Monthly Revenue",
            "totalAnnualRevenue": "Total annual revenue",
            "growthComparedToLastMonth": "Growth compared to last month",
            "monthlyAverage": "Monthly average",
            "month": "Month"
          }
        },
        "driver": {
          "title": "Driver Dashboard",
          "welcome": "Hello"
        },
        "auth": {
          "login": {
            "title": "Account Login",
            "subtitle": "Welcome back! Please log in to access your information",
            "email": "Email",
            "password": "Password",
            "emailPlaceholder": "Enter your email address",
            "passwordPlaceholder": "Enter your password",
            "loginButton": "Login",
            "invalidCredentials": "Invalid credentials. Please try again.",
            "loginSuccess": "Login successful"
          }
        },
        "search": {
          "placeholder": "Search all...",
          "advancedSearch": "Advanced Search",
          "noResults": "No results found for \"{{query}}\"",
          "suggestions": "Suggestions:",
          "types": {
            "vehicle": "Vehicle",
            "order": "Order",
            "user": "User",
            "driver": "Driver"
          }
        }
      }
    }
  },
  vi: {
    translation: {
      "navigation": {
        "dashboard": "Bảng điều khiển",
        "orders": "Đơn hàng",
        "fleet": "Đội xe",
        "operations": "Vận hành",
        "admin": "Quản trị",
        "profile": "Hồ sơ",
        "logout": "Đăng xuất",
        "login": "Đăng nhập"
      },
      "common": {
        "loading": "Đang tải...",
        "error": "Lỗi",
        "success": "Thành công",
        "cancel": "Hủy",
        "confirm": "Xác nhận",
        "save": "Lưu",
        "edit": "Chỉnh sửa",
        "delete": "Xóa",
        "add": "Thêm",
        "update": "Cập nhật",
        "search": "Tìm kiếm",
        "filter": "Lọc",
        "reset": "Đặt lại",
        "submit": "Gửi",
        "back": "Quay lại",
        "next": "Tiếp theo",
        "previous": "Trước",
        "close": "Đóng",
        "page": "Trang",
        "refresh": "Làm mới",
        "view": "Xem",
        "download": "Tải xuống",
        "upload": "Tải lên",
        "select": "Chọn",
        "choose": "Lựa chọn",
        "language": "Ngôn ngữ",
        "english": "English",
        "vietnamese": "Tiếng Việt",
        "welcome": "Chào mừng",
        "total": "Tổng cộng",
        "active": "Hoạt động",
        "inactive": "Không hoạt động",
        "pending": "Chờ xử lý",
        "completed": "Hoàn thành",
        "cancelled": "Đã hủy",
        "from": "Từ",
        "to": "Đến",
        "distance": "Khoảng cách",
        "duration": "Thời gian",
        "unknown": "Không rõ",
        "all": "Tất cả",
        "searching": "Đang tìm kiếm...",
        "notAvailable": "Chưa có",
        "options": "Tùy chọn",
        "today": "Hôm nay",
        "noName": "(Không tên)",
        "notUpdated": "Chưa cập nhật",
        "noPhone": "Chưa cập nhật SĐT",
        "yes": "Có",
        "no": "Không",
        "characters": "ký tự",
        "ofTotal": "trong tổng số",
        "timeFilters": {
          "24h": "24 giờ",
          "7d": "7 ngày",
          "1month": "1 tháng"
        }
      },
      "auth": {
        "login": {
          "title": "Đăng nhập tài khoản",
          "subtitle": "Chào mừng trở lại! Vui lòng nhập thông tin đăng nhập",
          "email": "Email",
          "password": "Mật khẩu",
          "rememberMe": "Ghi nhớ đăng nhập",
          "forgotPassword": "Quên mật khẩu?",
          "loginButton": "Đăng nhập",
          "noAccount": "Chưa có tài khoản?",
          "signUp": "Đăng ký tại đây",
          "invalidCredentials": "Email hoặc mật khẩu không đúng",
          "loginSuccess": "Đăng nhập thành công!",
          "needHelp": "Cần hỗ trợ truy cập tài khoản?",
          "contactAdmin": "Liên hệ Quản trị viên"
        }
      },
      "validation": {
        "required": "Trường này là bắt buộc",
        "invalidEmail": "Vui lòng nhập email hợp lệ",
        "invalidPhone": "Vui lòng nhập số điện thoại hợp lệ",
        "passwordTooShort": "Mật khẩu phải có ít nhất 8 ký tự",
        "passwordsNotMatch": "Mật khẩu không khớp"
      },
      "notifications": {
        "loginSuccess": "Đăng nhập thành công!",
        "loginFailed": "Đăng nhập thất bại!",
        "saveSuccess": "Lưu thành công!",
        "deleteSuccess": "Xóa thành công!",
        "updateSuccess": "Cập nhật thành công!",
        "error": "Đã xảy ra lỗi",
        "networkError": "Lỗi kết nối mạng"
      },
      "orders": {
        "orderNumber": "Đơn hàng"
      },
      "dashboard": {
        "dispatcher": {
          "title": "Dashboard Quản lý đội xe",
          "subtitle": "Quản lý đơn hàng và hoạt động đội xe",
          "tabs": {
            "orders": "Đơn hàng",
            "resources": "Tài nguyên",
            "assignment": "Phân công"
          },
          "orders": {
            "title": "Danh sách đơn hàng",
            "overview": "Tổng quan đơn hàng",
            "totalShipments": "Tổng chuyến hàng",
            "totalVehicles": "Tổng phương tiện",
            "pendingPackages": "Đơn hàng chờ",
            "packagesDelivered": "Đơn hàng đã giao",
            "trackingTitle": "Theo dõi trực tiếp với vị trí thời gian thực",
            "enterOrderId": "Nhập ID đơn hàng...",
            "search": "Tìm kiếm",
            "clickToViewMap": "Nhấn vào đơn hàng để xem đường đi trên bản đồ",
            "selectOrderToViewMap": "Chọn một đơn hàng để xem lộ trình trên bản đồ",
            "customer": "Khách hàng",
            "driver": "Tài xế",
            "vehicle": "Xe",
            "notAssigned": "Chưa phân công",
            "orderNotFound": "Không tìm thấy đơn hàng với ID đã nhập",
            "searchError": "Lỗi khi tìm kiếm đơn hàng"
          },
            "vehicles": {
              "title": "Danh sách phương tiện",
              "licensePlate": "Biển số xe",
              "type": "Loại xe",
              "driver": "Tài xế",
              "status": "Trạng thái",
              "weight": "Trọng tải",
              "volume": "Thể tích",
              "maintenanceDate": "Ngày bảo trì",
              "notScheduled": "Chưa lên lịch",
              "available": "Sẵn sàng",
              "inUse": "Đang sử dụng",
              "maintenance": "Bảo trì",
              "maintenanceRequired": "Cần bảo trì",
              "searchPlaceholder": "Tìm kiếm theo biển số, loại xe hoặc tài xế...",
              "unassigned": "Chưa gán",
              "noDriverAssigned": "Chưa gán tài xế",
              "total": "Tổng",
              "assignDriver": "Gán tài xế",
              "assignDriverToVehicle": "Gán tài xế cho xe",
              "assignSuccess": "Gán tài xế thành công!",
              "assignError": "Gán tài xế thất bại",
              "selectDriver": "Chọn tài xế",
              "updated": "Cập nhật",
              "weightUnit": "tấn",
              "noVehiclesFound": "Không tìm thấy phương tiện",
              "noFilterResults": "Không có phương tiện nào phù hợp với bộ lọc",
              "noVehiclesInSystem": "Chưa có phương tiện nào trong hệ thống",
              "unassignSuccess": "Đã bỏ gán tài xế!",
              "confirmAssign": "Xác nhận gán",
              "assigning": "Đang gán..."
            },
          "drivers": {
            "title": "Danh sách tài xế",
            "name": "Tên",
            "phone": "Điện thoại",
            "status": "Trạng thái",
            "currentOrder": "Đơn hàng hiện tại",
            "rating": "Đánh giá",
            "searchPlaceholder": "Tìm kiếm theo tên, email hoặc số điện thoại...",
            "contactInfo": "Thông tin liên hệ",
            "noDriversFound": "Không tìm thấy tài xế",
            "noSearchResults": "Không có tài xế nào phù hợp với từ khóa tìm kiếm",
            "noDriversInSystem": "Chưa có tài xế nào trong hệ thống",
            "driverNotFound": "Không tìm thấy tài xế"
          },
            "assignment": {
              "title": "Quản lý phân công đơn hàng",
              "noOrders": "Không có đơn hàng chờ phân công",
              "products": "sản phẩm",
              "selectVehicle": "Chọn xe...",
              "assignVehicle": "Gán xe",
              "updateVehicle": "Cập nhật xe",
              "headers": {
                "orderId": "Mã đơn",
                "products": "Sản phẩm",
                "customer": "Khách hàng",
                "route": "Lộ trình",
                "orderDetails": "Chi tiết đơn hàng",
                "createdDate": "Ngày tạo",
                "vehicleDriver": "Xe & Tài xế",
                "actions": "Thao tác"
              },
              "edit": "Chỉnh sửa",
              "noPhone": "Chưa có SĐT",
              "unknownDriver": "Không rõ tài xế",
              "assigned": "Đã gán",
              "unassignVehicle": "Gỡ gán xe",
              "vehicleAssigned": "Đã gán xe {{vehicle}} thành công cho đơn hàng {{orderId}}! Theo dõi giao hàng được cập nhật tự động.",
              "vehicleUpdated": "Đã cập nhật xe {{vehicle}} thành công cho đơn hàng {{orderId}}! Theo dõi giao hàng được cập nhật tự động.",
              "vehicleUnassigned": "Đã gỡ gán xe thành công khỏi đơn hàng {{orderId}}!",
              "assignError": "Gán xe thất bại: ",
              "unassignError": "Gỡ gán xe thất bại: "
            },
          "pagination": {
            "showing": "Hiển thị {{start}}-{{end}} trong {{total}} đơn hàng",
            "page": "Trang {{current}} / {{total}}"
          }
        },
        "errors": {
          "notLoggedIn": "Chưa đăng nhập",
          "needLogin": "Bạn cần đăng nhập để truy cập dashboard!",
          "invalidRole": "Role không hợp lệ",
          "cannotDetermineAccess": "Không xác định được quyền truy cập của bạn!",
          "noPermission": "Không có quyền truy cập hoặc token hết hạn!",
          "loadingData": "Không thể tải dữ liệu",
          "refreshFailed": "Làm mới thất bại",
          "mapUnavailable": "Bản đồ tạm thời không khả dụng",
          "invalidApiResponse": "API không trả về dữ liệu người dùng hợp lệ",
          "fetchUsersFailed": "Không thể tải dữ liệu người dùng. Vui lòng thử lại hoặc kiểm tra backend.",
          "userNotFound": "Không tìm thấy người dùng",
          "unknownError": "Lỗi không xác định"
        },
        "fleet": {
          "title": "Dashboard Quản lý đội xe",
          "subtitle": "Quản lý phương tiện và bảo trì",
          "refreshVehicles": "Làm mới danh sách phương tiện",
          "tabs": {
            "vehicles": "Quản lý phương tiện",
            "maintenance": "Bảo trì xe",
            "schedule": "Lịch bảo trì"
          },
          "stats": {
            "totalVehicles": "Tổng phương tiện",
            "inUse": "Đang hoạt động",
            "maintenance": "Đang bảo trì",
            "needMaintenance": "Cần bảo trì"
          },
          "status": {
            "available": "Sẵn sàng sử dụng",
            "inUse": "Đang sử dụng",
            "underMaintenance": "Đang bảo trì",
            "needMaintenance": "Cần bảo trì"
          },
          "vehicleType": "Loại xe",
          "capacity": "Trọng tải",
          "volume": "Thể tích",
          "driver": "Tài xế",
          "notAssigned": "Chưa gán",
          "maintenanceHistory": "Lịch sử bảo trì",
          "scheduleMaintenance": "Lên lịch bảo trì",
          "selectVehicle": "Chọn phương tiện",
          "selectType": "Chọn loại",
          "maintenanceType": "Loại bảo trì",
          "workDescription": "Mô tả công việc",
          "workDescriptionPlaceholder": "Mô tả chi tiết công việc cần thực hiện",
          "implementationDate": "Ngày thực hiện",
          "estimatedCost": "Chi phí dự kiến",
          "nextMaintenance": "Bảo trì sắp tới",
          "type": "Loại",
          "maintenanceContent": "Nội dung bảo trì",
          "cost": "Chi phí",
          "maintenanceDate": "Thời gian bảo trì",
          "nextMaintenanceDate": "Lịch bảo trì sắp tới",
          "notes": "Ghi chú",
          "noNotes": "Không có",
          "loadingVehicles": "Đang tải danh sách xe...",
          "notesPlaceholder": "Nhập ghi chú (nếu có)",
          "maintenanceTypes": {
            "scheduled": "Bảo dưỡng định kỳ",
            "repair": "Sửa chữa",
            "emergency": "Sửa chữa khẩn cấp"
          },
          "maintenanceStatus": {
            "completed": "Hoàn thành",
            "inProgress": "Đang thực hiện",
            "pending": "Chờ xử lý"
          },
          "errors": {
            "loadMaintenanceHistory": "Không thể tải dữ liệu bảo trì.",
            "loadVehicles": "Không thể tải danh sách xe",
            "submitError": "Có lỗi xảy ra khi {{action}} xe. Vui lòng thử lại."
          },
          "maintenance": {
            "scheduleSuccess": "Đã lên lịch bảo trì thành công!",
            "scheduleError": "Lỗi khi lưu lịch bảo trì!",
            "overdue": "Quá hạn {{days}} ngày",
            "remaining": "Còn {{days}} ngày"
          },
          "vehicleTypes": {
            "truck": "Xe tải",
            "van": "Xe van",
            "motorcycle": "Xe máy",
            "car": "Xe con"
          },
          "emptyState": {
            "noResults": "Không tìm thấy phương tiện nào",
            "noVehicles": "Chưa có phương tiện nào",
            "tryFilters": "Thử thay đổi bộ lọc hoặc từ khóa tìm kiếm",
            "addFirst": "Thêm phương tiện đầu tiên vào hệ thống"
          },
          "form": {
            "validForm": "Form hợp lệ - sẵn sàng {{action}} phương tiện",
            "addVehicle": "Thêm phương tiện",
            "editVehicle": "Chỉnh sửa phương tiện",
            "addVehicleSubtitle": "Nhập thông tin phương tiện vào hệ thống",
            "updateVehicleSubtitle": "Cập nhật thông tin phương tiện {{plate}}",
            "licensePlate": "Biển số xe",
            "notesPlaceholder": "Nhập thông tin bổ sung về xe...",
            "addSuccess": "Thêm phương tiện thành công!",
            "updateSuccess": "Cập nhật phương tiện thành công!",
            "adding": "Đang thêm...",
            "updating": "Đang cập nhật..."
          },
          "search": {
            "title": "Quản lý phương tiện",
            "subtitle": "Tìm kiếm và lọc danh sách phương tiện", 
            "hideForm": "Ẩn form",
            "placeholder": "Tìm kiếm theo biển số, hãng xe, model, tài xế...",
            "allStatus": "Tất cả trạng thái"
          }
        },
        "admin": {
          "title": {
            "admin": "Dashboard Quản trị",
            "userManagement": "Dashboard Quản lý người dùng",
            "rolePermissions": "Dashboard Phân quyền", 
            "systemConfiguration": "Dashboard Cấu hình hệ thống",
            "systemLogs": "Dashboard Nhật ký hệ thống"
          },
          "subtitle": "Quản lý người dùng, phân quyền và cấu hình hệ thống",
          "tabs": {
            "users": "Người dùng",
            "roles": "Phân quyền",
            "settings": "Cài đặt",
            "logs": "Nhật ký"
          },
          "stats": {
            "totalUsers": "Tổng người dùng",
            "totalRoles": "Tổng vai trò",
            "systemConfig": "Cấu hình hệ thống",
            "auditEvents": "Sự kiện kiểm tra"
          },
          "settings": {
            "apiKeyPlaceholder": "Nhập API key",
            "urlPlaceholder": "URL endpoint"
          }
        },
        "operations": {
          "title": "Dashboard Quản lý vận hành",
          "subtitle": "Giám sát và quản lý các chỉ số vận hành",
          "tabs": {
            "overview": "Tổng quan",
            "performance": "Hiệu suất", 
            "monitoring": "Giám sát",
            "staff": "Quản lý nhân viên"
          },
          "overview": {
            "todaysOrders": "Đơn hàng hôm nay",
            "activeVehicles": "Xe đang hoạt động", 
            "todaysRevenue": "Doanh thu hôm nay",
            "completedOrders": "Đơn hàng hoàn thành",
            "comparedToYesterday": "so với hôm qua",
            "ofTotalVehicles": "trong tổng số xe"
          },
          "performance": {
            "loadingData": "Đang tải dữ liệu hiệu suất...",
            "metrics": {
              "deliverySuccessRate": "Tỷ lệ giao hàng thành công",
              "avgDeliveryTime": "Thời gian giao hàng trung bình",
              "transportationCost": "Chi phí vận chuyển/km",
              "totalKmTransported": "Tổng km đã vận chuyển"
            },
            "target": "Mục tiêu",
            "recentOrders": "Đơn hàng gần đây",
            "headers": {
              "orderId": "Mã đơn hàng",
              "customer": "Khách hàng",
              "route": "Tuyến đường",
              "createdTime": "Thời gian tạo",
              "status": "Trạng thái"
            }
          },
          "monitoring": {
            "totalVehicles": "Tổng số xe",
            "maintenanceRequests": "Yêu cầu bảo trì",
            "resourceDetails": "Chi tiết tài nguyên",
            "headers": {
              "vehicleName": "Tên xe",
              "type": "Loại",
              "driver": "Tài xế", 
              "status": "Trạng thái",
              "createdDate": "Ngày tạo"
            },
            "pagination": {
              "showing": "Hiển thị {{start}}-{{end}} trong {{total}} phương tiện"
            }
          },
          "status": {
            "processing": "Đang xử lý",
            "shipped": "Đang giao"
          },
          "pagination": {
            "showing": "Hiển thị {{start}}-{{end}} trong {{total}} đơn hàng",
            "previous": "Trước",
            "next": "Tiếp theo"
          },
          "staff": {
            "metrics": {
              "totalStaff": "Tổng nhân viên",
              "working": "Đang làm việc",
              "onLeave": "Đang nghỉ",
              "avgPerformance": "Hiệu suất TB"
            },
            "staffList": "Danh sách nhân viên",
            "departments": {
              "transportation": "Vận chuyển",
              "dispatch": "Điều phối",
              "maintenance": "Bảo trì"
            },
            "roles": {
              "admin": "Quản trị viên",
              "driver": "Tài xế", 
              "dispatcher": "Điều phối viên",
              "fleetManager": "Quản lý đội xe",
              "customer": "Khách hàng",
              "operationsManager": "Quản lý vận hành"
            },
            "status": {
              "working": "Đang làm việc",
              "onLeave": "Đang nghỉ",
              "sickLeave": "Nghỉ ốm",
              "terminated": "Đã nghỉ việc"
            },
            "headers": {
              "name": "Tên",
              "role": "Vai trò",
              "department": "Phòng ban",
              "status": "Trạng thái",
              "contact": "Liên hệ"
            }
          },
          "chart": {
            "monthlyRevenue": "Doanh thu hàng tháng",
            "totalAnnualRevenue": "Tổng doanh thu hàng năm",
            "growthComparedToLastMonth": "Tăng trưởng so với tháng trước",
            "monthlyAverage": "Trung bình hàng tháng",
            "month": "Tháng"
          }
        },
        "driver": {
          "title": "Dashboard Tài xế",
          "welcome": "Xin chào"
        },
        "search": {
          "placeholder": "Tìm kiếm tất cả...",
          "advancedSearch": "Tìm kiếm nâng cao",
          "noResults": "Không tìm thấy kết quả nào cho \"{{query}}\"",
          "suggestions": "Gợi ý:",
          "types": {
            "vehicle": "Phương tiện",
            "order": "Đơn hàng",
            "user": "Người dùng",
            "driver": "Tài xế"
          }
        }
      }
    }
  }
};

i18n
  .use(LanguageDetector)
  .use(initReactI18next)
  .init({
    resources,
    fallbackLng: 'en',
    debug: typeof process !== 'undefined' && process.env.NODE_ENV === 'development',

    interpolation: {
      escapeValue: false, // React already escapes values
    },

    detection: {
      order: ['localStorage', 'navigator', 'htmlTag'],
      caches: ['localStorage'],
    },
  });

export default i18n;
