/**
 * Validation utilities cho địa chỉ Đà Nẵng
 * CHỈ CHO PHÉP ĐỊA CHỈ TRONG NỘI THÀNH ĐÀ NẴNG
 */

// Danh sách các tên tỉnh/thành phố được phép (Đà Nẵng)
const ALLOWED_PROVINCES = [
  "Đà Nẵng",
  "Da Nang", 
  "Thành phố Đà Nẵng",
  "TP. Đà Nẵng",
  "TP Đà Nẵng",
  "Da Nang City",
  "Danang",
  "DN"
];

// Danh sách các quận/huyện trong Đà Nẵng
const DA_NANG_DISTRICTS = [
  "Hải Châu",
  "Thanh Khê", 
  "Sơn Trà",
  "Ngũ Hành Sơn",
  "Liên Chiểu",
  "Cẩm Lệ",
  "Hòa Vang",
  "Hai Chau",
  "Thanh Khe",
  "Son Tra", 
  "Ngu Hanh Son",
  "Lien Chieu",
  "Cam Le",
  "Hoa Vang"
];

/**
 * Kiểm tra xem tỉnh/thành phố có phải là Đà Nẵng không
 */
export const isValidDaNangProvince = (provinceName: string): boolean => {
  if (!provinceName) return false;
  
  const normalizedProvince = provinceName.trim().toLowerCase();
  return ALLOWED_PROVINCES.some(allowed => 
    normalizedProvince.includes(allowed.toLowerCase()) ||
    allowed.toLowerCase().includes(normalizedProvince)
  );
};

/**
 * Kiểm tra xem quận/huyện có thuộc Đà Nẵng không
 */
export const isValidDaNangDistrict = (districtName: string): boolean => {
  if (!districtName) return false;
  
  const normalizedDistrict = districtName.trim().toLowerCase();
  return DA_NANG_DISTRICTS.some(allowed => 
    normalizedDistrict.includes(allowed.toLowerCase()) ||
    allowed.toLowerCase().includes(normalizedDistrict)
  );
};

/**
 * Kiểm tra địa chỉ có hợp lệ cho Đà Nẵng không
 */
export const validateDaNangAddress = (address: {
  city?: string;
  province?: string;
  district?: string;
  fullAddress?: string;
}): { isValid: boolean; message?: string } => {
  
  // Kiểm tra city/province
  const cityToCheck = address.city || address.province || "";
  if (!isValidDaNangProvince(cityToCheck)) {
    return {
      isValid: false,
      message: "Chúng tôi chỉ phục vụ giao hàng trong nội thành Đà Nẵng. Vui lòng chọn địa chỉ giao hàng tại Đà Nẵng."
    };
  }
  
  // Kiểm tra district nếu có
  if (address.district && !isValidDaNangDistrict(address.district)) {
    return {
      isValid: false,
      message: "Quận/huyện không thuộc Đà Nẵng. Vui lòng chọn quận/huyện trong nội thành Đà Nẵng."
    };
  }
  
  // Kiểm tra fullAddress nếu có
  if (address.fullAddress) {
    const fullAddressLower = address.fullAddress.toLowerCase();
    
    // Kiểm tra xem có chứa tên Đà Nẵng không
    const containsDaNang = ALLOWED_PROVINCES.some(province => 
      fullAddressLower.includes(province.toLowerCase())
    );
    
    if (!containsDaNang) {
      return {
        isValid: false,
        message: "Địa chỉ phải nằm trong nội thành Đà Nẵng. Chúng tôi hiện chỉ phục vụ giao hàng tại Đà Nẵng."
      };
    }
  }
  
  return { isValid: true };
};

/**
 * Custom validation rule cho Ant Design Form
 */
export const daNangAddressRule = {
  validator: (_: unknown, value: string) => {
    if (!value) {
      return Promise.reject(new Error('Vui lòng nhập địa chỉ'));
    }
    
    const validation = validateDaNangAddress({ fullAddress: value });
    if (!validation.isValid) {
      return Promise.reject(new Error(validation.message));
    }
    
    return Promise.resolve();
  },
};

/**
 * Custom validation rule cho province/city
 */
export const daNangProvinceRule = {
  validator: (_: unknown, value: string) => {
    if (!value) {
      return Promise.reject(new Error('Vui lòng chọn tỉnh/thành phố'));
    }
    
    if (!isValidDaNangProvince(value)) {
      return Promise.reject(new Error('Chúng tôi chỉ phục vụ giao hàng trong nội thành Đà Nẵng'));
    }
    
    return Promise.resolve();
  },
};

/**
 * Lấy tọa độ mặc định cho Đà Nẵng
 */
export const getDaNangDefaultCoordinates = () => ({
  latitude: 16.0544068,
  longitude: 108.2021667
});

/**
 * Thông báo lỗi khi địa chỉ không hợp lệ
 */
export const DA_NANG_ONLY_MESSAGE = 
  "🏍️ Chúng tôi hiện chỉ phục vụ giao hàng trong nội thành Đà Nẵng. " +
  "Vui lòng chọn địa chỉ giao hàng tại Đà Nẵng để sử dụng dịch vụ.";

export const DA_NANG_DISTRICTS_LIST = DA_NANG_DISTRICTS;
export const DA_NANG_PROVINCES_LIST = ALLOWED_PROVINCES;
