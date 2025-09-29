// Service để fetch dữ liệu địa chỉ từ API
export interface Province {
  code: string;
  name: string;
}

export interface District {
  code: string;
  name: string;
  province_code: string;
}

export interface Ward {
  code: string;
  name: string;
  district_code: string;
}


const API_BASE_URL = "https://provinces.open-api.vn/api";

export const addressService = {
  // Lấy danh sách tỉnh/thành phố
  async getProvinces(): Promise<Province[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/p/`);
      if (!response.ok) {
        throw new Error("Failed to fetch provinces");
      }
      const data = await response.json();
      return data.map((item: { code: number; name: string }) => ({
        code: item.code.toString(),
        name: item.name,
      }));
    } catch (error) {
      console.error("Error fetching provinces:", error);
      // Fallback data nếu API lỗi
      return [
        { code: "01", name: "Hà Nội" },
        { code: "79", name: "TP. Hồ Chí Minh" },
        { code: "48", name: "Đà Nẵng" },
        { code: "31", name: "Hải Phòng" },
        { code: "92", name: "Cần Thơ" },
      ];
    }
  },

  // Lấy danh sách quận/huyện theo tỉnh
  async getDistricts(provinceCode: string): Promise<District[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/p/${provinceCode}?depth=2`);
      if (!response.ok) {
        throw new Error("Failed to fetch districts");
      }
      const data = await response.json();
      return (
        data.districts?.map((item: { code: number; name: string }) => ({
          code: item.code.toString(),
          name: item.name,
          province_code: provinceCode,
        })) || []
      );
    } catch (error) {
      console.error("Error fetching districts:", error);
      return [];
    }
  },

  // Lấy danh sách phường/xã theo quận/huyện
  async getWards(districtCode: string): Promise<Ward[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/d/${districtCode}?depth=2`);
      if (!response.ok) {
        throw new Error("Failed to fetch wards");
      }
      const data = await response.json();
      return (
        data.wards?.map((item: { code: number; name: string }) => ({
          code: item.code.toString(),
          name: item.name,
          district_code: districtCode,
        })) || []
      );
    } catch (error) {
      console.error("Error fetching wards:", error);
      return [];
    }
  },
};
