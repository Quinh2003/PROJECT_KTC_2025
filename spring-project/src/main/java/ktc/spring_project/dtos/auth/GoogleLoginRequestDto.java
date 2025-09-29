package ktc.spring_project.dtos.auth;

import jakarta.validation.constraints.NotBlank;

/**
 * Google Login Request DTO - Data Transfer Object cho Google Login với Access Token
 *
 * DTO này được sử dụng khi frontend gọi Google OAuth 2.0 và nhận được access token,
 * sau đó gửi access token này đến backend để authenticate user.
 *
 * Flow:
 * 1. Frontend redirect user đến Google OAuth
 * 2. User đăng nhập với Google
 * 3. Google trả về access token cho frontend
 * 4. Frontend gửi request này với access token đến backend
 * 5. Backend dùng access token để lấy thông tin user từ Google API
 * 6. Backend tạo/authenticate user và trả về JWT token
 */
public class GoogleLoginRequestDto {

    /**
     * Google access token từ OAuth 2.0 flow
     * Token này được Google cấp sau khi user đăng nhập thành công
     * Backend sẽ dùng token này để gọi Google API lấy thông tin user
     */
    @NotBlank(message = "Google access token is required")
    private String accessToken;

    /**
     * Thông tin thiết bị của user (optional)
     * Có thể chứa thông tin như: browser, OS, IP address, etc.
     * Dùng để tracking và security monitoring
     */
    private String deviceInfo;

    // Default constructor - cần thiết cho JSON deserialization
    public GoogleLoginRequestDto() {}

    /**
     * Constructor với tất cả parameters
     *
     * @param accessToken Google access token
     * @param deviceInfo Thông tin thiết bị
     */
    public GoogleLoginRequestDto(String accessToken, String deviceInfo) {
        this.accessToken = accessToken;
        this.deviceInfo = deviceInfo;
    }

    // Getter cho accessToken
    public String getAccessToken() {
        return accessToken;
    }

    // Setter cho accessToken
    public void setAccessToken(String accessToken) {
        this.accessToken = accessToken;
    }

    // Getter cho deviceInfo
    public String getDeviceInfo() {
        return deviceInfo;
    }

    // Setter cho deviceInfo
    public void setDeviceInfo(String deviceInfo) {
        this.deviceInfo = deviceInfo;
    }
}
