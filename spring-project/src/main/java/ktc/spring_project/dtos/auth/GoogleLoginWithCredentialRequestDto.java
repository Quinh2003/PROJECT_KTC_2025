package ktc.spring_project.dtos.auth;

import jakarta.validation.constraints.NotBlank;

/**
 * Google Login With Credential Request DTO - Data Transfer Object cho Google Login với JWT Credential
 *
 * DTO này được sử dụng khi frontend sử dụng Google One Tap sign-in hoặc Google Sign-In JavaScript library
 * và nhận được JWT credential từ Google, sau đó gửi credential này đến backend để authenticate user.
 *
 * Khác biệt với GoogleLoginRequestDto:
 * - GoogleLoginRequestDto: Sử dụng access token từ OAuth 2.0 flow
 * - GoogleLoginWithCredentialRequestDto: Sử dụng JWT credential từ Google One Tap
 *
 * Flow:
 * 1. Frontend tích hợp Google One Tap hoặc Google Sign-In button
 * 2. User click sign-in với Google
 * 3. Google trả về JWT credential cho frontend (không cần redirect)
 * 4. Frontend gửi request này với credential đến backend
 * 5. Backend validate JWT credential và extract thông tin user
 * 6. Backend tạo/authenticate user và trả về JWT token
 */
public class GoogleLoginWithCredentialRequestDto {

    /**
     * Google JWT credential từ One Tap sign-in hoặc Sign-In JavaScript library
     * Đây là một JWT token được Google ký, chứa thông tin user như email, name, picture, etc.
     * Backend cần validate JWT này với Google để đảm bảo tính xác thực
     */
    @NotBlank(message = "Google credential is required")
    private String credential;

    /**
     * Google Client ID của ứng dụng (optional)
     * Dùng để validate credential có được tạo cho đúng ứng dụng này không
     * Tăng cường bảo mật để tránh credential được sử dụng cho ứng dụng khác
     */
    private String clientId;

    /**
     * Thông tin thiết bị của user (optional)
     * Có thể chứa thông tin như: browser, OS, IP address, etc.
     * Dùng để tracking và security monitoring
     */
    private String deviceInfo;

    // Default constructor - cần thiết cho JSON deserialization
    public GoogleLoginWithCredentialRequestDto() {}

    /**
     * Constructor với tất cả parameters
     *
     * @param credential Google JWT credential
     * @param clientId Google Client ID
     * @param deviceInfo Thông tin thiết bị
     */
    public GoogleLoginWithCredentialRequestDto(String credential, String clientId, String deviceInfo) {
        this.credential = credential;
        this.clientId = clientId;
        this.deviceInfo = deviceInfo;
    }

    // Getter cho credential
    public String getCredential() {
        return credential;
    }

    // Setter cho credential
    public void setCredential(String credential) {
        this.credential = credential;
    }

    // Getter cho clientId
    public String getClientId() {
        return clientId;
    }

    // Setter cho clientId
    public void setClientId(String clientId) {
        this.clientId = clientId;
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
