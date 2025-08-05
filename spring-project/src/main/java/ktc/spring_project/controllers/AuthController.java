package ktc.spring_project.controllers;

import ktc.spring_project.services.AuthService;
import ktc.spring_project.services.UserService;
import ktc.spring_project.dtos.auth.GoogleLoginRequestDto;
import ktc.spring_project.dtos.auth.GoogleLoginWithCredentialRequestDto;
import ktc.spring_project.entities.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.Map;

/**
 * Controller responsible for authentication
 * Based on user stories:
 * - US-AUTH-LOGIN-01: Login with Email & Password
 * - US-AUTH-FORGOT-01: Password Recovery
 */
@RestController
@RequestMapping("/api/auth")
public class AuthController {

    @Autowired
    private AuthService authService;

    @Autowired
    private UserService userService;

    /**
     * User login
     * US-AUTH-LOGIN-01
     */
    @PostMapping("/login")
    public ResponseEntity<Map<String, Object>> login(
            @Valid @RequestBody Map<String, String> credentials) {

        String email = credentials.get("email");
        String password = credentials.get("password");

        Map<String, Object> response = authService.authenticate(email, password);
        return ResponseEntity.ok(response);
    }

    /**
     * User logout
     */
    @PostMapping("/logout")
    public ResponseEntity<Map<String, String>> logout(Authentication authentication) {
        authService.logout(authentication);
        return ResponseEntity.ok(Map.of("message", "Logged out successfully"));
    }

    /**
     * Refresh token
     */
    @PostMapping("/refresh")
    public ResponseEntity<Map<String, Object>> refreshToken(
            @RequestBody Map<String, String> tokenData) {

        String refreshToken = tokenData.get("refreshToken");
        Map<String, Object> response = authService.refreshToken(refreshToken);
        return ResponseEntity.ok(response);
    }

    /**
     * Forgot password - send reset email
     * US-AUTH-FORGOT-01
     */
    @PostMapping("/forgot-password")
    public ResponseEntity<Map<String, String>> forgotPassword(
            @RequestBody Map<String, String> requestData) {

        String email = requestData.get("email");
        authService.sendPasswordResetEmail(email);

        return ResponseEntity.ok(Map.of("message", "Password reset email sent"));
    }

    /**
     * Reset password with token
     * US-AUTH-FORGOT-01
     */
    @PostMapping("/reset-password")
    public ResponseEntity<Map<String, String>> resetPassword(
            @Valid @RequestBody Map<String, String> resetData) {

        String token = resetData.get("token");
        String newPassword = resetData.get("newPassword");

        authService.resetPassword(token, newPassword);

        return ResponseEntity.ok(Map.of("message", "Password reset successfully"));
    }

    /**
     * Change password for authenticated user
     */
    @PostMapping("/change-password")
    public ResponseEntity<Map<String, String>> changePassword(
            @Valid @RequestBody Map<String, String> passwordData,
            Authentication authentication) {

        String currentPassword = passwordData.get("currentPassword");
        String newPassword = passwordData.get("newPassword");

        authService.changePassword(currentPassword, newPassword, authentication);

        return ResponseEntity.ok(Map.of("message", "Password changed successfully"));
    }

    /**
     * Verify email token
     */
    @PostMapping("/verify-email")
    public ResponseEntity<Map<String, String>> verifyEmail(
            @RequestBody Map<String, String> tokenData) {

        String token = tokenData.get("token");
        authService.verifyEmail(token);

        return ResponseEntity.ok(Map.of("message", "Email verified successfully"));
    }

    /**
     * Get current user information
     */
    @GetMapping("/me")
    public ResponseEntity<Map<String, Object>> getCurrentUser(Authentication authentication) {
        Map<String, Object> userInfo = authService.getCurrentUserInfo(authentication);
        return ResponseEntity.ok(userInfo);
    }

    /**
     * Google Login with Access Token
     * Validates Google access token and authenticates/creates user
     */
    @PostMapping("/google-login")
    public ResponseEntity<Map<String, Object>> googleLogin(
            @Valid @RequestBody GoogleLoginRequestDto request) {

        try {
            // Authenticate user via Google
            User user = userService.googleLogin(request);

            // Generate JWT token for the user
            Map<String, Object> response = authService.generateTokenForUser(user);

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
                    .body(Map.of("error", "Google login failed", "message", e.getMessage()));
        }
    }

    /**
     * Google Login with Credential (JWT from Google)
     * Validates Google credential and authenticates/creates user
     */
    @PostMapping("/google-login-credential")
    public ResponseEntity<Map<String, Object>> googleLoginWithCredential(
            @Valid @RequestBody GoogleLoginWithCredentialRequestDto request) {

        try {
            // Authenticate user via Google credential
            User user = userService.googleLoginWithCredential(request);

            // Generate JWT token for the user
            Map<String, Object> response = authService.generateTokenForUser(user);

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
                    .body(Map.of("error", "Google credential login failed", "message", e.getMessage()));
        }
    }
}
