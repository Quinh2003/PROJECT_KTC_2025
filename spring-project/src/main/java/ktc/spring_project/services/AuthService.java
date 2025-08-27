package ktc.spring_project.services;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;
import ktc.spring_project.entities.User;
import ktc.spring_project.repositories.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

@Service
public class AuthService {

    @Value("${jwt.secret:defaultSecretKey}")
    private String jwtSecret;

    @Value("${jwt.expiration:86400000}")
    private long jwtExpiration;

    @Value("${jwt.refresh-expiration:604800000}")
    private long refreshExpiration;

    @Autowired
    private AuthenticationManager authenticationManager;

    @Autowired
    private CustomUserDetailsService userDetailsService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private PasswordEncoder passwordEncoder;

    @Autowired
    private ActivityLogService activityLogService;

    public Map<String, Object> authenticate(String email, String password) {
        // Tìm user theo email
        User user = userRepository.findByEmail(email)
                .orElseThrow(() -> new RuntimeException("User not found with email: " + email));

        // Thực hiện authentication
        Authentication authentication = authenticationManager.authenticate(
                new UsernamePasswordAuthenticationToken(user.getUsername(), password)
        );
        SecurityContextHolder.getContext().setAuthentication(authentication);

        // Tạo các token JWT
        UserDetails userDetails = userDetailsService.loadUserByUsername(user.getUsername());
        String jwt = generateToken(userDetails);
        String refreshToken = generateRefreshToken(userDetails);

        // Ghi log hoạt động
        activityLogService.logUserActivity(user.getId(), "LOGIN", "User logged in successfully");

        // Trả về thông tin đăng nhập
        Map<String, Object> response = new HashMap<>();
        response.put("token", jwt);
        response.put("refreshToken", refreshToken);
        response.put("user", mapUserToDto(user));
        return response;
    }

    public void logout(Authentication authentication) {
        if (authentication != null) {
            UserDetails userDetails = (UserDetails) authentication.getPrincipal();
            Optional<User> userOpt = userRepository.findByUsername(userDetails.getUsername());

            if (userOpt.isPresent()) {
                User user = userOpt.get();
                activityLogService.logUserActivity(user.getId(), "LOGOUT", "User logged out");
            }
        }
    }

    public Map<String, Object> refreshToken(String refreshToken) {
        // Validate refreshToken
        String username = extractUsername(refreshToken);
        UserDetails userDetails = userDetailsService.loadUserByUsername(username);

        if (!isTokenValid(refreshToken, userDetails)) {
            throw new RuntimeException("Invalid refresh token");
        }

        // Generate new tokens
        String newToken = generateToken(userDetails);
        String newRefreshToken = generateRefreshToken(userDetails);

        // Get user info
        User user = userRepository.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("User not found"));

        // Return response
        Map<String, Object> response = new HashMap<>();
        response.put("token", newToken);
        response.put("refreshToken", newRefreshToken);
        response.put("user", mapUserToDto(user));
        return response;
    }

    public void sendPasswordResetEmail(String email) {
        User user = userRepository.findByEmail(email)
                .orElseThrow(() -> new RuntimeException("User not found with email: " + email));

        // Generate password reset token (implementation can be extended)
        String resetToken = generatePasswordResetToken(user);

        // Send email logic would be implemented here

        activityLogService.logUserActivity(user.getId(), "PASSWORD_RESET_REQUEST",
                "Password reset requested for email: " + email);
    }

    public void resetPassword(String token, String newPassword) {
        // Validate token and get username
        String username = extractUsernameFromResetToken(token);

        User user = userRepository.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("Invalid reset token"));

        // Update password
        user.setPassword(passwordEncoder.encode(newPassword));
        userRepository.save(user);

        activityLogService.logUserActivity(user.getId(), "PASSWORD_RESET",
                "Password reset successfully");
    }

    public void changePassword(String currentPassword, String newPassword, Authentication authentication) {
        UserDetails userDetails = (UserDetails) authentication.getPrincipal();
        User user = userRepository.findByUsername(userDetails.getUsername())
                .orElseThrow(() -> new RuntimeException("User not found"));

        // Verify current password
        if (!passwordEncoder.matches(currentPassword, user.getPassword())) {
            throw new RuntimeException("Current password is incorrect");
        }

        // Update to new password
        user.setPassword(passwordEncoder.encode(newPassword));
        userRepository.save(user);

        activityLogService.logUserActivity(user.getId(), "PASSWORD_CHANGE",
                "Password changed successfully");
    }

    public void verifyEmail(String token) {
        // Implementation would verify an email verification token
        // This is a simplified version
        String username = extractUsernameFromVerificationToken(token);

        User user = userRepository.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("Invalid verification token"));

        // Đánh dấu là đã xác minh email - thay đổi trạng thái của người dùng
        // Vì không có trường emailVerified nên chúng ta có thể cập nhật trạng thái
        // của người dùng thành "ACTIVE" hoặc một trạng thái tương tự
        if (user.getStatus() != null) {
            // Giả sử có một Status có tên là "ACTIVE" hoặc tương tự
            // user.setStatus(activeStatus);
            userRepository.save(user);
        }

        activityLogService.logUserActivity(user.getId(), "EMAIL_VERIFIED",
                "Email verified successfully");
    }

    public Map<String, Object> getCurrentUserInfo(Authentication authentication) {
        UserDetails userDetails = (UserDetails) authentication.getPrincipal();
        User user = userRepository.findByUsername(userDetails.getUsername())
                .orElseThrow(() -> new RuntimeException("User not found"));

        return mapUserToDto(user);
    }

    // JWT Token generation and validation methods

    private String generateToken(UserDetails userDetails) {
        Map<String, Object> claims = new HashMap<>();
        return createToken(claims, userDetails.getUsername(), jwtExpiration);
    }

    private String generateRefreshToken(UserDetails userDetails) {
        Map<String, Object> claims = new HashMap<>();
        return createToken(claims, userDetails.getUsername(), refreshExpiration);
    }

    private String generatePasswordResetToken(User user) {
        Map<String, Object> claims = new HashMap<>();
        claims.put("purpose", "password_reset");
        return createToken(claims, user.getUsername(), 3600000); // 1 hour expiration
    }

    private String createToken(Map<String, Object> claims, String subject, long expiration) {
        Date now = new Date();
        Date expiryDate = new Date(now.getTime() + expiration);

        Key key = Keys.hmacShaKeyFor(jwtSecret.getBytes(StandardCharsets.UTF_8));

        return Jwts.builder()
                .setClaims(claims)
                .setSubject(subject)
                .setIssuedAt(now)
                .setExpiration(expiryDate)
                .signWith(key, SignatureAlgorithm.HS512)
                .compact();
    }

    public String extractUsername(String token) {
        return extractClaim(token, Claims::getSubject);
    }

    public Date extractExpiration(String token) {
        return extractClaim(token, Claims::getExpiration);
    }

    public <T> T extractClaim(String token, Function<Claims, T> claimsResolver) {
        final Claims claims = extractAllClaims(token);
        return claimsResolver.apply(claims);
    }

    private Claims extractAllClaims(String token) {
        Key key = Keys.hmacShaKeyFor(jwtSecret.getBytes(StandardCharsets.UTF_8));
        return Jwts.parserBuilder()
                .setSigningKey(key)
                .build()
                .parseClaimsJws(token)
                .getBody();
    }

    private Boolean isTokenExpired(String token) {
        return extractExpiration(token).before(new Date());
    }

    public Boolean isTokenValid(String token, UserDetails userDetails) {
        final String username = extractUsername(token);
        return (username.equals(userDetails.getUsername()) && !isTokenExpired(token));
    }

    private String extractUsernameFromResetToken(String token) {
        Claims claims = extractAllClaims(token);
        if (!"password_reset".equals(claims.get("purpose"))) {
            throw new RuntimeException("Invalid token purpose");
        }
        return claims.getSubject();
    }

    private String extractUsernameFromVerificationToken(String token) {
        Claims claims = extractAllClaims(token);
        if (!"email_verification".equals(claims.get("purpose"))) {
            throw new RuntimeException("Invalid token purpose");
        }
        return claims.getSubject();
    }

    // Helper method to map User entity to DTO
    private Map<String, Object> mapUserToDto(User user) {
        Map<String, Object> userDto = new HashMap<>();
        userDto.put("id", user.getId());
        userDto.put("username", user.getUsername());
        userDto.put("email", user.getEmail());
        userDto.put("fullName", user.getFullName());
        userDto.put("role", user.getRole().getRoleName());
        return userDto;
    }

    /**
     * Generate JWT token for a specific user (used for Google Login)
     */
    public Map<String, Object> generateTokenForUser(User user) {
        // Load user details for token generation
        UserDetails userDetails = userDetailsService.loadUserByUsername(user.getUsername());

        // Generate JWT and refresh tokens
        String jwt = generateToken(userDetails);
        String refreshToken = generateRefreshToken(userDetails);

        // Log activity
    activityLogService.logUserActivity(user.getId(), "LOGIN", "User logged in via Google");

        // Return token response
        Map<String, Object> response = new HashMap<>();
        response.put("token", jwt);
        response.put("refreshToken", refreshToken);
        response.put("user", mapUserToDto(user));
        response.put("tokenType", "Bearer");
        return response;
    }
}
