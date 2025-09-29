package ktc.spring_project.services;
import ktc.spring_project.exceptions.HttpException;
import ktc.spring_project.exceptions.EntityDuplicateException;

import ktc.spring_project.entities.User;
import ktc.spring_project.entities.Role;
import ktc.spring_project.repositories.RoleRepository;
import ktc.spring_project.repositories.UserJpaRepository;
import ktc.spring_project.dtos.auth.GoogleLoginRequestDto;
import ktc.spring_project.dtos.auth.GoogleLoginWithCredentialRequestDto;
import jakarta.persistence.EntityNotFoundException;
import ktc.spring_project.exceptions.HttpException;
import org.springframework.http.HttpStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.mail.javamail.JavaMailSender;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.Map;
import java.lang.reflect.Field;

import org.springframework.util.ReflectionUtils;

@Service
public class UserService {
    // Simple OTP cache: email -> OTP, expire in 5 minutes (demo, nên dùng Redis hoặc DB cho thực tế)
    public static final java.util.concurrent.ConcurrentHashMap<String, OtpEntry> otpCache = new java.util.concurrent.ConcurrentHashMap<>();

    public static class OtpEntry {
    public String otp;
    public long expireTime;
        OtpEntry(String otp, long expireTime) {
            this.otp = otp;
            this.expireTime = expireTime;
        }
    }

    @Autowired
    private UserJpaRepository userRepository;

    @Autowired
    private RoleRepository roleRepository;

    @Autowired
    private ktc.spring_project.repositories.StoreRepository storeRepository;

    @Autowired
    private RestTemplate restTemplate;

    @Autowired
    private PasswordEncoder passwordEncoder;

    @Autowired
    private JavaMailSender mailSender;

    public User createUser(User user) {
    // Log bảo mật giữ lại nếu cần
        
        // Check if email already exists
        if (userRepository.findByEmail(user.getEmail()).isPresent()) {
            throw new EntityDuplicateException("Email");
        }
        
        // Gán username nếu chưa có (dùng email), chỉ kiểm tra trùng lặp
        if (user.getUsername() == null || user.getUsername().trim().isEmpty()) {
            String baseUsername = user.getEmail();
            String username = baseUsername;
            int counter = 1;
            while (userRepository.findByUsername(username).isPresent()) {
                username = baseUsername + "_" + counter;
                counter++;
            }
            user.setUsername(username);
            System.out.println("==> Đã gán username: " + username);
        } else {
            // Check if provided username already exists
            if (userRepository.findByUsername(user.getUsername()).isPresent()) {
                throw new EntityDuplicateException("Username đã được sử dụng");
            }
        }
        
        // Gán role mặc định nếu chưa có
        if (user.getRole() == null) {
            Role customerRole = roleRepository.findByRoleName("CUSTOMER")
                .orElseGet(() -> roleRepository.findByRoleName("USER")
                    .orElseThrow(() -> new RuntimeException("No default role found")));
            user.setRole(customerRole);
            System.out.println("==> Đã gán role: " + customerRole.getRoleName());
        }
        
        // Encode password
    String rawPassword = user.getPassword();
    // Log bảo mật: chỉ giữ lại nếu cần debug
    String encodedPassword = passwordEncoder.encode(rawPassword);
    user.setPassword(encodedPassword);
        
        User savedUser = userRepository.save(user);
        
        return savedUser;
    }

        // Hàm gửi email OTP/TOTP
        public void sendOtpEmail(String toEmail, String otpOrSecret) {
            org.springframework.mail.SimpleMailMessage message = new org.springframework.mail.SimpleMailMessage();
            message.setTo(toEmail);
            message.setSubject("Mã OTP xác thực đăng nhập");
            message.setText(otpOrSecret);
            mailSender.send(message);
        }

    public User getUserById(Long id) {
    return userRepository.findById(id)
        .orElseThrow(() -> new ktc.spring_project.exceptions.EntityNotFoundException("User not found with id: " + id));
    }

    public User updateUser(Long id, User userDetails) {
        User user = getUserById(id);
        // Chỉ cập nhật các trường không null từ userDetails
        if (userDetails.getUsername() != null) {
            user.setUsername(userDetails.getUsername());
        }
        if (userDetails.getEmail() != null) {
            user.setEmail(userDetails.getEmail());
        }
        if (userDetails.getPassword() != null) {
                user.setPassword(passwordEncoder.encode(userDetails.getPassword()));
        }
        if (userDetails.getFullName() != null) {
            user.setFullName(userDetails.getFullName());
        }
        if (userDetails.getPhone() != null) {
            user.setPhone(userDetails.getPhone());
        }
        if (userDetails.getRole() != null) {
            user.setRole(userDetails.getRole());
        }
        if (userDetails.getStatus() != null) {
            user.setStatus(userDetails.getStatus());
        }
        if (userDetails.getNotes() != null) {
            user.setNotes(userDetails.getNotes());
        }
        return userRepository.save(user);
    }

    public void deleteUser(Long id) {
User user = getUserById(id);
        userRepository.delete(user);
    }

    public List<User> getFilteredUsers(String role, String status, String search) {
        if (role == null && status == null && search == null) {
            return getAllUsers();
        }

        List<User> filteredUsers = new ArrayList<>();

        if (role != null && !role.isEmpty()) {
            return userRepository.findByRoleName(role);
        }

        if (search != null && !search.isEmpty()) {
            Optional<User> userOpt = userRepository.findByUsernameOrEmail(search, search);
            return userOpt.map(List::of).orElseGet(ArrayList::new);
        }

        return filteredUsers;
    }

    public User getCurrentUser(Authentication authentication) {
        if (authentication == null) {
            throw new HttpException("No authentication provided", org.springframework.http.HttpStatus.UNAUTHORIZED);
        }

        UserDetails userDetails = (UserDetails) authentication.getPrincipal();
        return userRepository.findByUsername(userDetails.getUsername())
                .orElseThrow(() -> new EntityNotFoundException("User not found"));
    }

    public User updatePartial(Long id, Map<String, Object> updates) {
        User user = userRepository.findById(id)
            .orElseThrow(() -> new EntityNotFoundException("User not found"));

        updates.forEach((key, value) -> {
            Field field = ReflectionUtils.findField(User.class, key);
            if (field != null) {
                field.setAccessible(true);
                if ("password".equals(key) && value instanceof String) {
                    String encodedPassword = passwordEncoder.encode((String) value);
                    ReflectionUtils.setField(field, user, encodedPassword);
                } else {
                    ReflectionUtils.setField(field, user, value);
                }
            }
        });

        return userRepository.save(user);
    }

    public List<User> getAllUsers() {
        return userRepository.findAll();
    }

    /**
     * Tìm user theo email
     */
    public User findByEmail(String email) {
        return userRepository.findByEmail(email)
                .orElseThrow(() -> new EntityNotFoundException("User not found with email: " + email));
    }

    /**
     * Google Login với Access Token
     */
    public User googleLogin(GoogleLoginRequestDto request) {
        try {
            String googleApiUrl = "https://www.googleapis.com/oauth2/v2/userinfo?access_token=" + request.getAccessToken();

            HttpHeaders headers = new HttpHeaders();
            headers.set("Authorization", "Bearer " + request.getAccessToken());
            HttpEntity<String> entity = new HttpEntity<>(headers);

            ResponseEntity<Map> response = restTemplate.exchange(
                googleApiUrl,
                HttpMethod.GET,
                entity,
                Map.class
            );

            @SuppressWarnings("unchecked")
            Map<String, Object> userInfo = (Map<String, Object>) response.getBody();
            if (userInfo == null) {
                throw new HttpException("Google API returned null response", org.springframework.http.HttpStatus.BAD_REQUEST);
            }

            String email = (String) userInfo.get("email");
            String name = (String) userInfo.get("name");
            String googleId = (String) userInfo.get("id");

            if (email == null || name == null || googleId == null) {
                throw new HttpException("Missing required fields from Google API response", org.springframework.http.HttpStatus.BAD_REQUEST);
            }
        // Sinh mã OTP 6 số cho xác thực qua email
        String otp = String.valueOf(100000 + (int)(Math.random() * 900000));
        // Lưu OTP vào cache với email và thời hạn 5 phút
        otpCache.put(email, new OtpEntry(otp, System.currentTimeMillis() + 5 * 60 * 1000));
        String otpEmailContent = "Chào mừng bạn đăng nhập FastRoute!\n\n"
            + "Mã OTP xác thực của bạn là: " + otp + "\n\n"
            + "Vui lòng nhập mã này vào hệ thống để hoàn tất xác thực đăng nhập.";
        sendOtpEmail(email, otpEmailContent);
        Optional<User> existingUser = userRepository.findByEmail(email);

            Role customerRole = roleRepository.findByRoleName("CUSTOMER")
                .orElseThrow(() -> new RuntimeException("Role CUSTOMER not found"));

            if (existingUser.isPresent()) {
                User user = existingUser.get();
                if (user.getGoogleId() == null) {
                    user.setGoogleId(googleId);
                }
                // Luôn gán role CUSTOMER khi đăng nhập Google
                user.setRole(customerRole);
                userRepository.save(user);
                return user;
            } else {
                // Tạo mới user với role CUSTOMER
                User newUser = new User();
                newUser.setEmail(email);
                newUser.setFullName(name);
                newUser.setGoogleId(googleId);
                newUser.setUsername(email);
                newUser.setRole(customerRole);
                // Gán password mặc định cho user Google
                String defaultPassword = passwordEncoder.encode("GOOGLE_LOGIN");
                newUser.setPassword(defaultPassword);
                // Đánh dấu chưa xác thực OTP
                newUser.setTotpEnabled(false);
                // ...existing code...
                User savedUser = userRepository.save(newUser);
                // Sinh secret TOTP và gửi email mã OTP/TOTP cho user
                String secret = getOrCreateTotpSecret(email);
                // Tạo link QR cho app Authenticator
                String qrUrl = new ktc.spring_project.services.TotpService().getQRBarcode(email, secret);
                // Gửi email hướng dẫn cấu hình app Authenticator
                String emailContent = "Chào mừng bạn đăng ký FastRoute!\n\n" +
                    "Để kích hoạt bảo mật 2 lớp, hãy cấu hình ứng dụng Google Authenticator hoặc Microsoft Authenticator với thông tin sau:\n" +
                    "- Secret: " + secret + "\n" +
                    "- Hoặc quét mã QR: " + qrUrl + "\n\n" +
                    "Sau khi cấu hình, hãy nhập mã OTP 6 số từ app vào hệ thống để hoàn tất xác thực.";
                sendOtpEmail(email, emailContent);
                return savedUser;
            }

        } catch (Exception e) {
            throw new HttpException("Google login failed: " + e.getMessage(), org.springframework.http.HttpStatus.UNAUTHORIZED);
        }
    }

    /**
     * Google Login với Credential (JWT từ Google)
     */
    public User googleLoginWithCredential(GoogleLoginWithCredentialRequestDto request) {
        try {
            String credential = request.getCredential();
            String googleTokenInfoUrl = "https://oauth2.googleapis.com/tokeninfo?id_token=" + credential;

            ResponseEntity<Map> response = restTemplate.getForEntity(googleTokenInfoUrl, Map.class);
            @SuppressWarnings("unchecked")
            Map<String, Object> tokenInfo = (Map<String, Object>) response.getBody();

            if (tokenInfo == null) {
                throw new HttpException("Google token validation returned null response", org.springframework.http.HttpStatus.BAD_REQUEST);
            }

            String email = (String) tokenInfo.get("email");
            String name = (String) tokenInfo.get("name");
            String googleId = (String) tokenInfo.get("sub");

            if (email == null || name == null || googleId == null) {
                throw new HttpException("Missing required fields from Google token response", org.springframework.http.HttpStatus.BAD_REQUEST);
            }

            if (request.getClientId() != null) {
                String aud = (String) tokenInfo.get("aud");
                if (!request.getClientId().equals(aud)) {
                    throw new HttpException("Invalid client ID in credential", org.springframework.http.HttpStatus.BAD_REQUEST);
                }
            }

            Optional<User> existingUser = userRepository.findByEmail(email);

            Role customerRole = roleRepository.findByRoleName("CUSTOMER")
                .orElseThrow(() -> new RuntimeException("Role CUSTOMER not found"));

            if (existingUser.isPresent()) {
                User user = existingUser.get();
                if (user.getGoogleId() == null) {
                    user.setGoogleId(googleId);
                }
                // Luôn gán role CUSTOMER khi đăng nhập Google
                user.setRole(customerRole);
                userRepository.save(user);
                return user;
            } else {
                // Tạo mới user với role CUSTOMER
                User newUser = new User();
                newUser.setEmail(email);
                newUser.setFullName(name);
                newUser.setGoogleId(googleId);
                newUser.setUsername(email);

                // Gán role mặc định USER
                Role defaultRole = roleRepository.findByRoleName("USER")
                    .orElseThrow(() -> new RuntimeException("Default role USER not found"));
                newUser.setRole(defaultRole);

                return userRepository.save(newUser);
            }

        } catch (Exception e) {
            throw new HttpException("Google credential login failed: " + e.getMessage(), org.springframework.http.HttpStatus.UNAUTHORIZED);
        }
    }

    // Lấy hoặc tạo secret TOTP cho user
    public String getOrCreateTotpSecret(String email) {
        User user = findByEmail(email);
        if (user.getTotpSecret() == null || user.getTotpSecret().isEmpty()) {
            String secret = new ktc.spring_project.services.TotpService().generateSecret();
            user.setTotpSecret(secret);
            userRepository.save(user);
            return secret;
        }
        return user.getTotpSecret();
    }

    // Lấy secret TOTP của user
    public String getTotpSecret(String email) {
        User user = findByEmail(email);
        return user.getTotpSecret();
    }

    // Kích hoạt trạng thái 2FA cho user
    public void enableTotp(String email) {
        User user = findByEmail(email);
        user.setTotpEnabled(true);
        userRepository.save(user);
    }

    // Hủy kích hoạt trạng thái 2FA cho user
    public void disableTotp(String email) {
        User user = findByEmail(email);
        user.setTotpEnabled(false);
        userRepository.save(user);
    }
     /**
     * Lấy danh sách store thuộc về user
     */
    public List<ktc.spring_project.entities.Store> getStoresByUserId(Long userId) {
        if (userId == null) {
            throw new HttpException("User ID cannot be null", HttpStatus.BAD_REQUEST);
        }
        return storeRepository.findByCreatedById(userId);
    }
    // Tìm user theo username
    public User findByUsername(String username) {
        return userRepository.findByUsername(username).orElse(null);
    }
}
