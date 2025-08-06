package ktc.spring_project.services;

import ktc.spring_project.entities.User;
import ktc.spring_project.repositories.UserJpaRepository;
import ktc.spring_project.dtos.auth.GoogleLoginRequestDto;
import ktc.spring_project.dtos.auth.GoogleLoginWithCredentialRequestDto;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Map;

@Service
public class UserService {

    @Autowired
    private UserJpaRepository userRepository;

    public User createUser(User user) {
        return userRepository.save(user);
    }

    public User getUserById(Long id) {
        return userRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("User not found with id: " + id));
    }

    public List<User> getAllUsers() {
        return userRepository.findAll();
    }

    public User updateUser(Long id, User userDetails) {
        User user = getUserById(id);
        user.setUsername(userDetails.getUsername());
        user.setEmail(userDetails.getEmail());
        user.setPassword(userDetails.getPassword());
        user.setFullName(userDetails.getFullName());
        user.setPhone(userDetails.getPhone());
        user.setRole(userDetails.getRole());
        user.setStatus(userDetails.getStatus());
        user.setNotes(userDetails.getNotes());
        return userRepository.save(user);
    }

    public void deleteUser(Long id) {
        User user = getUserById(id);
        userRepository.delete(user);
    }

    /**
     * Get filtered users based on role, status, and search terms
     *
     * @param role Optional role filter
     * @param status Optional status filter
     * @param search Optional search term for username or fullName
     * @return List of users matching criteria
     */
    public List<User> getFilteredUsers(String role, String status, String search) {
        // Simple implementation - in a real app, would use more sophisticated filtering
        if (role == null && status == null && search == null) {
            return getAllUsers();
        }

        List<User> filteredUsers = new ArrayList<>();

        // Filter by role if specified
        if (role != null && !role.isEmpty()) {
            return userRepository.findByRoleName(role);
        }

        // Search by username or email
        if (search != null && !search.isEmpty()) {
            Optional<User> userOpt = userRepository.findByUsernameOrEmail(search, search);
            return userOpt.map(List::of).orElseGet(ArrayList::new);
        }

        return filteredUsers;
    }

    /**
     * Get current authenticated user
     *
     * @param authentication Spring Security Authentication object
     * @return User entity for the authenticated user
     */
    public User getCurrentUser(Authentication authentication) {
        if (authentication == null) {
            throw new RuntimeException("No authentication provided");
        }

        UserDetails userDetails = (UserDetails) authentication.getPrincipal();
        return userRepository.findByUsername(userDetails.getUsername())
                .orElseThrow(() -> new EntityNotFoundException("User not found"));
    }

    @Autowired
    private RestTemplate restTemplate;

    /**
     * Google Login với Access Token
     *
     * Method này xử lý authentication user thông qua Google OAuth 2.0 access token
     *
     * Flow xử lý:
     * 1. Nhận access token từ frontend (đã được Google cấp)
     * 2. Sử dụng access token để gọi Google API lấy thông tin user
     * 3. Kiểm tra user đã tồn tại trong database chưa
     * 4. Nếu có: cập nhật Google ID (nếu chưa có)
     * 5. Nếu chưa có: tạo user mới từ thông tin Google
     * 6. Trả về User entity để AuthController tạo JWT token
     *
     * @param request GoogleLoginRequestDto chứa access token và device info
     * @return User entity (existing hoặc newly created)
     * @throws RuntimeException nếu Google API call failed hoặc token invalid
     */
    public User googleLogin(GoogleLoginRequestDto request) {
        try {
            // 1. Gọi Google API để lấy thông tin user từ access token
            String googleApiUrl = "https://www.googleapis.com/oauth2/v2/userinfo?access_token=" + request.getAccessToken();

            // Setup headers với Bearer token
            HttpHeaders headers = new HttpHeaders();
            headers.set("Authorization", "Bearer " + request.getAccessToken());
            HttpEntity<String> entity = new HttpEntity<>(headers);

            // Gọi Google API với proper type safety
            ResponseEntity<Map> response = restTemplate.exchange(
                googleApiUrl,
                HttpMethod.GET,
                entity,
                Map.class
            );

            // 2. Extract thông tin user từ Google API response với null checks
            @SuppressWarnings("unchecked")
            Map<String, Object> userInfo = (Map<String, Object>) response.getBody();
            if (userInfo == null) {
                throw new RuntimeException("Google API returned null response");
            }

            String email = (String) userInfo.get("email");
            String name = (String) userInfo.get("name");
            String googleId = (String) userInfo.get("id");

            if (email == null || name == null || googleId == null) {
                throw new RuntimeException("Missing required fields from Google API response");
            }

            // 3. Kiểm tra user đã tồn tại trong database chưa
            Optional<User> existingUser = userRepository.findByEmail(email);

            if (existingUser.isPresent()) {
                // 4. User đã tồn tại - cập nhật Google ID nếu chưa có
                User user = existingUser.get();
                if (user.getGoogleId() == null) {
                    user.setGoogleId(googleId);
                    userRepository.save(user);
                }
                return user;
            } else {
                // 5. User chưa tồn tại - tạo user mới từ thông tin Google
                User newUser = new User();
                newUser.setEmail(email);
                newUser.setFullName(name);
                newUser.setGoogleId(googleId);
                newUser.setUsername(email); // Sử dụng email làm username

                // Note: Cần set default role và status
                // Bạn có thể uncomment và adjust theo entities của mình
                // newUser.setRole(defaultRole);
                // newUser.setStatus(activeStatus);

                return userRepository.save(newUser);
            }

        } catch (Exception e) {
            // Log error và throw RuntimeException để Controller xử lý
            throw new RuntimeException("Google login failed: " + e.getMessage());
        }
    }

    /**
     * Google Login với Credential (JWT từ Google)
     *
     * Method này xử lý authentication user thông qua Google One Tap hoặc Sign-In JavaScript library
     *
     * Flow xử lý:
     * 1. Nhận JWT credential từ frontend (đã được Google cấp)
     * 2. Validate JWT credential với Google tokeninfo endpoint
     * 3. Extract thông tin user từ validated token
     * 4. Kiểm tra user đã tồn tại trong database chưa
     * 5. Nếu có: cập nhật Google ID (nếu chưa có)
     * 6. Nếu chưa có: tạo user mới từ thông tin Google
     * 7. Trả về User entity để AuthController tạo JWT token
     *
     * @param request GoogleLoginWithCredentialRequestDto chứa JWT credential
     * @return User entity (existing hoặc newly created)
     * @throws RuntimeException nếu credential validation failed
     */
    public User googleLoginWithCredential(GoogleLoginWithCredentialRequestDto request) {
        try {
            // Note: Đây là implementation đơn giản
            // Trong production, bạn nên sử dụng Google JWT validation library
            // để validate signature và claims một cách chính xác

            String credential = request.getCredential();

            // 1. Validate credential với Google tokeninfo endpoint
            String googleTokenInfoUrl = "https://oauth2.googleapis.com/tokeninfo?id_token=" + credential;

            ResponseEntity<Map> response = restTemplate.getForEntity(googleTokenInfoUrl, Map.class);
            @SuppressWarnings("unchecked")
            Map<String, Object> tokenInfo = (Map<String, Object>) response.getBody();

            if (tokenInfo == null) {
                throw new RuntimeException("Google token validation returned null response");
            }

            // 2. Extract thông tin user từ validated token với null checks
            String email = (String) tokenInfo.get("email");
            String name = (String) tokenInfo.get("name");
            String googleId = (String) tokenInfo.get("sub"); // 'sub' là user ID trong JWT

            if (email == null || name == null || googleId == null) {
                throw new RuntimeException("Missing required fields from Google token response");
            }

            // Optional: Validate client_id nếu được provide
            if (request.getClientId() != null) {
                String aud = (String) tokenInfo.get("aud");
                if (!request.getClientId().equals(aud)) {
                    throw new RuntimeException("Invalid client ID in credential");
                }
            }

            // 3. Kiểm tra user đã tồn tại trong database chưa
            Optional<User> existingUser = userRepository.findByEmail(email);

            if (existingUser.isPresent()) {
                // 4. User đã tồn tại - cập nhật Google ID nếu chưa có
                User user = existingUser.get();
                if (user.getGoogleId() == null) {
                    user.setGoogleId(googleId);
                    userRepository.save(user);
                }
                return user;
            } else {
                // 5. User chưa tồn tại - tạo user mới từ thông tin Google
                User newUser = new User();
                newUser.setEmail(email);
                newUser.setFullName(name);
                newUser.setGoogleId(googleId);
                newUser.setUsername(email); // Sử dụng email làm username

                // Note: Cần set default role và status
                // newUser.setRole(defaultRole);
                // newUser.setStatus(activeStatus);

                return userRepository.save(newUser);
            }

        } catch (Exception e) {
            // Log error và throw RuntimeException để Controller xử lý
            throw new RuntimeException("Google credential login failed: " + e.getMessage());
        }
    }
}