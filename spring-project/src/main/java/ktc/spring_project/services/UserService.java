package ktc.spring_project.services;

import ktc.spring_project.entities.User;
import ktc.spring_project.entities.Role;
import ktc.spring_project.repositories.RoleRepository;
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
import java.lang.reflect.Field;

import org.springframework.util.ReflectionUtils;

@Service
public class UserService {

    @Autowired
    private UserJpaRepository userRepository;

    @Autowired
    private RoleRepository roleRepository;

    @Autowired
    private RestTemplate restTemplate;

    public User createUser(User user) {
        // Gán role mặc định nếu chưa có
        if (user.getRole() == null) {
            Role defaultRole = roleRepository.findByRoleName("USER")
                .orElseThrow(() -> new RuntimeException("Default role USER not found"));
            user.setRole(defaultRole);
        }
        return userRepository.save(user);
    }

    public User getUserById(Long id) {
        return userRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("User not found with id: " + id));
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
            user.setPassword(userDetails.getPassword());
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
            throw new RuntimeException("No authentication provided");
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
                ReflectionUtils.setField(field, user, value);
            }
        });

        return userRepository.save(user);
    }

    public List<User> getAllUsers() {
        return userRepository.findAll();
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
                throw new RuntimeException("Google API returned null response");
            }

            String email = (String) userInfo.get("email");
            String name = (String) userInfo.get("name");
            String googleId = (String) userInfo.get("id");

            if (email == null || name == null || googleId == null) {
                throw new RuntimeException("Missing required fields from Google API response");
            }

            Optional<User> existingUser = userRepository.findByEmail(email);

            if (existingUser.isPresent()) {
                User user = existingUser.get();
                if (user.getGoogleId() == null) {
                    user.setGoogleId(googleId);
                    userRepository.save(user);
                }
                return user;
            } else {
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
            throw new RuntimeException("Google login failed: " + e.getMessage());
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
                throw new RuntimeException("Google token validation returned null response");
            }

            String email = (String) tokenInfo.get("email");
            String name = (String) tokenInfo.get("name");
            String googleId = (String) tokenInfo.get("sub");

            if (email == null || name == null || googleId == null) {
                throw new RuntimeException("Missing required fields from Google token response");
            }

            if (request.getClientId() != null) {
                String aud = (String) tokenInfo.get("aud");
                if (!request.getClientId().equals(aud)) {
                    throw new RuntimeException("Invalid client ID in credential");
                }
            }

            Optional<User> existingUser = userRepository.findByEmail(email);

            if (existingUser.isPresent()) {
                User user = existingUser.get();
                if (user.getGoogleId() == null) {
                    user.setGoogleId(googleId);
                    userRepository.save(user);
                }
                return user;
            } else {
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
            throw new RuntimeException("Google credential login failed: " + e.getMessage());
        }
    }
}