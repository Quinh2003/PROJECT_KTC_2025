package ktc.spring_project.services;

import ktc.spring_project.entities.User;
import ktc.spring_project.repositories.UserJpaRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Custom User Details Service - Service để load thông tin user cho Spring Security
 *
 * Class này implement UserDetailsService interface của Spring Security
 * để cung cấp thông tin user từ database cho quá trình authentication
 *
 * Chức năng chính:
 * - Load user từ database dựa trên username hoặc email
 * - Convert User entity thành UserDetails object cho Spring Security
 * - Xử lý roles và permissions của user
 *
 * Được sử dụng bởi:
 * - DaoAuthenticationProvider để authenticate user login
 * - JwtAuthenticationFilter để load user từ JWT token
 */
@Service
public class CustomUserDetailsService implements UserDetailsService {

    // Repository để truy cập User data từ database
    @Autowired
    private UserJpaRepository userRepository;

    /**
     * Load user theo username hoặc email
     * Method chính được Spring Security gọi khi cần authenticate user
     *
     * @param username Username hoặc email của user
     * @return UserDetails object chứa thông tin user và authorities
     * @throws UsernameNotFoundException nếu không tìm thấy user
     */
    @Override
    @Transactional  // Đảm bảo lazy loading hoạt động đúng cho user.role
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
        // Tìm user theo username trước, nếu không có thì tìm theo email
        User user = userRepository.findByUsernameWithRole(username)
                .orElseGet(() -> userRepository.findByEmailWithRole(username)
                        .orElseThrow(() -> new UsernameNotFoundException("User not found with username or email: " + username)));

        // Convert User entity thành UserPrincipal (implement UserDetails)
        return UserPrincipal.create(user);
    }

    /**
     * Load user theo ID
     * Được sử dụng khi cần load user từ JWT token (token chứa user ID)
     *
     * @param id User ID
     * @return UserDetails object
     */
    @Transactional
    public UserDetails loadUserById(Long id) {
        User user = userRepository.findByIdWithRole(id)
                .orElseThrow(() -> new UsernameNotFoundException("User not found with id: " + id));

        return UserPrincipal.create(user);
    }

    /**
     * UserPrincipal - Inner class implement UserDetails interface
     *
     * Class này đóng gói thông tin user và implement các method của UserDetails
     * để Spring Security có thể sử dụng cho authentication và authorization
     */
    public static class UserPrincipal implements UserDetails {
        // Thông tin cơ bản của user
        private Long id;
        private String username;
        private String email;
        private String password;

        // Authorities (roles và permissions) của user
        private Collection<? extends GrantedAuthority> authorities;

        /**
         * Constructor với tất cả thông tin cần thiết
         */
        public UserPrincipal(Long id, String username, String email, String password, Collection<? extends GrantedAuthority> authorities) {
            this.id = id;
            this.username = username;
            this.email = email;
            this.password = password;
            this.authorities = authorities;
        }

        /**
         * Factory method để tạo UserPrincipal từ User entity
         *
         * @param user User entity từ database
         * @return UserPrincipal instance
         */
        public static UserPrincipal create(User user) {
            // Tạo list authorities từ user role
            List<GrantedAuthority> authorities = new ArrayList<>();

            // Thêm role của user vào authorities với prefix "ROLE_"
            // Spring Security yêu cầu role phải có prefix "ROLE_"
            if (user.getRole() != null) {
                authorities.add(new SimpleGrantedAuthority("ROLE_" + user.getRole().getRoleName()));
            }

            // Có thể mở rộng thêm permissions cụ thể nếu cần
            // authorities.add(new SimpleGrantedAuthority("PERMISSION_READ"));
            // authorities.add(new SimpleGrantedAuthority("PERMISSION_WRITE"));

            return new UserPrincipal(
                    user.getId(),
                    user.getUsername(),
                    user.getEmail(),
                    user.getPassword(),
                    authorities
            );
        }

        // Getter methods để truy cập thông tin user
        public Long getId() {
            return id;
        }

        public String getEmail() {
            return email;
        }

        // Implementation của UserDetails interface
        @Override
        public String getUsername() {
            return username;
        }

        @Override
        public String getPassword() {
            return password;
        }

        @Override
        public Collection<? extends GrantedAuthority> getAuthorities() {
            return authorities;
        }

        // Các method để kiểm tra trạng thái account
        // Trong implementation đơn giản này, tất cả đều return true
        // Có thể customize dựa trên business logic

        @Override
        public boolean isAccountNonExpired() {
            // Account không bao giờ hết hạn
            return true;
        }

        @Override
        public boolean isAccountNonLocked() {
            // Account không bao giờ bị khóa
            // Có thể kiểm tra user.getStatus() để implement lock logic
            return true;
        }

        @Override
        public boolean isCredentialsNonExpired() {
            // Credentials không bao giờ hết hạn
            return true;
        }

        @Override
        public boolean isEnabled() {
            // Account luôn được enable
            // Có thể kiểm tra user.getStatus() để implement enable/disable logic
            return true;
        }
    }
}
