package ktc.spring_project.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

/**
 * Security Configuration - Cấu hình bảo mật cho ứng dụng Spring Boot
 *
 * Class này cấu hình:
 * - JWT authentication thay vì session-based authentication
 * - RBAC (Role-Based Access Control) cho các endpoints
 * - CORS cho phép frontend truy cập API
 * - Password encoding với BCrypt
 * - Custom authentication provider
 *
 * Annotations:
 * - @Configuration: Đánh dấu đây là class cấu hình Spring
 * - @EnableWebSecurity: Enable Spring Security cho web application
 * - @EnableMethodSecurity: Enable method-level security với @PreAuthorize
 */
@Configuration
@EnableWebSecurity
@EnableMethodSecurity(prePostEnabled = true)
public class SecurityConfig {

    // Service để load thông tin user từ database
    @Autowired
    private UserDetailsService userDetailsService;

    // Custom JWT filter để xử lý authentication
    @Autowired
    private JwtAuthenticationFilter jwtAuthenticationFilter;

    /**
     * Bean để encode/decode password
     * Sử dụng BCrypt algorithm - một trong những thuật toán hash mạnh nhất hiện tại
     *
     * @return PasswordEncoder instance
     */
    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

    /**
     * Authentication Provider để xác thực user
     * Sử dụng DAO (Database Access Object) pattern để load user từ database
     *
     * @return DaoAuthenticationProvider instance
     */
    @Bean
    public DaoAuthenticationProvider authenticationProvider() {
        DaoAuthenticationProvider authProvider = new DaoAuthenticationProvider();
        // Set service để load user details
        authProvider.setUserDetailsService(userDetailsService);
        // Set password encoder để verify password
        authProvider.setPasswordEncoder(passwordEncoder());
        return authProvider;
    }

    /**
     * Authentication Manager để quản lý quá trình authentication
     * Được sử dụng trong AuthService để authenticate user login
     *
     * @param config Authentication configuration
     * @return AuthenticationManager instance
     */
    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration config) throws Exception {
        return config.getAuthenticationManager();
    }

    /**
     * Security Filter Chain - Cấu hình chính cho Spring Security
     *
     * Cấu hình:
     * 1. CORS: Cho phép frontend (React/Next.js) truy cập API
     * 2. CSRF: Disable vì sử dụng JWT thay vì session
     * 3. Session Management: Stateless vì sử dụng JWT
     * 4. Authorization Rules: Định nghĩa quyền truy cập cho từng endpoint
     * 5. JWT Filter: Thêm custom filter để xử lý JWT token
     *
     * @param http HttpSecurity configuration
     * @return SecurityFilterChain instance
     */
    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        http
            // Cấu hình CORS để frontend có thể gọi API
            .cors(cors -> cors.configurationSource(corsConfigurationSource()))

            // Disable CSRF vì ứng dụng sử dụng JWT thay vì session cookies
            .csrf(csrf -> csrf.disable())

            // Cấu hình session management là STATELESS (không lưu session)
            .sessionManagement(session -> session.sessionCreationPolicy(SessionCreationPolicy.STATELESS))

            // Cấu hình authorization rules cho các endpoints
            .authorizeHttpRequests(authz -> authz
                // Public endpoints - không cần authentication
                .requestMatchers("/api/auth/**").permitAll()        // Login, register, forgot password
                .requestMatchers("/api/public/**").permitAll()      // Public APIs

                // Role-based endpoints - cần authentication + specific role
                .requestMatchers("/api/admin/**").hasRole("ADMIN")              // Chỉ ADMIN
                .requestMatchers("/api/dispatcher/**").hasAnyRole("ADMIN", "DISPATCHER")  // ADMIN hoặc DISPATCHER
                .requestMatchers("/api/driver/**").hasAnyRole("ADMIN", "DRIVER")          // ADMIN hoặc DRIVER
                .requestMatchers("/api/customer/**").hasAnyRole("ADMIN", "CUSTOMER")      // ADMIN hoặc CUSTOMER

                // Protected endpoints - cần authentication nhưng không cần role cụ thể
                .requestMatchers("/api/protected/**").authenticated()

                // Tất cả endpoints khác đều cần authentication
                .anyRequest().authenticated()
            )

            // Set custom authentication provider
            .authenticationProvider(authenticationProvider())

            // Thêm JWT filter trước UsernamePasswordAuthenticationFilter
            // JWT filter sẽ check token trước khi Spring Security check username/password
            .addFilterBefore(jwtAuthenticationFilter, UsernamePasswordAuthenticationFilter.class);

        return http.build();
    }

    /**
     * CORS Configuration Source - Cấu hình chi tiết cho CORS
     *
     * CORS (Cross-Origin Resource Sharing) cho phép frontend chạy trên domain khác
     * có thể gọi API backend
     *
     * @return CorsConfigurationSource instance
     */
    @Bean
    public org.springframework.web.cors.CorsConfigurationSource corsConfigurationSource() {
        org.springframework.web.cors.CorsConfiguration configuration = new org.springframework.web.cors.CorsConfiguration();

        // Cho phép frontend từ các domain này truy cập API
        configuration.setAllowedOriginPatterns(java.util.List.of("http://localhost:3000", "http://localhost:3001"));

        // Cho phép các HTTP methods
        configuration.setAllowedMethods(java.util.List.of("GET", "POST", "PUT", "DELETE", "OPTIONS"));

        // Cho phép tất cả headers
        configuration.setAllowedHeaders(java.util.List.of("*"));

        // Cho phép gửi credentials (cookies, authorization headers)
        configuration.setAllowCredentials(true);

        // Apply configuration cho tất cả API endpoints
        org.springframework.web.cors.UrlBasedCorsConfigurationSource source = new org.springframework.web.cors.UrlBasedCorsConfigurationSource();
        source.registerCorsConfiguration("/api/**", configuration);
        return source;
    }
}
