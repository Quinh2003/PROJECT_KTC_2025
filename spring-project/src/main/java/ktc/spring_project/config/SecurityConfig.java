package ktc.spring_project.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.password.NoOpPasswordEncoder;
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
    // @Bean
    // public PasswordEncoder passwordEncoder() {
    // return new BCryptPasswordEncoder();
    // }

    @Bean
    public PasswordEncoder passwordEncoder() {
        return new org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder();
    }

    /**
     * Authentication Provider để xác thực user
     * Sử dụng modern approach cho Spring Security 6.x
     *
     * @return DaoAuthenticationProvider instance
     */
    @Bean
    public DaoAuthenticationProvider authenticationProvider() {
        DaoAuthenticationProvider authProvider = new DaoAuthenticationProvider();
authProvider.setUserDetailsService(userDetailsService);
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
            // Static resources và public endpoints
            .requestMatchers("/", "/index.html", "/favicon.ico", "/static/**").permitAll()
            .requestMatchers("/uploads/**").permitAll() // Cho phép truy cập công khai file minh chứng
            .requestMatchers("/api/auth/**").permitAll()
            .requestMatchers("/api/public/**").permitAll()
            .requestMatchers("/actuator/**").permitAll()
            .requestMatchers("/swagger-ui.html", "/swagger-ui/**", "/v3/api-docs/**", "/webjars/**").permitAll()
                        
                        // Role-based access control
                        .requestMatchers("/api/admin/**").hasRole("ADMIN")
                        .requestMatchers("/api/dispatcher/**").hasAnyRole("ADMIN", "DISPATCHER")
                        .requestMatchers("/api/drivers/**").hasAnyRole("ADMIN", "DRIVER")
                        .requestMatchers("/api/driver/**").hasAnyRole("ADMIN", "DRIVER")
                        .requestMatchers("/api/customer/**").hasAnyRole("ADMIN", "CUSTOMER")
                        
                        // Public API endpoints - không cần authentication
.requestMatchers("/api/auth/users/**").permitAll()
                        .requestMatchers("/api/auth/users").permitAll()
                        .requestMatchers("/api/categories/**").permitAll()
                        .requestMatchers("/api/products/**").permitAll()
                        .requestMatchers("/api/orders/**").permitAll()
                        .requestMatchers("/api/addresses/**").permitAll()
                        .requestMatchers("/api/order-items/**").permitAll()
                        .requestMatchers("/api/deliveries/**").permitAll()
                        
                        // Store endpoints
                        .requestMatchers(HttpMethod.GET, "/api/stores", "/api/stores/**").permitAll()
                        .requestMatchers(HttpMethod.POST, "/api/stores").permitAll()
                        .requestMatchers(HttpMethod.PUT, "/api/stores/**").permitAll()
                        .requestMatchers(HttpMethod.PATCH, "/api/stores/**").permitAll()
                        .requestMatchers(HttpMethod.DELETE, "/api/stores/**").hasAnyRole("ADMIN", "USER", "CUSTOMER")
                        
                        // Order endpoints
                        .requestMatchers(HttpMethod.POST, "/api/orders", "/api/orders/**").permitAll()
                        .requestMatchers(HttpMethod.PUT, "/api/orders/**").permitAll()
                        .requestMatchers(HttpMethod.PATCH, "/api/orders/**").permitAll()
                        
                        // Route endpoints
                        .requestMatchers(HttpMethod.POST, "/api/routes").permitAll()
                        
                        // Delivery endpoints
                        .requestMatchers(HttpMethod.GET, "/api/deliveries", "/api/deliveries/**").permitAll()
                        
                        // Maintenance APIs require authentication
                        .requestMatchers("/api/drivers/*/maintenance-requests/**").hasAnyRole("ADMIN", "DRIVER")
                        .requestMatchers("/api/fleet/maintenance-requests/**").hasAnyRole("ADMIN", "FLEET")
                        .requestMatchers("/api/maintenance-requests/**").hasAnyRole("ADMIN", "DRIVER", "FLEET")
                        
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

        // Cho phép tất cả các origin truy cập API (thích hợp cho môi trường
        // development)
        configuration.setAllowedOriginPatterns(java.util.List.of("*"));

        // Cho phép các HTTP methods
        configuration.setAllowedMethods(java.util.List.of("GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"));

        // Cho phép tất cả headers
        configuration.setAllowedHeaders(java.util.List.of("*"));

        // Cho phép gửi credentials (cookies, authorization headers)
        configuration.setAllowCredentials(true);

        // Apply configuration cho tất cả API endpoints
        org.springframework.web.cors.UrlBasedCorsConfigurationSource source = new org.springframework.web.cors.UrlBasedCorsConfigurationSource();
        source.registerCorsConfiguration("/api/**", configuration);
        source.registerCorsConfiguration("/api/sse/**", configuration);  // Explicit CORS for SSE
        return source;
    }
}