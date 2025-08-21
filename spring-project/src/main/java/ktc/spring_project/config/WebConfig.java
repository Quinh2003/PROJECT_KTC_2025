package ktc.spring_project.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * Web Configuration - Cấu hình MVC cho ứng dụng Spring Boot
 *
 * Class này implement WebMvcConfigurer để tùy chỉnh cấu hình Spring MVC
 *
 * Chức năng chính:
 * - Cấu hình CORS (Cross-Origin Resource Sharing) cho phép frontend truy cập API
 * - Có thể mở rộng thêm các cấu hình MVC khác như interceptors, view resolvers, etc.
 *
 * Note: CORS cũng được cấu hình trong SecurityConfig, nhưng cấu hình này
 * sẽ áp dụng cho tất cả endpoints, kể cả những endpoint không qua Spring Security
 */
@Configuration
public class WebConfig implements WebMvcConfigurer {

    /**
     * Cấu hình CORS mapping cho tất cả API endpoints
     *
     * CORS (Cross-Origin Resource Sharing) giải quyết vấn đề Same-Origin Policy
     * của browser, cho phép frontend chạy trên domain khác có thể gọi API backend
     *
     * @param registry CorsRegistry để đăng ký CORS configuration
     */
    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/api/**")  // Áp dụng cho tất cả endpoints bắt đầu với /api/
                // Cho phép frontend từ các domain này truy cập
                // localhost:3000 - thường là React development server
                // localhost:3001 - có thể là Next.js hoặc React app khác
                .allowedOriginPatterns("http://localhost:3000", "http://localhost:3001", "http://localhost:5173")

                // Cho phép các HTTP methods
                .allowedMethods("GET", "POST", "PUT", "DELETE", "OPTIONS")

                // Cho phép tất cả headers trong request
                .allowedHeaders("*")

                // Cho phép gửi credentials (cookies, authorization headers)
                // Cần thiết để gửi JWT token trong Authorization header
                .allowCredentials(true);
    }
}
