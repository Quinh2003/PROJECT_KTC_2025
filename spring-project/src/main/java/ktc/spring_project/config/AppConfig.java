package ktc.spring_project.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestTemplate;

/**
 * Application Configuration - Cấu hình các beans chung cho ứng dụng
 *
 * Class này định nghĩa các beans được sử dụng chung trong toàn bộ ứng dụng
 *
 * Chức năng chính:
 * - Cấu hình RestTemplate để thực hiện HTTP requests đến external APIs
 * - Có thể mở rộng thêm các beans khác như ObjectMapper, HttpClient, etc.
 */
@Configuration
public class AppConfig {

    /**
     * Bean RestTemplate để thực hiện HTTP requests
     *
     * RestTemplate là Spring class để gọi REST APIs từ backend
     * Được sử dụng trong UserService để:
     * - Gọi Google APIs để validate Google access token
     * - Gọi Google APIs để validate Google credential JWT
     * - Lấy thông tin user từ Google sau khi authenticate
     *
     * RestTemplate hỗ trợ:
     * - GET, POST, PUT, DELETE requests
     * - Automatic JSON serialization/deserialization
     * - Error handling
     * - Custom headers và authentication
     *
     * @return RestTemplate instance
     */
    @Bean
    public RestTemplate restTemplate() {
        return new RestTemplate();
    }
}
