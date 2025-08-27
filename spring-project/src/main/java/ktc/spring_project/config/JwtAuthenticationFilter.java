package ktc.spring_project.config;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

/**
 * JWT Authentication Filter - Filter xử lý JWT token trong mỗi HTTP request
 *
 * Class này extends OncePerRequestFilter để đảm bảo filter chỉ chạy 1 lần per request
 *
 * Chức năng:
 * - Extract JWT token từ Authorization header
 * - Validate JWT token
 * - Set authentication context cho Spring Security nếu token hợp lệ
 * - Cho phép request tiếp tục nếu token hợp lệ
 */
@Component
public class JwtAuthenticationFilter extends OncePerRequestFilter {

    // Component để validate và extract thông tin từ JWT token
    @Autowired
    private JwtTokenProvider tokenProvider;

    // Service để load thông tin user từ database
    @Autowired
    private UserDetailsService userDetailsService;

    /**
     * Method chính của filter, được gọi cho mỗi HTTP request
     *
     * @param request HTTP request
     * @param response HTTP response
     * @param filterChain Filter chain để tiếp tục xử lý request
     */
    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                  HttpServletResponse response,
                                  FilterChain filterChain) throws ServletException, IOException {
        try {
            // 1. Extract JWT token từ request header
            String jwt = getJwtFromRequest(request);

            // 2. Kiểm tra token có tồn tại và hợp lệ không
            if (StringUtils.hasText(jwt) && tokenProvider.validateToken(jwt)) {
                // 3. Extract username từ token
                String username = tokenProvider.getUsernameFromToken(jwt);

                // 4. Load thông tin user từ database
                UserDetails userDetails = userDetailsService.loadUserByUsername(username);

                // 5. Tạo Authentication object cho Spring Security
                UsernamePasswordAuthenticationToken authentication =
                    new UsernamePasswordAuthenticationToken(userDetails, null, userDetails.getAuthorities());

                // 6. Set thêm thông tin chi tiết từ request
                authentication.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));

                // 7. Set authentication vào SecurityContext để Spring Security biết user đã đăng nhập
                SecurityContextHolder.getContext().setAuthentication(authentication);
            }
        } catch (Exception ex) {
            // Log lỗi nhưng không throw exception để không block request
            logger.error("Could not set user authentication in security context", ex);
        }

        // 8. Tiếp tục với filter chain (cho phép request được xử lý tiếp)
        filterChain.doFilter(request, response);
    }

    /**
     * Extract JWT token từ Authorization header
     *
     * JWT token thường được gửi trong format: "Bearer <token>"
     * Method này sẽ extract phần <token>
     *
     * @param request HTTP request
     * @return JWT token string hoặc null nếu không có
     */
    private String getJwtFromRequest(HttpServletRequest request) {
        // Lấy Authorization header
        String bearerToken = request.getHeader("Authorization");

        // Kiểm tra header có tồn tại và bắt đầu với "Bearer " không
        if (StringUtils.hasText(bearerToken) && bearerToken.startsWith("Bearer ")) {
            // Return phần token (bỏ "Bearer " ở đầu)
            return bearerToken.substring(7);
        }
        return null;
    }

 @Override
protected boolean shouldNotFilter(HttpServletRequest request) throws ServletException {
    String path = request.getRequestURI();
    // Bỏ qua filter cho các endpoint public (tùy chỉnh theo API của bạn)
    return path.startsWith("/api/auth/");
}
}
