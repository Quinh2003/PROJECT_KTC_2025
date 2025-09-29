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
import io.jsonwebtoken.ExpiredJwtException;

import java.io.IOException;

/**
 * JWT Authentication Filter - Enhanced with proper expired token handling
 *
 * This filter now properly handles expired tokens by sending appropriate
 * HTTP responses instead of just logging errors.
 */
@Component
public class JwtAuthenticationFilter extends OncePerRequestFilter {

    @Autowired
    private JwtTokenProvider tokenProvider;

    @Autowired
    private UserDetailsService userDetailsService;

    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                  HttpServletResponse response,
                                  FilterChain filterChain) throws ServletException, IOException {
        try {
            // 1. Extract JWT token from request header
            String jwt = getJwtFromRequest(request);

            // 2. Check if token exists and validate it
            if (StringUtils.hasText(jwt)) {
                // Check if token is expired first
                if (tokenProvider.isTokenExpired(jwt)) {
                    handleExpiredToken(request, response);
                    return; // Don't continue the filter chain
                }
                
                // Validate token (signature, format, etc.)
                if (tokenProvider.validateToken(jwt)) {
                    // 3. Extract username from token
                    String username = tokenProvider.getUsernameFromToken(jwt);

                    // 4. Load user details from database
                    UserDetails userDetails = userDetailsService.loadUserByUsername(username);

                    // 5. Create Authentication object for Spring Security
                    UsernamePasswordAuthenticationToken authentication =
                        new UsernamePasswordAuthenticationToken(userDetails, null, userDetails.getAuthorities());

                    // 6. Set additional details from request
                    authentication.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));

                    // 7. Set authentication in SecurityContext
                    SecurityContextHolder.getContext().setAuthentication(authentication);
                } else {
                    // Token is invalid
                    handleInvalidToken(request, response);
                    return;
                }
            }
        } catch (ExpiredJwtException ex) {
            logger.error("JWT token is expired: " + ex.getMessage());
            handleExpiredToken(request, response);
            return;
        } catch (Exception ex) {
            logger.error("Could not set user authentication in security context", ex);
            handleInvalidToken(request, response);
            return;
        }

        // 8. Continue with filter chain
        filterChain.doFilter(request, response);
    }

    /**
     * Handle expired JWT token
     */
    private void handleExpiredToken(HttpServletRequest request, HttpServletResponse response) throws IOException {
        response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        
        String jsonResponse = "{"
            + "\"error\": \"Token expired\","
            + "\"message\": \"Your session has expired after 15 minutes of inactivity. Please log in again.\","
            + "\"timestamp\": \"" + java.time.Instant.now().toString() + "\","
            + "\"path\": \"" + request.getRequestURI() + "\","
            + "\"status\": 401"
            + "}";
            
        response.getWriter().write(jsonResponse);
    }

    /**
     * Handle invalid JWT token
     */
    private void handleInvalidToken(HttpServletRequest request, HttpServletResponse response) throws IOException {
        response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        
        String jsonResponse = "{"
            + "\"error\": \"Invalid token\","
            + "\"message\": \"The provided authentication token is invalid. Please log in again.\","
            + "\"timestamp\": \"" + java.time.Instant.now().toString() + "\","
            + "\"path\": \"" + request.getRequestURI() + "\","
            + "\"status\": 401"
            + "}";
            
        response.getWriter().write(jsonResponse);
    }

    /**
     * Extract JWT token from Authorization header
     */
    private String getJwtFromRequest(HttpServletRequest request) {
        String bearerToken = request.getHeader("Authorization");
        if (StringUtils.hasText(bearerToken) && bearerToken.startsWith("Bearer ")) {
            return bearerToken.substring(7);
        }
        return null;
    }

    /**
     * Skip filter for public endpoints
     */
    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) throws ServletException {
        String path = request.getRequestURI();
        // Skip filter for authentication endpoints and public resources
        return path.startsWith("/api/auth/") || 
               path.startsWith("/swagger-ui/") || 
               path.startsWith("/v3/api-docs/") ||
               path.equals("/login") ||
               path.equals("/register") ||
               path.startsWith("/public/");
    }
}
