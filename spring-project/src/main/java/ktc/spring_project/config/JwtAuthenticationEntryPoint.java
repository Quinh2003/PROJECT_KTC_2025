package ktc.spring_project.config;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.stereotype.Component;

import java.io.IOException;

/**
 * JWT Authentication Entry Point
 * 
 * This component handles authentication failures and expired token scenarios.
 * When a user's JWT token expires after 15 minutes of inactivity, this entry point
 * will be triggered to handle the unauthorized access appropriately.
 */
@Component
public class JwtAuthenticationEntryPoint implements AuthenticationEntryPoint {

    @Override
    public void commence(HttpServletRequest request, 
                        HttpServletResponse response,
                        AuthenticationException authException) throws IOException, ServletException {
        
        // Check if this is an API request or a web request
        String requestURI = request.getRequestURI();
        String acceptHeader = request.getHeader("Accept");
        
        // For API requests (JSON responses expected)
        if (requestURI.startsWith("/api/") || 
            (acceptHeader != null && acceptHeader.contains("application/json"))) {
            
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            response.setContentType("application/json");
            response.setCharacterEncoding("UTF-8");
            
            String jsonResponse = "{"
                + "\"error\": \"Authentication required\","
                + "\"message\": \"Your session has expired after 15 minutes of inactivity. Please log in again.\","
                + "\"timestamp\": \"" + java.time.Instant.now().toString() + "\","
                + "\"path\": \"" + requestURI + "\","
                + "\"status\": 401,"
                + "\"loginUrl\": \"/api/auth/login\""
                + "}";
                
            response.getWriter().write(jsonResponse);
        } else {
            // For web requests, redirect to login page
            // You can customize this URL based on your frontend routing
            response.sendRedirect("/login?expired=true");
        }
    }
}