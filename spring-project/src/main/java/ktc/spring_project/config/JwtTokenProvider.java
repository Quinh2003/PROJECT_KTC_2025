// package ktc.spring_project.config;

// import io.jsonwebtoken.*;
// import io.jsonwebtoken.security.Keys;
// import org.springframework.beans.factory.annotation.Value;
// import org.springframework.security.core.Authentication;
// import org.springframework.security.core.userdetails.UserDetails;
// import org.springframework.stereotype.Component;

// import java.security.Key;
// import java.util.Date;

// /**
//  * JWT Token Provider - Component chịu trách nhiệm tạo và validate JWT tokens
//  *
//  * Class này xử lý:
//  * - Tạo JWT token từ thông tin authentication
//  * - Validate JWT token có hợp lệ không
//  * - Extract thông tin user từ JWT token
//  *
//  * Sử dụng thư viện JJWT để xử lý JWT
//  */
// @Component
// public class JwtTokenProvider {

//     // Secret key để ký JWT token, đọc từ application.properties
//     // Nếu không có trong config thì dùng giá trị mặc định
//     @Value("${app.jwt.secret:mySecretKey}")
//     private String jwtSecret;

//     // Thời gian hết hạn của JWT token (mili giây)
//     // Mặc định 86400000ms = 24 giờ
//     @Value("${app.jwt.expiration:86400000}")
//     private int jwtExpirationInMs;

//     /**
//      * Tạo signing key từ secret string
//      * Sử dụng HMAC SHA algorithm để tạo key
//      *
//      * @return Key object để ký JWT
//      */
//     private Key getSigningKey() {
//         byte[] keyBytes = jwtSecret.getBytes();
//         return Keys.hmacShaKeyFor(keyBytes);
//     }

//     /**
//      * Tạo JWT token từ Authentication object
//      *
//      * @param authentication Spring Security Authentication object chứa thông tin user đã đăng nhập
//      * @return JWT token string
//      */
//     public String generateToken(Authentication authentication) {
//         // Lấy UserDetails từ authentication
//         UserDetails userPrincipal = (UserDetails) authentication.getPrincipal();

//         // Tính thời gian hết hạn
//         Date expiryDate = new Date(System.currentTimeMillis() + jwtExpirationInMs);

//         // Build và return JWT token
//         return Jwts.builder()
//                 .setSubject(userPrincipal.getUsername())  // Username làm subject
//                 .setIssuedAt(new Date())                  // Thời gian tạo token
//                 .setExpiration(expiryDate)               // Thời gian hết hạn
//                 .signWith(getSigningKey(), SignatureAlgorithm.HS512)  // Ký token với HMAC SHA512
//                 //.signWith(getSigningKey(), SignatureAlgorithm.HS256)
//                 .compact();                              // Tạo token string
//     }

//     /**
//      * Extract username từ JWT token
//      *
//      * @param token JWT token string
//      * @return username của user
//      */
//     public String getUsernameFromToken(String token) {
//         // Parse token và lấy claims
//         Claims claims = Jwts.parserBuilder()
//                 .setSigningKey(getSigningKey())
//                 .build()
//                 .parseClaimsJws(token)
//                 .getBody();

//         // Return subject (username)
//         return claims.getSubject();
//     }

//     /**
//      * Validate JWT token
//      * Kiểm tra token có hợp lệ không (signature, expiration, format)
//      *
//      * @param authToken JWT token cần validate
//      * @return true nếu token hợp lệ, false nếu không
//      */
//     public boolean validateToken(String authToken) {
//         try {
//             // Thử parse token, nếu thành công thì token hợp lệ
//             Jwts.parserBuilder()
//                 .setSigningKey(getSigningKey())
//                 .build()
//                 .parseClaimsJws(authToken);
//             return true;
//         } catch (SecurityException ex) {
//             // Signature không hợp lệ
//             System.err.println("Invalid JWT signature");
//         } catch (MalformedJwtException ex) {
//             // Format token không đúng
//             System.err.println("Invalid JWT token");
//         } catch (ExpiredJwtException ex) {
//             // Token đã hết hạn
//             System.err.println("Expired JWT token");
//         } catch (UnsupportedJwtException ex) {
//             // JWT không được hỗ trợ
//             System.err.println("Unsupported JWT token");
//         } catch (IllegalArgumentException ex) {
//             // Claims string rỗng
//             System.err.println("JWT claims string is empty");
//         }
//         return false;
//     }
// }


package ktc.spring_project.config;

import io.jsonwebtoken.*;
import io.jsonwebtoken.security.Keys;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.util.Date;

@Component
public class JwtTokenProvider {

    // Đọc đúng key từ application.properties
    @Value("${jwt.secret:mySecretKey}")
    private String jwtSecret;

    @Value("${jwt.expiration:86400000}")
    private int jwtExpirationInMs;

    private Key getSigningKey() {
        // Đảm bảo dùng UTF-8 để lấy bytes
        byte[] keyBytes = jwtSecret.getBytes(StandardCharsets.UTF_8);
        return Keys.hmacShaKeyFor(keyBytes);
    }

    public String generateToken(Authentication authentication) {
        UserDetails userPrincipal = (UserDetails) authentication.getPrincipal();
        Date expiryDate = new Date(System.currentTimeMillis() + jwtExpirationInMs);

        return Jwts.builder()
                .setSubject(userPrincipal.getUsername())
                .setIssuedAt(new Date())
                .setExpiration(expiryDate)
                .signWith(getSigningKey(), SignatureAlgorithm.HS512)
                .compact();
    }

    public String getUsernameFromToken(String token) {
        Claims claims = Jwts.parserBuilder()
                .setSigningKey(getSigningKey())
                .build()
                .parseClaimsJws(token)
                .getBody();
        return claims.getSubject();
    }

    public boolean validateToken(String authToken) {
        try {
            Jwts.parserBuilder()
                .setSigningKey(getSigningKey())
                .build()
                .parseClaimsJws(authToken);
            return true;
        } catch (SecurityException ex) {
            System.err.println("Invalid JWT signature");
        } catch (MalformedJwtException ex) {
            System.err.println("Invalid JWT token");
        } catch (ExpiredJwtException ex) {
            System.err.println("Expired JWT token");
        } catch (UnsupportedJwtException ex) {
            System.err.println("Unsupported JWT token");
        } catch (IllegalArgumentException ex) {
            System.err.println("JWT claims string is empty");
        }
        return false;
    }
}