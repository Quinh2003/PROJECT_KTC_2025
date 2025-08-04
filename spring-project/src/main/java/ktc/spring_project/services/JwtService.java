package ktc.spring_project.services;

import ktc.spring_project.entities.Role;
import ktc.spring_project.entities.User;
import ktc.spring_project.repositories.RoleRepository;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.security.Keys;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;

import javax.crypto.SecretKey;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

@Service
public class JwtService {

    @Value("${jwt.secret}")
    private String secretKey;

    @Autowired
    private RoleRepository roleRepository;

    private SecretKey getSigningKey() {
        byte[] keyBytes = io.jsonwebtoken.io.Decoders.BASE64.decode(secretKey);
        return Keys.hmacShaKeyFor(keyBytes);
    }

    private String createToken(Map<String, Object> claims, String subject, long expiration) {
        Date now = new Date();
        Date expiryDate = new Date(now.getTime() + expiration);
        return Jwts.builder()
                .claims(claims)
                .subject(subject)
                .issuedAt(now)
                .expiration(expiryDate)
                .signWith(getSigningKey(), Jwts.SIG.HS256)
                .compact();
    }

    public String generateAccessToken(User user) {
        Map<String, Object> claims = new HashMap<>();
        claims.put("id", user.getId());
        claims.put("type", "access_token");
        
        // Thêm role_id và role_name vào claims
        Role role = roleRepository.findById(user.getRoleId())
                .orElseThrow(() -> new IllegalStateException("Không tìm thấy vai trò với id: " + user.getRoleId()));
        claims.put("role_id", role.getId());
        claims.put("role_name", role.getRoleName());

        long jwtExpiration = 86400000; // 24 giờ
        return createToken(claims, user.getUsername(), jwtExpiration);
    }

    private Claims extractAllClaims(String token) {
        try {
            return Jwts.parser()
                    .verifyWith(getSigningKey())
                    .build()
                    .parseSignedClaims(token)
                    .getPayload();
        } catch (Exception e) {
            throw new IllegalArgumentException("Token không hợp lệ: " + e.getMessage());
        }
    }

    public <T> T extractClaim(String token, Function<Claims, T> claimsResolver) {
        final Claims claims = extractAllClaims(token);
        return claimsResolver.apply(claims);
    }

    public String extractUsername(String token) {
        return extractClaim(token, Claims::getSubject);
    }

    public String extractTokenType(String token) {
        return extractClaim(token, claims -> claims.get("type", String.class));
    }

    public Long extractRoleId(String token) {
        return extractClaim(token, claims -> claims.get("role_id", Long.class));
    }

    public String extractRoleName(String token) {
        return extractClaim(token, claims -> claims.get("role_name", String.class));
    }

    public Date extractExpiration(String token) {
        return extractClaim(token, Claims::getExpiration);
    }

    private Boolean isTokenExpired(String token) {
        return extractExpiration(token).before(new Date());
    }

    public Boolean isTokenValid(String token, UserDetails userDetails) {
        final String username = extractUsername(token);
        final String tokenType = extractTokenType(token);
        return (username.equals(userDetails.getUsername()))
                && !isTokenExpired(token)
                && "access_token".equals(tokenType);
    }
}