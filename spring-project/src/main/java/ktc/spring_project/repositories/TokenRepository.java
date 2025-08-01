package ktc.spring_project.repositories;

import ktc.spring_project.entities.Token;
import ktc.spring_project.entities.User;
import ktc.spring_project.enums.TokenType;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * Repository for Token entity
 * Handles authentication tokens and password reset tokens
 */
@Repository
public interface TokenRepository extends BaseRepository<Token, Long> {
    
    /**
     * Find token by token string
     */
    Optional<Token> findByToken(@Param("token") String token);
    
    /**
     * Find valid token by token string and type
     */
    @Query("SELECT t FROM Token t WHERE t.token = :token AND t.tokenType = :tokenType " +
           "AND t.expiry > CURRENT_TIMESTAMP AND t.status.code = 'ACTIVE'")
    Optional<Token> findValidToken(@Param("token") String token, @Param("tokenType") TokenType tokenType);
    
    /**
     * Find tokens by user and type
     */
    List<Token> findByUserAndTokenType(@Param("user") User user, @Param("tokenType") TokenType tokenType);
    
    /**
     * Find active tokens by user
     */
    @Query("SELECT t FROM Token t WHERE t.user = :user AND t.expiry > CURRENT_TIMESTAMP " +
           "AND t.status.code = 'ACTIVE' ORDER BY t.createdAt DESC")
    List<Token> findActiveTokensByUser(@Param("user") User user);
    
    /**
     * Delete expired tokens
     */
    @Query("DELETE FROM Token t WHERE t.expiry < CURRENT_TIMESTAMP")
    void deleteExpiredTokens();
    
    /**
     * Find expired tokens for cleanup
     */
    @Query("SELECT t FROM Token t WHERE t.expiry < CURRENT_TIMESTAMP")
    List<Token> findExpiredTokens();
    
    /**
     * Revoke all tokens for user
     */
    @Query("UPDATE Token t SET t.status.code = 'REVOKED' WHERE t.user = :user")
    void revokeAllTokensForUser(@Param("user") User user);
    
    /**
     * Find refresh tokens for user
     */
    @Query("SELECT t FROM Token t WHERE t.user = :user AND t.tokenType = 'REFRESH' " +
           "AND t.expiry > CURRENT_TIMESTAMP AND t.status.code = 'ACTIVE'")
    List<Token> findValidRefreshTokensByUser(@Param("user") User user);
    
    /**
     * Clean up old tokens (older than specified date)
     */
    @Query("DELETE FROM Token t WHERE t.createdAt < :cutoffDate")
    void deleteTokensOlderThan(@Param("cutoffDate") LocalDateTime cutoffDate);
    
    /**
     * Count active tokens by type
     */
    @Query("SELECT COUNT(t) FROM Token t WHERE t.tokenType = :tokenType " +
           "AND t.expiry > CURRENT_TIMESTAMP AND t.status.code = 'ACTIVE'")
    Long countActiveTokensByType(@Param("tokenType") TokenType tokenType);
}