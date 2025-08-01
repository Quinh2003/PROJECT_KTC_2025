package ktc.spring_project.repositories;

import ktc.spring_project.entities.Token;
import ktc.spring_project.enums.TokenType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Repository
public interface TokenRepository extends JpaRepository<Token, Long> {
    
    Optional<Token> findByToken(String token);
    
    List<Token> findByUserId(Long userId);
    
    List<Token> findByTokenType(TokenType tokenType);
    
    List<Token> findByStatusId(Long statusId);
    
    @Query("SELECT t FROM Token t WHERE t.expiry < :currentTime")
    List<Token> findExpiredTokens(@Param("currentTime") LocalDateTime currentTime);
    
    @Query("SELECT t FROM Token t WHERE t.user.id = :userId AND t.tokenType = :tokenType AND t.status.code = 'ACTIVE'")
    List<Token> findActiveTokensByUserAndType(@Param("userId") Long userId, @Param("tokenType") TokenType tokenType);
    
    boolean existsByToken(String token);
}
