package ktc.spring_project.services;

import ktc.spring_project.entities.Token;
import ktc.spring_project.repository.TokenRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class TokenService {
    private final TokenRepository tokenRepository;

    public List<Token> findAll() {
        return tokenRepository.findAll();
    }

    public Optional<Token> findById(Long id) {
        return tokenRepository.findById(id);
    }

    public Token save(Token entities) {
        return tokenRepository.save(entities);
    }

    public void delete(Long id) {
        tokenRepository.deleteById(id);
    }
}
