package com.ktc.logistics.service;

import com.ktc.logistics.entity.Token;
import com.ktc.logistics.repository.TokenRepository;
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

    public Token save(Token entity) {
        return tokenRepository.save(entity);
    }

    public void delete(Long id) {
        tokenRepository.deleteById(id);
    }
}
