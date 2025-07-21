package com.ktc.springbootproject.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDateTime;
import java.util.Map;

@RestController
@RequestMapping("/")
public class HomeController {
    
    @GetMapping
    public Map<String, Object> home() {
        return Map.of(
                "message", "Welcome to KTC Spring Boot Project!",
                "timestamp", LocalDateTime.now(),
                "status", "running",
                "version", "1.0.0"
        );
    }
    
    @GetMapping("/health")
    public Map<String, String> health() {
        return Map.of(
                "status", "UP",
                "service", "KTC Spring Boot Project"
        );
    }
}
