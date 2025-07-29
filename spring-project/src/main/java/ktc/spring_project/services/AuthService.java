package ktc.spring_project.services;

import ktc.spring_project.entities.User;
import ktc.spring_project.repositories.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

//	Đăng nhập / Quên mật khẩu

@Service
public class AuthService {

    @Autowired
    private UserRepository userRepository;

    public Optional<User> login(String email, String password) {
        return userRepository.findByEmailAndPassword(email, password);
    }

    public boolean forgotPassword(String email) {
        // Gửi email và tạo PasswordResetToken
        return userRepository.existsByEmail(email);
    }
}