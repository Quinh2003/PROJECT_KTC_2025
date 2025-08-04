package ktc.spring_project.services;

import ktc.spring_project.dtos.LoginRequestDto;
import ktc.spring_project.dtos.LoginResponseDto;
import ktc.spring_project.entities.User;
import ktc.spring_project.repositories.UserRepository;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

@Service
public class UserService {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private JwtService jwtService;

    @Autowired
    private BCryptPasswordEncoder passwordEncoder;

    public LoginResponseDto login(LoginRequestDto request) {
        // Tìm người dùng theo username
        User user = userRepository.findByUsername(request.getUsername())
                .orElseThrow(() -> new EntityNotFoundException("Tên người dùng hoặc mật khẩu không đúng"));

        // Kiểm tra trạng thái tài khoản
        if (user.getStatusId() == null || user.getStatusId() != 1) {
            throw new EntityNotFoundException("Tài khoản không hoạt động");
        }

        // Kiểm tra mật khẩu
        if (!passwordEncoder.matches(request.getPassword(), user.getPassword())) {
            throw new EntityNotFoundException("Tên người dùng hoặc mật khẩu không đúng");
        }

        // Tạo token JWT
        String accessToken = jwtService.generateAccessToken(user);

        return LoginResponseDto.builder()
                .id(user.getId())
                .username(user.getUsername())
                .accessToken(accessToken)
                .build();
    }
}