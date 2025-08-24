package ktc.spring_project;

import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Profile;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.JavaMailSenderImpl;

/**
 * Test configuration cho Electronic Invoice testing
 * 
 * @author KTC Team
 * @version 1.0
 */
@TestConfiguration
@Profile("test")
public class ElectronicInvoiceTestConfiguration {

    /**
     * Mock JavaMailSender để test email functionality
     */
    @Bean
    @Primary
    public JavaMailSender mockMailSender() {
        return new JavaMailSenderImpl();
    }
}
