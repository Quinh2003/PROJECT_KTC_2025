package ktc.spring_project.services.utils;

import ktc.spring_project.dtos.common.PagedResponse;
import org.springframework.data.domain.Page;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Random;

/**
 * Utility class for common service operations
 */
public class ServiceUtils {
    
    private static final Random RANDOM = new Random();
    private static final DateTimeFormatter ORDER_CODE_FORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");
    
    /**
     * Convert Spring Data Page to custom PagedResponse DTO
     */
    public static <T> PagedResponse<T> convertToPagedResponse(Page<T> page) {
        PagedResponse<T> pagedResponse = new PagedResponse<>();
        pagedResponse.setContent(page.getContent());
        pagedResponse.setTotalElements(page.getTotalElements());
        pagedResponse.setTotalPages(page.getTotalPages());
        pagedResponse.setSize(page.getSize());
        pagedResponse.setNumber(page.getNumber());
        pagedResponse.setFirst(page.isFirst());
        pagedResponse.setLast(page.isLast());
        pagedResponse.setEmpty(page.isEmpty());
        return pagedResponse;
    }
    
    /**
     * Hash password using SHA-256
     * Note: In production, use BCrypt or similar strong hashing
     */
    public static String hashPassword(String password) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            byte[] hash = md.digest(password.getBytes());
            StringBuilder hexString = new StringBuilder();
            for (byte b : hash) {
                String hex = Integer.toHexString(0xff & b);
                if (hex.length() == 1) {
                    hexString.append('0');
                }
                hexString.append(hex);
            }
            return hexString.toString();
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("Error hashing password", e);
        }
    }
    
    /**
     * Generate unique order code
     * Format: ORD-YYYYMMDD-XXXXX
     */
    public static String generateOrderCode() {
        String dateStr = LocalDateTime.now().format(ORDER_CODE_FORMATTER);
        String randomStr = String.format("%05d", RANDOM.nextInt(100000));
        return "ORD-" + dateStr + "-" + randomStr;
    }
    
    /**
     * Generate unique product code
     * Format: PROD-XXXXX
     */
    public static String generateProductCode() {
        String randomStr = String.format("%05d", RANDOM.nextInt(100000));
        return "PROD-" + randomStr;
    }
    
    /**
     * Check if string is null or empty
     */
    public static boolean isNullOrEmpty(String str) {
        return str == null || str.trim().isEmpty();
    }
    
    /**
     * Check if list is null or empty
     */
    public static boolean isNullOrEmpty(List<?> list) {
        return list == null || list.isEmpty();
    }
    
    /**
     * Get safe string value (return empty string if null)
     */
    public static String getSafeString(String str) {
        return str != null ? str : "";
    }
    
    /**
     * Validate email format using simple regex
     */
    public static boolean isValidEmail(String email) {
        if (isNullOrEmpty(email)) {
            return false;
        }
        return email.matches("^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$");
    }
    
    /**
     * Validate phone number format
     */
    public static boolean isValidPhoneNumber(String phone) {
        if (isNullOrEmpty(phone)) {
            return true; // Phone is optional
        }
        return phone.matches("^[+]?[0-9\\s\\-\\(\\)]{8,20}$");
    }
    
    /**
     * Calculate percentage
     */
    public static double calculatePercentage(long part, long total) {
        if (total == 0) {
            return 0.0;
        }
        return (double) part / total * 100.0;
    }
}