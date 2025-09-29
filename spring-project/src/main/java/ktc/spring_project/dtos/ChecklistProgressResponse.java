package ktc.spring_project.dtos;

import java.util.List;

import java.util.Map;

public class ChecklistProgressResponse {
    private Long userId;
    private String userRole;
    private String userName;
    private int totalSteps;
    private int completedSteps;
    private double completionPercentage;
    private List<ChecklistStepResponse> steps;
    private String status;
    private String message;

    // Danh sách các đơn còn hoạt động của customer
    private List<Map<String, Object>> activeOrders;
    public List<Map<String, Object>> getActiveOrders() {
        return activeOrders;
    }

    public void setActiveOrders(List<Map<String, Object>> activeOrders) {
        this.activeOrders = activeOrders;
    }

    public ChecklistProgressResponse() {}

    public ChecklistProgressResponse(Long userId, String userRole, String userName) {
        this.userId = userId;
        this.userRole = userRole;
        this.userName = userName;
    }

    // Getters and Setters
    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public String getUserRole() {
        return userRole;
    }

    public void setUserRole(String userRole) {
        this.userRole = userRole;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public int getTotalSteps() {
        return totalSteps;
    }

    public void setTotalSteps(int totalSteps) {
        this.totalSteps = totalSteps;
    }

    public int getCompletedSteps() {
        return completedSteps;
    }

    public void setCompletedSteps(int completedSteps) {
        this.completedSteps = completedSteps;
    }

    public double getCompletionPercentage() {
        return completionPercentage;
    }

    public void setCompletionPercentage(double completionPercentage) {
        this.completionPercentage = completionPercentage;
    }

    public List<ChecklistStepResponse> getSteps() {
        return steps;
    }

    public void setSteps(List<ChecklistStepResponse> steps) {
        this.steps = steps;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    // Helper method để tính toán percentage
    public void calculateCompletionPercentage() {
        if (totalSteps > 0) {
            this.completionPercentage = Math.round((double) completedSteps / totalSteps * 100 * 100.0) / 100.0;
        } else {
            this.completionPercentage = 0.0;
        }
    }
}
