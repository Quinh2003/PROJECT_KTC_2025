package ktc.spring_project.dtos;

import java.util.List;
import java.util.Map;

public class ChecklistStepResponse {
    private String stepCode;
    private String stepName;
    private String description;
    private String category;
    private boolean isCompleted;
    private String stepStatus;  // Trạng thái từ bảng status (VD: "Pending", "Completed", "Processing")
    private String completionDetails;
    private int stepOrder;  // Đổi từ order thành stepOrder để tránh nhầm lẫn với đơn hàng
    private boolean isRequired;


    // Danh sách substeps cho step CUSTOMER_CREATE_ORDER (danh sách đơn hàng còn hoạt động)
    private List<Map<String, Object>> substeps;

    public ChecklistStepResponse() {}
    public List<Map<String, Object>> getSubsteps() {
        return substeps;
    }

    public void setSubsteps(List<Map<String, Object>> substeps) {
        this.substeps = substeps;
    }

    public ChecklistStepResponse(String stepCode, String stepName, String description) {
        this.stepCode = stepCode;
        this.stepName = stepName;
        this.description = description;
        this.isCompleted = false;
        this.isRequired = true;
    }

    // Getters and Setters
    public String getStepCode() {
        return stepCode;
    }

    public void setStepCode(String stepCode) {
        this.stepCode = stepCode;
    }

    public String getStepName() {
        return stepName;
    }

    public void setStepName(String stepName) {
        this.stepName = stepName;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getCategory() {
        return category;
    }

    public void setCategory(String category) {
        this.category = category;
    }

    public boolean isCompleted() {
        return isCompleted;
    }

    public void setCompleted(boolean completed) {
        isCompleted = completed;
    }

    public String getStepStatus() {
        return stepStatus;
    }

    public void setStepStatus(String stepStatus) {
        this.stepStatus = stepStatus;
    }

    public String getCompletionDetails() {
        return completionDetails;
    }

    public void setCompletionDetails(String completionDetails) {
        this.completionDetails = completionDetails;
    }

    public int getStepOrder() {
        return stepOrder;
    }

    public void setStepOrder(int stepOrder) {
        this.stepOrder = stepOrder;
    }

    public boolean isRequired() {
        return isRequired;
    }

    public void setRequired(boolean required) {
        isRequired = required;
    }
}
