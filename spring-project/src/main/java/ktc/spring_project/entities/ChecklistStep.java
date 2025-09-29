
package ktc.spring_project.entities;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

@Entity
@Table(name = "checklist_step", uniqueConstraints = @UniqueConstraint(columnNames = "stepCode"))
public class ChecklistStep {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @NotBlank
    @Column(nullable = false)
    private String role; // CUSTOMER, DISPATCHER, DRIVER

    @NotBlank
    @Column(nullable = false, unique = true, length = 100)
    private String stepCode;

    @NotBlank
    @Column(nullable = false)
    private String stepName;

    @NotBlank
    @Column(nullable = false, length = 255)
    private String description;

    @NotNull
    @Column(nullable = false)
    private Integer stepOrder;

    // Getters and setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public String getRole() { return role; }
    public void setRole(String role) { this.role = role; }

    public String getStepCode() { return stepCode; }
    public void setStepCode(String stepCode) { this.stepCode = stepCode; }

    public String getStepName() { return stepName; }
    public void setStepName(String stepName) { this.stepName = stepName; }

    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }

    public Integer getStepOrder() { return stepOrder; }
    public void setStepOrder(Integer stepOrder) { this.stepOrder = stepOrder; }
}
