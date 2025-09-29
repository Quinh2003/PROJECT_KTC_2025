
package ktc.spring_project.dtos.address;

import ktc.spring_project.enums.AddressType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSetter;

import java.math.BigDecimal;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

/**
 * DTO for creating a new address
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class CreateAddressRequestDTO {

    private AddressType addressType;
    
    // Custom setter để convert string thành enum
    @JsonSetter("addressType")
    public void setAddressTypeFromString(String addressTypeStr) {
        if (addressTypeStr != null) {
            try {
                this.addressType = AddressType.valueOf(addressTypeStr.toUpperCase());
            } catch (IllegalArgumentException e) {
                this.addressType = AddressType.DELIVERY; // Default fallback
            }
        } else {
            this.addressType = AddressType.DELIVERY;
        }
    }

    @NotBlank(message = "Address is required")
    private String address;

    private BigDecimal latitude;
    private BigDecimal longitude;

    @NotBlank(message = "City is required")
    private String city;

    private String state;

    private String country;

    private String region;
    private String postalCode;

    @NotBlank(message = "Contact name is required")
    private String contactName;

    @NotBlank(message = "Contact phone is required")
    private String contactPhone;

    @Email(message = "Contact email must be valid")
    private String contactEmail;
    private String floorNumber;
    
    // Constructors
    public CreateAddressRequestDTO(AddressType addressType, String address) {
        this.addressType = addressType;
        this.address = address;
    }
    
    public AddressType getAddressType() { return addressType; }
    public void setAddressType(AddressType addressType) { this.addressType = addressType; }
    
    public String getAddress() { return address; }
    public void setAddress(String address) { this.address = address; }
    
    public BigDecimal getLatitude() { return latitude; }
    public void setLatitude(BigDecimal latitude) { this.latitude = latitude; }
    
    public BigDecimal getLongitude() { return longitude; }
    public void setLongitude(BigDecimal longitude) { this.longitude = longitude; }
    
    public String getCity() { return city; }
    public void setCity(String city) { this.city = city; }
    
    public String getState() { return state; }
    public void setState(String state) { this.state = state; }
    
    public String getCountry() { return country; }
    public void setCountry(String country) { this.country = country; }
    
    public String getRegion() { return region; }
    public void setRegion(String region) { this.region = region; }
    
    public String getPostalCode() { return postalCode; }
    public void setPostalCode(String postalCode) { this.postalCode = postalCode; }
    
    public String getContactName() { return contactName; }
    public void setContactName(String contactName) { this.contactName = contactName; }
    
    public String getContactPhone() { return contactPhone; }
    public void setContactPhone(String contactPhone) { this.contactPhone = contactPhone; }
    
    public String getContactEmail() { return contactEmail; }
    public void setContactEmail(String contactEmail) { this.contactEmail = contactEmail; }
    
    public String getFloorNumber() { return floorNumber; }
    public void setFloorNumber(String floorNumber) { this.floorNumber = floorNumber; }
    
    
    public String getFullAddress() {
        StringBuilder fullAddress = new StringBuilder(address);
        if (floorNumber != null && !floorNumber.trim().isEmpty()) {
            fullAddress.append(", Floor ").append(floorNumber);
        }
        if (city != null && !city.trim().isEmpty()) {
            fullAddress.append(", ").append(city);
        }
        if (state != null && !state.trim().isEmpty()) {
            fullAddress.append(", ").append(state);
        }
        if (country != null && !country.trim().isEmpty()) {
            fullAddress.append(", ").append(country);
        }
        return fullAddress.toString();
    }
}