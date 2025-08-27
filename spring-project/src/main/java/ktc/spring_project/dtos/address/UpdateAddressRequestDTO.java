package ktc.spring_project.dtos.address;

import ktc.spring_project.enums.AddressType;
import java.math.BigDecimal;

/**
 * DTO for updating an existing address
 */
public class UpdateAddressRequestDTO {
    
    private AddressType addressType;
    private String address;
    private BigDecimal latitude;
    private BigDecimal longitude;
    private String city;
    private String state;
    private String country;
    private String region;
    private String postalCode;
    private String contactName;
    private String contactPhone;
    private String contactEmail;
    private String floorNumber;
    
    // Constructors
    public UpdateAddressRequestDTO() {}
    
    // Getters and Setters
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
    
    // Validation methods
    public boolean hasValidAddress() {
        return address != null && !address.trim().isEmpty();
    }
    
    public boolean hasCoordinates() {
        return latitude != null && longitude != null;
    }
    
    public boolean hasContactInfo() {
        return (contactName != null && !contactName.trim().isEmpty()) ||
               (contactPhone != null && !contactPhone.trim().isEmpty()) ||
               (contactEmail != null && !contactEmail.trim().isEmpty());
    }
}