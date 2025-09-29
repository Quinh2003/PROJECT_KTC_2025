package ktc.spring_project.controllers;

import ktc.spring_project.entities.Address;
import ktc.spring_project.services.AddressService;
import lombok.RequiredArgsConstructor;
import ktc.spring_project.dtos.address.CreateAddressRequestDTO;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping("/api/addresses")
public class AddressController {

    private final AddressService addressService;

    /**
     * Tạo mới address
     */
    @PostMapping
    public ResponseEntity<Address> createAddress(@Valid @RequestBody CreateAddressRequestDTO dto) {
        try {
            // Mapping thủ công từ DTO sang entity Address
            Address address = new Address();
            
            // Handle addressType conversion từ string thành enum
            if (dto.getAddressType() != null) {
                address.setAddressType(dto.getAddressType());
            } else {
                // Default to DELIVERY if not specified
                address.setAddressType(ktc.spring_project.enums.AddressType.DELIVERY);
            }
            
            address.setAddress(dto.getAddress());
            address.setLatitude(dto.getLatitude());
            address.setLongitude(dto.getLongitude());
            address.setCity(dto.getCity());
            address.setState(dto.getState());
            address.setCountry(dto.getCountry());
            address.setRegion(dto.getRegion());
            address.setPostalCode(dto.getPostalCode());
            address.setContactName(dto.getContactName());
            address.setContactPhone(dto.getContactPhone());
            address.setContactEmail(dto.getContactEmail());
            address.setFloorNumber(dto.getFloorNumber());
            
            Address created = addressService.createAddress(address);
            return new ResponseEntity<>(created, HttpStatus.CREATED);
        } catch (Exception e) {
            System.err.println("Error creating address: " + e.getMessage());
            e.printStackTrace();
            return new ResponseEntity<>(null, HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    /**
     * Lấy address theo id
     */
    @GetMapping("/{id}")
    public ResponseEntity<Address> getAddressById(@PathVariable Long id) {
        Address address = addressService.getAddressById(id);
        return ResponseEntity.ok(address);
    }

    /**
     * Lấy tất cả address
     */
    @GetMapping
    public ResponseEntity<List<Address>> getAllAddresses() {
        List<Address> addresses = addressService.getAllAddresses();
        return ResponseEntity.ok(addresses);
    }

    /**
     * Sửa address
     */
    @PutMapping("/{id}")
    public ResponseEntity<Address> updateAddress(@PathVariable Long id, @Valid @RequestBody Address address) {
        Address updated = addressService.updateAddress(id, address);
        return ResponseEntity.ok(updated);
    }

    /**
     * Xóa address
     */
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteAddress(@PathVariable Long id) {
        addressService.deleteAddress(id);
        return ResponseEntity.noContent().build();
    }
}
