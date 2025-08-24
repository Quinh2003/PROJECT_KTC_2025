package ktc.spring_project.controllers;

import ktc.spring_project.entities.Address;
import ktc.spring_project.services.AddressService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/addresses")
public class AddressController {

    @Autowired
    private AddressService addressService;

    /**
     * Tạo mới address
     */
    @PostMapping
    public ResponseEntity<Address> createAddress(@RequestBody Address address) {
        Address created = addressService.createAddress(address);
        return new ResponseEntity<>(created, HttpStatus.CREATED);
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
    public ResponseEntity<Address> updateAddress(@PathVariable Long id, @RequestBody Address address) {
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
