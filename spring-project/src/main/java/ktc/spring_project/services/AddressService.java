package ktc.spring_project.services;

import ktc.spring_project.entities.Address;
import ktc.spring_project.repositories.AddressRepository;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class AddressService {

    @Autowired
    private AddressRepository addressRepository;

    public Address createAddress(Address address) {
        return addressRepository.save(address);
    }

    public Address getAddressById(Long id) {
        return addressRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Address not found with id: " + id));
    }

    public List<Address> getAllAddresses() {
        return addressRepository.findAll();
    }

    public Address updateAddress(Long id, Address addressDetails) {
        Address address = getAddressById(id);
        address.setAddressType(addressDetails.getAddressType());
        address.setAddress(addressDetails.getAddress());
        address.setLatitude(addressDetails.getLatitude());
        address.setLongitude(addressDetails.getLongitude());
        address.setCity(addressDetails.getCity());
        address.setState(addressDetails.getState());
        address.setCountry(addressDetails.getCountry());
        address.setRegion(addressDetails.getRegion());
        address.setPostalCode(addressDetails.getPostalCode());
        address.setContactName(addressDetails.getContactName());
        address.setContactPhone(addressDetails.getContactPhone());
        address.setContactEmail(addressDetails.getContactEmail());
        address.setFloorNumber(addressDetails.getFloorNumber());
        return addressRepository.save(address);
    }

    public void deleteAddress(Long id) {
        Address address = getAddressById(id);
        addressRepository.delete(address);
    }
}