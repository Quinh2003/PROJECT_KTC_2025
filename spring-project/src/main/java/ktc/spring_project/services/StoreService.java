package ktc.spring_project.services;

import ktc.spring_project.entities.Store;
import ktc.spring_project.repositories.StoreRepository;
import jakarta.persistence.EntityNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class StoreService {
    public Store createStoreFromDto(ktc.spring_project.dtos.store.CreateStoreRequestDTO dto) {
        Store store = new Store();
        store.setStoreName(dto.getStoreName());
        store.setEmail(dto.getEmail());
        store.setPhone(dto.getPhone());
        store.setAddress(dto.getAddress());
    store.setLatitude(dto.getLatitude() != null ? java.math.BigDecimal.valueOf(dto.getLatitude()) : null);
    store.setLongitude(dto.getLongitude() != null ? java.math.BigDecimal.valueOf(dto.getLongitude()) : null);
        store.setIsActive(dto.getIsActive());
        store.setNotes(dto.getNotes());
        // Nếu có các trường khác (createdBy...), hãy gán thêm ở đây
        return storeRepository.save(store);
    }

    @Autowired
    private StoreRepository storeRepository;

    public Store createStore(Store store) {
        return storeRepository.save(store);
    }

    public Store getStoreById(Long id) {
        return storeRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Store not found with id: " + id));
    }

    public List<Store> getAllStores() {
        return storeRepository.findAll();
    }

    public Store updateStore(Long id, Store storeDetails) {
        Store store = getStoreById(id);
        store.setId(storeDetails.getId());
        store.setStoreName(storeDetails.getStoreName());
        store.setEmail(storeDetails.getEmail());
        store.setPhone(storeDetails.getPhone());
        store.setAddress(storeDetails.getAddress());
        store.setLatitude(storeDetails.getLatitude());
        store.setLongitude(storeDetails.getLongitude());
        store.setIsActive(storeDetails.getIsActive());
        store.setCreatedBy(storeDetails.getCreatedBy());
        store.setNotes(storeDetails.getNotes());
        return storeRepository.save(store);
    }

    // public void deleteStore(Long id) {
    //     Store store = getStoreById(id);
    //     storeRepository.delete(store);
    // }

    // ...existing code...
public void deleteStore(Long id) {
    Store store = getStoreById(id);
    store.setIsActive(false); // Soft delete
    storeRepository.save(store);
}
// ...existing code...
}