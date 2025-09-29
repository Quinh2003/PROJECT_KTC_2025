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
        
        // Manually update the timestamp for explicit control
        store.setUpdatedAt(new java.sql.Timestamp(System.currentTimeMillis()));
        
        return storeRepository.save(store);
    }

    // public void deleteStore(Long id) {
    //     Store store = getStoreById(id);
    //     storeRepository.delete(store);
    // }

    public void deleteStore(Long id) {
    Store store = getStoreById(id);
    storeRepository.delete(store);
}

public List<Store> getStoresByUserId(Long userId) {
    return storeRepository.findByCreatedById(userId);
}

    /**
     * Update store information with restricted fields
     * Only updates: storeName, email, phone, isActive, notes
     * Address is not updated through this method
     */
    public Store updateStoreInfo(Long id, ktc.spring_project.dtos.store.UpdateStoreInfoDTO dto) {
        Store store = getStoreById(id);
        
        // Update only the allowed fields
        store.setStoreName(dto.getStoreName());
        store.setEmail(dto.getEmail());
        store.setPhone(dto.getPhone());
        store.setIsActive(dto.getIsActive());
        store.setNotes(dto.getNotes());
        
        // Manually set updated_at to current timestamp (optional, since @UpdateTimestamp handles this)
        // But this ensures we have explicit control over the update time
        store.setUpdatedAt(new java.sql.Timestamp(System.currentTimeMillis()));
        
        // Address is not updated - it remains as it was
        // Coordinates are also not updated
        
        return storeRepository.save(store);
    }

    /**
     * Convert Store entity to StoreInfoResponseDTO
     */
    public ktc.spring_project.dtos.store.StoreInfoResponseDTO convertToStoreInfoResponseDTO(Store store) {
        ktc.spring_project.dtos.store.StoreInfoResponseDTO dto = new ktc.spring_project.dtos.store.StoreInfoResponseDTO();
        dto.setId(store.getId());
        dto.setStoreName(store.getStoreName());
        dto.setEmail(store.getEmail());
        dto.setPhone(store.getPhone());
        dto.setAddress(store.getAddress());
        dto.setIsActive(store.getIsActive());
        dto.setNotes(store.getNotes());
        dto.setCreatedAt(store.getCreatedAt());
        dto.setUpdatedAt(store.getUpdatedAt());
        
        // Set creator name if available
        if (store.getCreatedBy() != null) {
            dto.setCreatedByUserName(store.getCreatedBy().getUsername());
        }
        
        return dto;
    }

    /**
     * Convert Store entity to UpdateStoreInfoDTO (for display purposes)
     */
    public ktc.spring_project.dtos.store.UpdateStoreInfoDTO convertToUpdateStoreInfoDTO(Store store) {
        ktc.spring_project.dtos.store.UpdateStoreInfoDTO dto = new ktc.spring_project.dtos.store.UpdateStoreInfoDTO();
        dto.setStoreName(store.getStoreName());
        dto.setEmail(store.getEmail());
        dto.setPhone(store.getPhone());
        dto.setIsActive(store.getIsActive());
        dto.setNotes(store.getNotes());
        dto.setAddress(store.getAddress()); // For display only
        
        return dto;
    }
}