package ktc.spring_project.repositories;

import ktc.spring_project.entities.Address;
import ktc.spring_project.enums.AddressType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;

@Repository
public interface AddressRepository extends JpaRepository<Address, Long> {
        
    List<Address> findByAddressType(AddressType addressType);
    
    List<Address> findByCity(String city);
    
    // @Query("SELECT a FROM Address a WHERE a.order.id = :orderId")
    // List<Address> findByOrderOrderId(@Param("orderId") Long orderId);

    // @Query("SELECT a FROM Address a WHERE a.order.id = :orderId AND a.addressType = :type")
    // List<Address> findByOrderIdAndType(@Param("orderId") Long orderId, @Param("type") AddressType type);
    
    @Query("SELECT a FROM Address a WHERE a.latitude BETWEEN :minLat AND :maxLat " +
           "AND a.longitude BETWEEN :minLng AND :maxLng")
    List<Address> findAddressesInArea(@Param("minLat") BigDecimal minLat,
                                    @Param("maxLat") BigDecimal maxLat,
                                    @Param("minLng") BigDecimal minLng,
                                    @Param("maxLng") BigDecimal maxLng);
    
    @Query("SELECT a FROM Address a WHERE LOWER(a.address) LIKE LOWER(CONCAT('%', :address, '%'))")
    List<Address> findByAddressContaining(@Param("address") String address);
    
    @Query("SELECT COUNT(a) FROM Address a WHERE a.city = :city")
    long countByCity(@Param("city") String city);
}