package ktc.spring_project.converters;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import ktc.spring_project.enums.ProductStatus;

@Converter(autoApply = true)
public class ProductStatusConverter implements AttributeConverter<ProductStatus, Integer> {

    @Override
    public Integer convertToDatabaseColumn(ProductStatus status) {
        if (status == null) {
            return null;
        }
        return status.getValue();
    }

    @Override
    public ProductStatus convertToEntityAttribute(Integer value) {
        if (value == null) {
            return null;
        }
        
        try {
            return ProductStatus.fromValue(value);
        } catch (IllegalArgumentException e) {
            // Nếu gặp giá trị không hợp lệ trong DB, mặc định về ACTIVE
            return ProductStatus.ACTIVE;
        }
    }
}
