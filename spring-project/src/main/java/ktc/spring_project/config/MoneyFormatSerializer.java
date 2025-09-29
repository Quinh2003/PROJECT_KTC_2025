package ktc.spring_project.config;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;

import java.io.IOException;
import java.math.BigDecimal;
import java.text.DecimalFormat;

/**
 * Custom serializer để định dạng số tiền với dấu phẩy phân cách hàng nghìn
 * Ví dụ: 670521.00 -> "670,521"
 */
public class MoneyFormatSerializer extends JsonSerializer<BigDecimal> {
    
    private static final DecimalFormat MONEY_FORMAT = new DecimalFormat("#,###");
    
    @Override
    public void serialize(BigDecimal value, JsonGenerator gen, SerializerProvider serializers) 
            throws IOException {
        if (value == null) {
            gen.writeNull();
        } else {
            // Định dạng số với dấu phẩy và loại bỏ phần thập phân nếu là số nguyên
            String formattedValue = MONEY_FORMAT.format(value);
            gen.writeString(formattedValue);
        }
    }
}
