package ktc.spring_project.services;

import ktc.spring_project.entities.ElectronicInvoice;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import com.lowagie.text.*;
import com.lowagie.text.pdf.*;

import java.io.*;
import java.awt.Color;
import java.math.BigDecimal;

/**
 * Service để tạo file PDF thật sự từ dữ liệu hóa đơn
 * Sử dụng OpenPDF (iText) để tạo PDF từ dữ liệu cấu trúc
 */
@Service
@Slf4j
public class PdfGenerationService {

    /**
     * Tạo file PDF thật sự từ dữ liệu hóa đơn bằng OpenPDF
     * 
     * @param invoice Hóa đơn cần tạo PDF
     * @param outputFile File output để lưu PDF
     * @throws IOException nếu có lỗi tạo file
     */
    public void generatePdfFromInvoice(ElectronicInvoice invoice, File outputFile) throws IOException {
        try {
            log.info("🔄 Bắt đầu tạo PDF thật sự cho hóa đơn {}", invoice.getInvoiceNumber());
            
            // Tạo PDF Document
            Document document = new Document(PageSize.A4, 50, 50, 50, 50);
            PdfWriter writer = PdfWriter.getInstance(document, new FileOutputStream(outputFile));
            document.open();
            
            // Thêm content vào PDF
            addInvoiceContentToPdf(document, invoice);
            
            document.close();
            writer.close();
            
            log.info("✅ Đã tạo PDF thành công: {} (size: {} KB)", 
                outputFile.getName(), outputFile.length() / 1024);
                
        } catch (Exception e) {
            log.error("❌ Lỗi khi tạo PDF cho hóa đơn {}: ", invoice.getInvoiceNumber(), e);
            throw new IOException("Không thể tạo file PDF: " + e.getMessage(), e);
        }
    }
    
    /**
     * Thêm nội dung hóa đơn vào PDF document
     */
    private void addInvoiceContentToPdf(Document document, ElectronicInvoice invoice) throws DocumentException {
        // Font cho tiếng Việt
        Font titleFont = new Font(Font.HELVETICA, 18, Font.BOLD, Color.BLUE);
        Font headerFont = new Font(Font.HELVETICA, 14, Font.BOLD, Color.RED);
        Font normalFont = new Font(Font.HELVETICA, 12, Font.NORMAL);
        Font boldFont = new Font(Font.HELVETICA, 12, Font.BOLD);
        
        // Title
        Paragraph title = new Paragraph("HÓA ĐƠN THANH TOÁN", titleFont);
        title.setAlignment(Element.ALIGN_CENTER);
        document.add(title);
        
        Paragraph company = new Paragraph("KTC LOGISTICS", headerFont);
        company.setAlignment(Element.ALIGN_CENTER);
        document.add(company);
        
        document.add(new Paragraph(" ")); // Khoảng trống
        
        // Thông tin hóa đơn
        Table infoTable = new Table(2);
        infoTable.setWidth(100);
        infoTable.setBorderWidth(1);
        infoTable.setBorderColor(Color.GRAY);
        infoTable.setPadding(5);
        infoTable.setSpacing(0);
        
        infoTable.addCell(new Cell(new Phrase("Số hóa đơn:", boldFont)));
        infoTable.addCell(new Cell(new Phrase(invoice.getInvoiceNumber(), normalFont)));
        
        infoTable.addCell(new Cell(new Phrase("Ngày xuất:", boldFont)));
        infoTable.addCell(new Cell(new Phrase(invoice.getIssuedAt().toString(), normalFont)));
        
        infoTable.addCell(new Cell(new Phrase("Mã đơn hàng:", boldFont)));
        infoTable.addCell(new Cell(new Phrase(invoice.getOrder().getId().toString(), normalFont)));
        
        infoTable.addCell(new Cell(new Phrase("Mã vận đơn:", boldFont)));
        infoTable.addCell(new Cell(new Phrase(invoice.getDelivery().getId().toString(), normalFont)));
        
        if (invoice.getCustomerName() != null && !invoice.getCustomerName().trim().isEmpty()) {
            infoTable.addCell(new Cell(new Phrase("Khách hàng:", boldFont)));
            infoTable.addCell(new Cell(new Phrase(invoice.getCustomerName(), normalFont)));
        }
        
        if (invoice.getCustomerEmail() != null && !invoice.getCustomerEmail().trim().isEmpty()) {
            infoTable.addCell(new Cell(new Phrase("Email:", boldFont)));
            infoTable.addCell(new Cell(new Phrase(invoice.getCustomerEmail(), normalFont)));
        }
        
        document.add(infoTable);
        document.add(new Paragraph(" ")); // Khoảng trống
        
        // Chi tiết hóa đơn
        Paragraph detailTitle = new Paragraph("Chi tiết hóa đơn:", boldFont);
        document.add(detailTitle);
        document.add(new Paragraph(" "));
        
        Table detailTable = new Table(2);
        detailTable.setWidth(100);
        detailTable.setBorderWidth(1);
        detailTable.setBorderColor(Color.GRAY);
        detailTable.setPadding(5);
        detailTable.setSpacing(0);
        
        // Header
        Cell headerCell1 = new Cell(new Phrase("Mô tả", boldFont));
        headerCell1.setBackgroundColor(Color.LIGHT_GRAY);
        detailTable.addCell(headerCell1);
        
        Cell headerCell2 = new Cell(new Phrase("Số tiền", boldFont));
        headerCell2.setBackgroundColor(Color.LIGHT_GRAY);
        detailTable.addCell(headerCell2);
        
        // Tổng giá trị (lấy từ delivery fee)
        detailTable.addCell(new Cell(new Phrase("Tổng giá trị đơn hàng", normalFont)));
        detailTable.addCell(new Cell(new Phrase(formatCurrency(invoice.getDelivery().getDeliveryFee()), normalFont)));
        
        // Thuế VAT nếu có
        if (invoice.getTaxAmount() != null && invoice.getTaxAmount().compareTo(BigDecimal.ZERO) > 0) {
            detailTable.addCell(new Cell(new Phrase("Thuế VAT (10%)", normalFont)));
            detailTable.addCell(new Cell(new Phrase(formatCurrency(invoice.getTaxAmount()), normalFont)));
            
            // Thành tiền
            Cell totalLabelCell = new Cell(new Phrase("Thành tiền", boldFont));
            totalLabelCell.setBackgroundColor(new Color(232, 245, 232));
            detailTable.addCell(totalLabelCell);
            
            Cell totalAmountCell = new Cell(new Phrase(formatCurrency(invoice.getNetAmount()), boldFont));
            totalAmountCell.setBackgroundColor(new Color(232, 245, 232));
            detailTable.addCell(totalAmountCell);
        }
        
        document.add(detailTable);
        document.add(new Paragraph(" "));
        
        // Ghi chú
        if (invoice.getNotes() != null && !invoice.getNotes().trim().isEmpty()) {
            Paragraph notes = new Paragraph("Ghi chú: " + invoice.getNotes(), normalFont);
            document.add(notes);
            document.add(new Paragraph(" "));
        }
        
        // Footer
        Paragraph footer1 = new Paragraph("Hóa đơn thanh toán được xuất từ hệ thống KTC Logistics", normalFont);
        footer1.setAlignment(Element.ALIGN_CENTER);
        document.add(footer1);
        
        Paragraph footer2 = new Paragraph("Cảm ơn quý khách đã sử dụng dịch vụ của chúng tôi!", normalFont);
        footer2.setAlignment(Element.ALIGN_CENTER);
        document.add(footer2);
    }

    
    /**
     * Format tiền tệ
     */
    private String formatCurrency(BigDecimal amount) {
        if (amount == null) return "0 VND";
        return String.format("%,.0f VND", amount);
    }

}
