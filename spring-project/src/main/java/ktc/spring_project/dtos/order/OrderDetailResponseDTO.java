package ktc.spring_project.dtos.order;

import ktc.spring_project.dtos.address.AddressResponseDTO;
import ktc.spring_project.dtos.store.StoreResponseDTO;
import ktc.spring_project.dtos.delivery.DeliveryResponseDTO;
import ktc.spring_project.dtos.orderitem.OrderItemResponseDTO;

import java.util.List;

public class OrderDetailResponseDTO {
    private Long id;
    private String status;
    private String description;
    private String notes;

    private AddressResponseDTO address;
    private StoreResponseDTO store;
    private DeliveryResponseDTO delivery;
    private List<OrderItemResponseDTO> orderItems;

    // Getters & Setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public String getStatus() { return status; }
    public void setStatus(String status) { this.status = status; }

    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }

    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }

    public AddressResponseDTO getAddress() { return address; }
    public void setAddress(AddressResponseDTO address) { this.address = address; }

    public StoreResponseDTO getStore() { return store; }
    public void setStore(StoreResponseDTO store) { this.store = store; }

    public DeliveryResponseDTO getDelivery() { return delivery; }
    public void setDelivery(DeliveryResponseDTO delivery) { this.delivery = delivery; }

    public List<OrderItemResponseDTO> getOrderItems() { return orderItems; }
    public void setOrderItems(List<OrderItemResponseDTO> orderItems) { this.orderItems = orderItems; }
}
