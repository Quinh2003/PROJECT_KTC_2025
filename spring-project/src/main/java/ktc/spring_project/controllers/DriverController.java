
package ktc.spring_project.controllers;
import org.springframework.web.bind.annotation.*;

import ktc.spring_project.dtos.order.DeliveryOrderResponseDTO;
import ktc.spring_project.dtos.order.DriverOrderSimpleResponseDTO;
import ktc.spring_project.dtos.order.OrderDetailResponseDTO;
import ktc.spring_project.services.DeliveryService;
import org.springframework.beans.factory.annotation.Autowired;
import java.util.List;

@RestController
@RequestMapping("/api/driver")
public class DriverController {

	private final DeliveryService deliveryService;

	@Autowired
	public DriverController(DeliveryService deliveryService) {
		this.deliveryService = deliveryService;
	}

	@GetMapping("/{driverId}/orders")
	public List<DriverOrderSimpleResponseDTO> getOrdersByDriver(@PathVariable Long driverId) {
		return deliveryService.getOrdersByDriverId(driverId);
	}

		@GetMapping("/orders/{orderId}")
	public OrderDetailResponseDTO getOrderDetail(@PathVariable Long orderId) {
		return deliveryService.getOrderDetailById(orderId);
	}

}
