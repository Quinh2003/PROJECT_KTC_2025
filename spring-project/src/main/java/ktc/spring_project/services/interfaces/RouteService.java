package ktc.spring_project.services.interfaces;

import ktc.spring_project.dtos.route.CreateRouteRequestDTO;
import ktc.spring_project.dtos.route.RouteResponseDTO;
import ktc.spring_project.dtos.common.PagedResponse;
import ktc.spring_project.entities.Route;
import org.springframework.data.domain.Pageable;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * Service interface for Route management and AI optimization
 * Handles route planning, optimization, and GPS tracking
 */
public interface RouteService {
    
    // ===== ROUTE CRUD =====
    /**
     * Create a new route with waypoints
     * @param createRouteDTO Route creation data
     * @param createdBy User who creates the route
     * @return Created route response
     */
    RouteResponseDTO createRoute(CreateRouteRequestDTO createRouteDTO, Long createdBy);
    
    /**
     * Update existing route
     * @param routeId Route ID
     * @param updateData Updated route data
     * @return Updated route response
     */
    RouteResponseDTO updateRoute(Long routeId, CreateRouteRequestDTO updateData);
    
    /**
     * Get route by ID
     * @param routeId Route ID
     * @return Route response or empty if not found
     */
    Optional<RouteResponseDTO> getRouteById(Long routeId);
    
    /**
     * Get route entity by ID (for internal service use)
     * @param routeId Route ID
     * @return Route entity or empty if not found
     */
    Optional<Route> getRouteEntityById(Long routeId);
    
    /**
     * Get route by name
     * @param routeName Route name
     * @return Route response or empty if not found
     */
    Optional<RouteResponseDTO> getRouteByName(String routeName);
    
    /**
     * Delete route
     * @param routeId Route ID
     */
    void deleteRoute(Long routeId);
    
    // ===== ROUTE QUERIES =====
    /**
     * Get all routes with pagination
     * @param pageable Pagination parameters
     * @return Paged route responses
     */
    PagedResponse<RouteResponseDTO> getAllRoutes(Pageable pageable);
    
    /**
     * Get routes created by user
     * @param createdBy User ID
     * @param pageable Pagination parameters
     * @return Paged routes created by user
     */
    PagedResponse<RouteResponseDTO> getRoutesByCreator(Long createdBy, Pageable pageable);
    
    /**
     * Get AI-optimized routes
     * @param pageable Pagination parameters
     * @return Paged AI-optimized routes
     */
    PagedResponse<RouteResponseDTO> getAiOptimizedRoutes(Pageable pageable);
    
    /**
     * Get routes within distance range
     * @param minDistance Minimum distance in km
     * @param maxDistance Maximum distance in km
     * @param pageable Pagination parameters
     * @return Paged routes within distance range
     */
    PagedResponse<RouteResponseDTO> getRoutesByDistanceRange(BigDecimal minDistance, BigDecimal maxDistance, Pageable pageable);
    
    /**
     * Get routes within duration range
     * @param minDuration Minimum duration in minutes
     * @param maxDuration Maximum duration in minutes
     * @param pageable Pagination parameters
     * @return Paged routes within duration range
     */
    PagedResponse<RouteResponseDTO> getRoutesByDurationRange(Integer minDuration, Integer maxDuration, Pageable pageable);
    
    /**
     * Get routes within cost range
     * @param minCost Minimum estimated cost
     * @param maxCost Maximum estimated cost
     * @param pageable Pagination parameters
     * @return Paged routes within cost range
     */
    PagedResponse<RouteResponseDTO> getRoutesByCostRange(BigDecimal minCost, BigDecimal maxCost, Pageable pageable);
    
    // ===== ROUTE OPTIMIZATION =====
    /**
     * Optimize route using AI algorithms
     * @param routeId Route ID to optimize
     * @param optimizedBy User who triggers optimization
     * @return Optimized route response
     */
    RouteResponseDTO optimizeRoute(Long routeId, Long optimizedBy);
    
    /**
     * Create optimized route for multiple delivery addresses
     * @param deliveryAddresses List of delivery addresses with coordinates
     * @param startingPoint Starting point coordinates
     * @param createdBy User who creates the route
     * @return AI-optimized route response
     */
    RouteResponseDTO createOptimizedRoute(List<CreateRouteRequestDTO.WaypointDTO> deliveryAddresses, 
                                        CreateRouteRequestDTO.WaypointDTO startingPoint, 
                                        Long createdBy);
    
    /**
     * Calculate optimal route for given waypoints
     * @param waypoints List of waypoints
     * @return Optimized waypoints order with distance/duration calculations
     */
    List<CreateRouteRequestDTO.WaypointDTO> calculateOptimalRoute(List<CreateRouteRequestDTO.WaypointDTO> waypoints);
    
    /**
     * Recalculate route estimates (distance, duration, cost)
     * @param routeId Route ID
     * @return Updated route response with new estimates
     */
    RouteResponseDTO recalculateEstimates(Long routeId);
    
    // ===== ROUTE EFFICIENCY =====
    /**
     * Get most efficient routes (best cost per km)
     * @param limit Number of routes to return
     * @return List of most efficient routes
     */
    List<RouteResponseDTO> getMostEfficientRoutes(int limit);
    
    /**
     * Get routes with highest utilization (most orders assigned)
     * @param limit Number of routes to return
     * @return List of most utilized routes
     */
    List<RouteResponseDTO> getMostUtilizedRoutes(int limit);
    
    /**
     * Calculate route efficiency score
     * @param routeId Route ID
     * @return Efficiency score (cost per km)
     */
    BigDecimal calculateEfficiencyScore(Long routeId);
    
    /**
     * Get average route efficiency
     * @return Average efficiency score across all routes
     */
    BigDecimal getAverageEfficiency();
    
    // ===== ROUTE ASSIGNMENT =====
    /**
     * Get routes with assigned orders
     * @param pageable Pagination parameters
     * @return Paged routes that have orders assigned
     */
    PagedResponse<RouteResponseDTO> getRoutesWithOrders(Pageable pageable);
    
    /**
     * Get routes without assigned orders
     * @param pageable Pagination parameters
     * @return Paged routes that have no orders assigned
     */
    PagedResponse<RouteResponseDTO> getRoutesWithoutOrders(Pageable pageable);
    
    /**
     * Get available routes for new order assignment
     * @param estimatedDistance Estimated distance for the order
     * @param estimatedDuration Estimated duration for the order
     * @return List of suitable routes for assignment
     */
    List<RouteResponseDTO> getAvailableRoutesForOrder(BigDecimal estimatedDistance, Integer estimatedDuration);
    
    // ===== SEARCH AND ANALYTICS =====
    /**
     * Search routes by name or notes
     * @param searchTerm Search term
     * @param pageable Pagination parameters
     * @return Paged search results
     */
    PagedResponse<RouteResponseDTO> searchRoutes(String searchTerm, Pageable pageable);
    
    /**
     * Get route statistics
     * @return Map of route statistics (total, optimized, average distance, etc.)
     */
    java.util.Map<String, Object> getRouteStatistics();
    
    /**
     * Get distance distribution statistics
     * @return Map of distance range statistics
     */
    java.util.Map<String, Object> getDistanceDistribution();
    
    /**
     * Get cost savings from AI optimization
     * @return Total cost savings achieved through AI optimization
     */
    BigDecimal getOptimizationSavings();
    
    // ===== GPS AND WAYPOINT UTILITIES =====
    /**
     * Calculate distance between two GPS coordinates
     * @param lat1 Latitude of first point
     * @param lon1 Longitude of first point
     * @param lat2 Latitude of second point
     * @param lon2 Longitude of second point
     * @return Distance in kilometers
     */
    BigDecimal calculateDistance(Double lat1, Double lon1, Double lat2, Double lon2);
    
    /**
     * Validate GPS coordinates
     * @param latitude Latitude to validate
     * @param longitude Longitude to validate
     * @return true if coordinates are valid
     */
    boolean validateCoordinates(Double latitude, Double longitude);
    
    /**
     * Get geocoded address from coordinates
     * @param latitude Latitude
     * @param longitude Longitude
     * @return Address string or null if not found
     */
    String getAddressFromCoordinates(Double latitude, Double longitude);
    
    /**
     * Get coordinates from address
     * @param address Address string
     * @return Array [latitude, longitude] or null if not found
     */
    Double[] getCoordinatesFromAddress(String address);
}