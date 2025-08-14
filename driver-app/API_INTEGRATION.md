# KTC Logistics Driver App - API Integration Guide

## Overview

This document provides guidance on connecting the KTC Logistics Driver App with the Spring Boot backend using the REST API endpoints.

## API Base URL

The base URL for all API endpoints is configured in `lib/data/env/environment.dart`:

```dart
static const String endpointBase = 'http://10.0.2.2:8080/';
static const String endpointApi = 'http://10.0.2.2:8080/api';
```

> **Note:** `10.0.2.2` is used to access localhost (your development machine) from an Android emulator. For physical devices or iOS simulators, you may need to use your machine's actual IP address.

## Available Endpoints

### Authentication

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/auth/login` | POST | Driver login |
| `/api/auth/logout` | POST | Logout and invalidate token |
| `/api/auth/register` | POST | Register new driver account |

### User & Driver Management

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/users/profile` | GET | Get current driver profile |
| `/api/users/profile` | PUT | Update driver profile |

### Order Management

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/orders` | GET | Get all orders or filtered by status |
| `/api/orders/{id}` | GET | Get order details by ID |
| `/api/orders/{id}/status` | PATCH | Update order status |
| `/api/orders/{id}/tracking` | GET | Get order tracking information |

### Delivery Management

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/deliveries` | GET | Get all deliveries |
| `/api/deliveries/{id}` | GET | Get delivery details |
| `/api/deliveries/{id}/tracking` | GET | Get delivery tracking info |

### Route Management

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/routes` | GET | Get all routes |
| `/api/routes/{id}` | GET | Get route details |
| `/api/routes/{id}/tracking` | GET | Get route tracking information |

### GPS Tracking

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/tracking/update` | POST | Update driver location |
| `/api/tracking/vehicles/{id}` | GET | Get vehicle real-time location |
| `/api/tracking/history` | GET | Get tracking history |

## Authentication Flow

The app uses JWT token-based authentication:

1. **Login**: Call the login endpoint to get a JWT token
2. **Token Storage**: JWT token is securely stored using Flutter Secure Storage
3. **Authenticated Requests**: All protected endpoints require the token in the `Authorization` header
4. **Token Renewal**: If the token expires, the app will prompt for re-login

```dart
// Sample login request
final response = await http.post(
  Uri.parse('${Environment.endpointApi}/auth/login'),
  headers: {'Content-Type': 'application/json'},
  body: jsonEncode({
    'email': email,
    'password': password,
  }),
);
```

## Real-time Communication

The app uses Socket.IO for real-time notifications:

- **Connection URL**: `http://10.0.2.2:8080`
- **Events**: 
  - `new_order`: Notifies when a new order is assigned
  - `order_updated`: Notifies when an order is updated
  - `order_cancelled`: Notifies when an order is cancelled
  - `driver_location`: Updates driver location on the server
  - `driver_status`: Updates driver status on the server

## Connection with Docker

To connect to the Docker container running the Spring Boot backend:

```bash
# Pull the Docker image
docker pull fanglee2003/ktc-logistics-backend

# Run the container
docker run -d -p 8080:8080 fanglee2003/ktc-logistics-backend
```

Ensure the app's environment.dart file points to the correct host:
- For emulators using host machine: `10.0.2.2:8080`
- For physical devices on same network: `<your-machine-ip>:8080`
- For remote Docker deployment: Update with the server's public IP or domain

## API Service Usage

The `ApiService` class in `lib/data/services/api_service.dart` provides methods for all API interactions:

```dart
// Example usage
final apiService = ApiService();

// Login
final authResponse = await apiService.login(email, password);

// Get orders by status
final orders = await apiService.getOrdersByStatus('PENDING');

// Update driver location
await apiService.updateDriverLocation(latitude, longitude);
```

## Troubleshooting

- **401 Unauthorized**: Check if token is valid or expired
- **Connection Errors**: Verify the backend server is running and accessible from device
- **Socket Connection Issues**: Check WebSocket port is open and correctly configured
