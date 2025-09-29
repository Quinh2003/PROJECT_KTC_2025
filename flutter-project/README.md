# 📱 KTC Logistics Management Platform - Driver Mobile App

**FastRoute** is a mobile application for delivery drivers, developed with Flutter and featuring a modern **Spatial UI**. Built on **Clean Architecture** principles, the app is easy to maintain, extend, and test, with clear separation between UI, business logic, and data layers.

## 📋 Table of Contents

1. [Quick Start](#-quick-start)
2. [Main Features](#-main-features)
3. [Tech Stack & Clean Architecture](#-tech-stack--clean-architecture)
4. [Project Configuration](#project-configuration)
5. [Troubleshooting](#-troubleshooting)
6. [License & Contact](#-license--contact)

## ⚡ Quick Start

Follow these steps to quickly test the app with a local backend:

### 1. Setup Environment
- Ensure your phone and computer are connected to the **same WiFi network**
- Download the latest APK from [this](https://drive.google.com/drive/folders/1xaxwYYIBaBgO1-hCYK8MLx2tlR_SYVew?usp=sharing).

### 2. Find Computer's IP Address
- On the computer running the backend, open Command Prompt/PowerShell
- Run `ipconfig` and look for the IPv4 address (e.g., 192.168.1.5)
- Note this IP address as you'll need it for configuration

### 3. Start Backend Server
```bash
cd PROJECT_KTC_2025/spring-project
./gradlew bootRun
```
- Wait until you see "Started Application" message

### 4. Install and Configure the App
- Install the APK on your Android device
- Launch the app
- Select "Test Environment"
- Enter the API URL: `http://[YOUR_COMPUTER_IP]:8080/api` (e.g., `http://192.168.1.5:8080/api`)
- Save the configuration

### 5. Test the App
- Use the test credentials:
  - Email: `driver@gmail.com`
  - Password: `123456`
- Verify basic features work correctly:
  - Dashboard loads with test data
  - Map displays correctly
  - Delivery list shows test orders
  - Driver can update delivery status

### 6. Troubleshooting

- If login fails, ensure the backend is running and IP is correct
- Check if the device can reach the backend by opening `http://[YOUR_COMPUTER_IP]:8080/actuator/health` in the phone browser
- If connection fails, try using port forwarding with: `adb reverse tcp:8080 tcp:8080`
- If needed, disable firewall or add exception for port 8080

## 🚀 Main Features

### 📦 Order Management
- Receive new orders in real time
- View detailed delivery information
- Update order status (in progress, completed)
- Delivery history

### 🗺️ Navigation & Maps
- Integrated MapBox with GPS tracking and route optimization
- Display pickup, delivery locations and turn-by-turn navigation
- Google Maps integration with multiple stops
- Background location tracking

### 📊 Data Visualization & Analytics
- Dashboard analytics with delivery performance overview
- Multiple chart types: Area, Bar, Pie, Scatter, Bubble, and Radar Chart
- Light/Dark mode for all charts

### 🔧 Vehicle Maintenance
- Create emergency maintenance requests
- Track maintenance status
- Receive notifications when vehicle is ready
- View maintenance history

## 🛠️ Tech Stack & Clean Architecture

### Frontend Technologies
- **Flutter**: 3.32.8 (latest stable)
- **Dart**: 3.8.1
- **State Management**: BLoC Pattern
- **UI Components**: Material Design 3
- **Charts**: fl_chart library (3.0.0+)

### Backend Integration
- **HTTP Client**: Dart HTTP package
- **Real-time**: Socket.IO client
- **Caching**: Flutter Secure Storage

### Maps & Location
- **MapBox**: MapBox Flutter SDK
- **GPS**: Geolocator package
- **Geocoding**: Geocoding package

### Android Configuration
- **SDK**: 35 (Android 14)
- **Java**: 21 (LTS)
- **Min SDK**: 24 (Android 7.0)

### Clean Architecture Layers

The app is built following **Clean Architecture** with 3 main layers:

#### 🎯 Domain Layer
- Models, Repository Interfaces, Use Cases, and BLoC

#### 📊 Data Layer
- Repository Implementations, Data Sources, and DTOs

#### 🎨 Presentation Layer
- Screens, Widgets, State Management, and Chart Components

#### ⚙️ Data Flow & Dependency Injection
- UI → BLoC → Domain → Data → External Sources
- GetIt service locator for dependency inversion

## Project Configuration

### Prerequisites
- **Flutter SDK**: 3.32.8 
- **Dart**: 3.8.1
- **Android Studio**: Latest version
- **Gradle**: 8.14.3
- **Android SDK**: API 35 (Android 14)

### Setup and Run Project

```bash
# Clone and install dependencies
cd PROJECT_KTC_2025/driver-app
flutter pub get

# Run app on emulator with baseUrl = "http://10.0.2.2:8000"
flutter run

# Run app on real device with baseUrl = "http://localhost:8000"
adb reverse tcp:8080 tcp:8080  # Port forwarding for localhost
cd ../spring-project && ./gradlew bootRun  # Start backend (in another terminal)
cd ../flutter-project && flutter run  # Run app

# Build release APK
flutter build apk --release
flutter install
```

### Test Accounts

#### 🔒 **Real backend account**
- **Email**: `driver_01@fr.com`
- **Password**: `123456`

#### 🔌 **Offline mode account**
- **Email**: `driver_offline@ktc.com`
- **Password**: `123456`

> **Note**: The app is currently using mock data to demo features.

## 🔧 Troubleshooting

### Connect real device

If you get `adb: command not found` error:

1. **Install Android SDK Platform-tools**:
   - Install via Android Studio > SDK Manager > SDK Tools
   - Or download directly from: https://developer.android.com/tools/releases/platform-tools

2. **Add to PATH**:
   ```
   setx PATH "%PATH%;C:\Users\<username>\AppData\Local\Android\Sdk\platform-tools" /M
   ```

3. **Check device connection**:
   ```
   adb devices
   ```

4. **Check backend connection**:
   Open browser on device and go to: `http://localhost:8080/actuator/health`

### Common Issues

```bash
# Gradle build fails
cd android && ./gradlew clean
cd .. && flutter clean && flutter pub get

# Maps not showing
flutter run -v  # to see detailed error
```

---

## 📄 License & Contact

Copyright © 2025 KTC Logistics. All rights reserved.

For technical issues or support:
- Open an issue in the repository for quick assistance
- Email: support@ktclogistics.com
- Project Lead: mobile-team@ktclogistics.com