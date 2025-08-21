# Spatial UI Design System

A modern, glass-effect UI design system for the KTC Logistics Driver App. This design system provides a cohesive set of visual elements, components, and patterns to create a consistent user experience, inspired by Apple Vision Pro's spatial UI concepts.

## Overview

The Spatial UI design system introduces a modern, immersive interface featuring glass effects, subtle gradients, and depth through layering. It's designed to make the KTC Logistics driver app more intuitive, visually appealing, and functionally effective for delivery drivers operating in Vietnam's busy urban environments.

## Key Features

### Glass Effect Components

- **GlassCard**: A versatile container with backdrop blur, transparency, and subtle borders
- **SpatialButton**: Configurable buttons with multiple styles (solid, outlined, glass, gradient)
- **SpatialTextField**: Text inputs with glass effect and floating labels
- **SpatialAppBar**: Custom app bar with glass effect and dynamic theming
- **SpatialBottomSheet**: Bottom sheet with glass effect and custom animations

### Color System

- Primary palette based on KTC Logistics blue (#4264FB) with complementary accent colors
- Dark and light theme support with appropriate contrast ratios for Vietnam's varied lighting conditions
- Semantic colors for success, warning, error, and information states
- Context-aware color adaptation for navigation, orders, and delivery statuses

### Typography

- Clear hierarchy with headings, body text, captions, and other text styles
- Responsive text scaling for different device sizes
- Optimized for readability in various lighting conditions
- Support for Vietnamese characters and language-specific typography rules
- Font families selected for optimal rendering on mobile devices

### Spacing and Layout

- Consistent spacing scales for margins, padding, and layout
- Border radius system for visual consistency
- Grid-based layouts for alignment and proportion
- Adaptive layouts for different screen sizes and orientations
- Optimized for single-handed operation common for delivery drivers

## Example Screens

The design system has been implemented in several key screens of the KTC Logistics Driver App:

### Order Detail Screen

- Tabbed interface with Details, Route, and Timeline views
- Status card with delivery progress
- Customer and delivery information cards with glass effect
- Timeline visualization of the delivery process with active states
- Quick actions for driver communication and delivery confirmation

### Route Map Screen

- Real-time navigation interface optimized for Vietnamese traffic conditions
- Glass effect info panels with distance, ETA, and directions
- Navigation controls with glass effect buttons
- Simulated navigation with step-by-step directions in Vietnamese
- Traffic alerts and route optimization suggestions

### Authentication Screens

- Spatial login screen with animated transitions
- Registration flow with image picker and form validation
- Password recovery and email verification screens
- Biometric authentication support for quick access
- Onboarding tutorial with interactive elements

## Usage

To use the Spatial UI design system in your KTC Logistics Driver App screens:

1. Import the design system:

```dart
import '../../design/spatial_ui.dart';
```

1. Use the color constants and text styles:

```dart
Container(
  color: SpatialDesignSystem.backgroundColor,
  child: Text(
    'Đơn hàng mới', // New order
    style: SpatialDesignSystem.headingMedium,
  ),
)
```

1. Implement glass effect components:

```dart
GlassCard(
  padding: const EdgeInsets.all(16),
  child: Column(
    children: [
      Text('Thông tin khách hàng', style: SpatialDesignSystem.subtitleMedium), // Customer Information
      const SizedBox(height: 8),
      Text('Nguyễn Văn A - 0912345678', style: SpatialDesignSystem.bodyMedium),
    ],
  ),
)
```

1. Use SpatialButton for actions:

```dart
SpatialButton(
  text: 'Xác nhận giao hàng', // Confirm Delivery
  onPressed: () {
    // Action here
  },
  iconData: Icons.check,
)

SpatialButton(
  text: 'Liên hệ khách hàng', // Contact Customer
  onPressed: () {
    // Action here
  },
  isOutlined: true,
)

SpatialButton(
  text: 'Chỉ đường', // Get Directions
  onPressed: () {
    // Action here
  },
  isGlass: true,
  iconData: Icons.directions,
)
```

1. Add animations to components:

```dart
SpatialCard(
  child: Text('Thông báo mới'), // New notification
).animate()
 .fadeIn(duration: 500.ms)
 .slideY(begin: 0.2, end: 0, curve: Curves.easeOutQuad);
```

### Responsive Design

- Adaptive layouts for different Vietnamese smartphone models
- Consistent experience across various screen sizes and pixel densities
- Dynamic spacing that adjusts to screen dimensions
- Orientation support for both portrait and landscape modes
- Optimized for both phone and tablet form factors

### Performance Considerations

- Efficient backdrop filter usage to maintain 60fps even on mid-range devices
- Conditional rendering of effects based on device capabilities
- Optimized asset loading and caching
- Reduced shader compilation jank through prewarming
- Battery usage optimizations for all-day driver usage

## Demo and Documentation

To see the KTC Logistics Spatial UI design system in action, run the spatial_ui_demo.dart file:

```bash
flutter run -t lib/presentation/design/demo/spatial_ui_demo.dart
```

This will launch a demo app that showcases all components including:

- Order Detail Screen with delivery timeline
- Route Map Screen with turn-by-turn navigation
- Authentication flows with spatial effects
- Component showcase with interactive examples

### Version History

- **v1.0.0** (August 2025) - Initial release with core components
- **v1.1.0** (Planned September 2025) - Advanced map integration and route optimization
- **v1.2.0** (Planned October 2025) - Offline support and performance enhancements

### Screenshots

![KTC Logistics Order Screen](assets/docs/order_screen.png)
![KTC Logistics Route Screen](assets/docs/route_screen.png)
![KTC Logistics Component Demo](assets/docs/components.png)

For complete documentation, visit the [KTC Logistics Design System Documentation](https://ktc-logistics.com/design-system) (internal link).
