# Spatial UI Design System

A modern, glass-effect UI design system for the KTC Logistics Driver App. This design system provides a cohesive set of visual elements, components, and patterns to create a consistent user experience.

## Overview

The Spatial UI design system introduces a modern, immersive interface featuring glass effects, subtle gradients, and depth through layering. It's designed to make the driver app more intuitive, visually appealing, and functionally effective.

## Key Features

### Glass Effect Components

- **GlassCard**: A versatile container with backdrop blur, transparency, and subtle borders
- **SpatialButton**: Configurable buttons with multiple styles (solid, outlined, glass, gradient)
- **SpatialTextField**: Text inputs with glass effect and floating labels

### Color System

- Primary palette based on blue (#4264FB) with complementary accent colors
- Dark and light theme support with appropriate contrast ratios
- Semantic colors for success, warning, error, and information states

### Typography

- Clear hierarchy with headings, body text, captions, and other text styles
- Responsive text scaling for different device sizes
- Optimized for readability in various lighting conditions

### Spacing and Layout

- Consistent spacing scales for margins, padding, and layout
- Border radius system for visual consistency
- Grid-based layouts for alignment and proportion

## Example Screens

The design system has been implemented in several key screens:

### Order Detail Screen

- Tabbed interface with Details, Route, and Timeline views
- Status card with delivery progress
- Customer and delivery information cards
- Timeline visualization of the delivery process

### Route Map Screen

- Real-time navigation interface
- Glass effect info panels with distance, ETA, and directions
- Navigation controls with glass effect buttons
- Simulated navigation with step-by-step directions

## Usage

To use the Spatial UI design system in your screens:

1. Import the design system:

```dart
import '../design/spatial_ui.dart';
```

2. Use the color constants and text styles:

```dart
Container(
  color: SpatialDesignSystem.backgroundColor,
  child: Text(
    'Hello World',
    style: SpatialDesignSystem.headingMedium,
  ),
)
```

3. Implement glass effect components:

```dart
GlassCard(
  padding: const EdgeInsets.all(16),
  child: Column(
    children: [
      Text('Card Title', style: SpatialDesignSystem.subtitleMedium),
      const SizedBox(height: 8),
      Text('Card content goes here', style: SpatialDesignSystem.bodyMedium),
    ],
  ),
)
```

4. Use SpatialButton for actions:

```dart
SpatialButton(
  text: 'Primary Action',
  onPressed: () {
    // Action here
  },
  iconData: Icons.check,
)

SpatialButton(
  text: 'Secondary Action',
  onPressed: () {
    // Action here
  },
  isOutlined: true,
)

SpatialButton(
  text: 'Glass Button',
  onPressed: () {
    // Action here
  },
  isGlass: true,
)
```

## Demo

To see the Spatial UI design system in action, run the spatial_ui_demo.dart file:

```bash
flutter run -t lib/spatial_ui_demo.dart
```

This will launch a demo app that showcases the Order Detail Screen and Route Map Screen with the new design system.
