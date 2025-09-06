# Spatial UI Design System Integration Guide

This guide provides instructions for integrating the Spatial UI design system into new and existing Flutter projects for the KTC Logistics ecosystem.

## Environment Setup

### Prerequisites

- Flutter SDK 3.10.0 or higher
- Dart 3.0.0 or higher
- Android Studio / VS Code with Flutter extensions

### Package Installation

Add the Spatial UI package to your `pubspec.yaml`:

```yaml
dependencies:
  spatial_ui:
    git:
      url: git@github.com:ktc-logistics/spatial-ui.git
      ref: main  # or specify a version tag
```

Run `flutter pub get` to install the package.

## Basic Integration

### 1. Import Spatial UI

Add these imports to your Flutter files:

```dart
import 'package:spatial_ui/spatial_theme.dart';
import 'package:spatial_ui/spatial_components.dart';
```

### 2. Initialize Theme

Wrap your application with the Spatial UI theme provider:

```dart
void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'KTC Logistics',
      theme: SpatialTheme.lightTheme,
      darkTheme: SpatialTheme.darkTheme,
      themeMode: ThemeMode.system, // Or .light/.dark
      home: const MyHomePage(),
    );
  }
}
```

### 3. Use Components

Now you can use Spatial UI components in your widgets:

```dart
class MyHomePage extends StatelessWidget {
  const MyHomePage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    // Use a Spatial UI background container
    return SpatialComponents.backgroundContainer(
      useDarkMode: Theme.of(context).brightness == Brightness.dark,
      child: Scaffold(
        // Make scaffold background transparent to show the spatial background
        backgroundColor: Colors.transparent,
        appBar: AppBar(
          title: const Text('KTC Logistics'),
          backgroundColor: Colors.transparent,
          elevation: 0,
        ),
        body: Center(
          child: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              // Use a glass container
              SpatialComponents.glassContainer(
                width: 300,
                padding: const EdgeInsets.all(SpatialTheme.spaceLG),
                child: Column(
                  children: [
                    Text(
                      'Welcome to KTC Logistics',
                      style: SpatialTheme.textTheme.headlineMedium?.copyWith(
                        color: Theme.of(context).brightness == Brightness.dark
                            ? Colors.white
                            : SpatialTheme.textPrimary,
                      ),
                    ),
                    const SizedBox(height: SpatialTheme.spaceMD),
                    // Use a gradient button
                    SpatialComponents.gradientButton(
                      text: 'Get Started',
                      onPressed: () {
                        // Handle button press
                      },
                    ),
                  ],
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }
}
```

## Advanced Integration

### Custom Theming

You can customize the Spatial UI theme for your specific application:

```dart
// Create a custom theme based on Spatial UI
ThemeData customTheme = SpatialTheme.lightTheme.copyWith(
  colorScheme: SpatialTheme.lightTheme.colorScheme.copyWith(
    primary: const Color(0xFF005599), // Custom primary color
  ),
);

// Apply it to your app
MaterialApp(
  theme: customTheme,
  // ...
)
```

### Component Customization

Most Spatial UI components accept customization parameters:

```dart
SpatialComponents.glassContainer(
  // Standard parameters
  child: myContent,
  padding: const EdgeInsets.all(20),
  
  // Customization
  borderRadius: BorderRadius.circular(30), // Custom radius
  blur: 15, // Custom blur amount
  opacity: 0.3, // Custom opacity
  border: Border.all(
    color: Colors.white.withOpacity(0.3),
    width: 2,
  ),
)
```

### Responsive Design

Spatial UI provides utilities for creating responsive layouts:

```dart
// Use the responsive layout utility
SpatialComponents.responsiveLayout(
  mobile: MobileView(),
  tablet: TabletView(),
  desktop: DesktopView(),
)

// Or use the helper method directly
Widget build(BuildContext context) {
  if (SpatialTheme.isMobile(context)) {
    return MobileView();
  } else if (SpatialTheme.isTablet(context)) {
    return TabletView();
  } else {
    return DesktopView();
  }
}
```

## Migration Guide

### From Material Design

If you're migrating from standard Material Design:

1. Replace standard Material components with Spatial UI equivalents:
   - `ElevatedButton` → `SpatialComponents.gradientButton`
   - `Card` → `SpatialComponents.spatialCard`
   - `TextField` → `SpatialComponents.spatialTextField`

2. Update your theme implementation to use Spatial UI themes

3. Add background containers to create the signature Spatial UI look

### From Custom Design

If you're migrating from a custom design:

1. Audit your existing UI components and map them to Spatial UI equivalents

2. Update your color scheme to align with the Spatial UI palette

3. Refactor your widget tree to use Spatial UI components starting with layout containers

## Performance Considerations

Spatial UI uses advanced visual effects that can impact performance on lower-end devices. To optimize:

- Use `SpatialComponents.isLowPerfMode(context)` to check if the device should use simplified effects
- Set `simplifiedEffects: true` on glass components for lower-end devices
- Use `SpatialComponents.conditionalBlur()` to conditionally apply blur effects

## Troubleshooting

### Common Issues

**Glass effect not showing:**
- Ensure you're using a transparent Scaffold background
- Check that you have the latest version of Spatial UI

**Text color issues in dark/light mode:**
- Use the theme-aware text color helpers: `SpatialTheme.getTextColor(context, emphasis)`

**Performance issues:**
- Enable simplified effects for better performance on lower-end devices

### Getting Help

For additional help:

- Check the [Example Gallery](./examples.md) for implementation samples
- Review the [Components Documentation](./components.md) for detailed API references
- Contact the design system team at design-system@ktc-logistics.com

---

© 2025 KTC Logistics | Design System Team
