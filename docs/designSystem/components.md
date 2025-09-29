# Components

The Spatial UI component library provides ready-to-use interface elements built on our design system foundations. Each component is carefully crafted for consistency, accessibility, and reusability.

## Glass Container

A versatile container with glassmorphism effects, useful for creating depth and focus in the interface.

![Glass Container](https://via.placeholder.com/500x200/4264FB/FFFFFF?text=Glass+Container)

### Usage

```dart
SpatialComponents.glassContainer(
  child: YourContent(),
  width: double.infinity,
  height: 200,
  padding: const EdgeInsets.all(SpatialTheme.spaceMD),
  borderRadius: SpatialTheme.borderRadiusMedium,
  elevated: true,
  useDarkMode: true,
)
```

### Properties

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| child | Widget | required | Content inside the container |
| width | double? | null | Container width (null = content width) |
| height | double? | null | Container height (null = content height) |
| padding | EdgeInsetsGeometry? | spaceMD all sides | Internal padding |
| margin | EdgeInsetsGeometry? | null | External margin |
| borderRadius | BorderRadius? | borderRadiusMedium | Corner radius |
| elevated | bool | false | Apply elevated shadow |
| useDarkMode | bool | true | Dark mode styling |

### Design Guidelines

- Use for important content that should stand out
- Avoid nesting multiple glass containers
- Keep content simple and readable
- Consider backdrop blur performance on lower-end devices
- Use elevated property sparingly for important elements

## Spatial Card

A card component with depth effects, useful for containing related content.

![Spatial Card](https://via.placeholder.com/500x200/4264FB/FFFFFF?text=Spatial+Card)

### Usage

```dart
SpatialComponents.spatialCard(
  child: YourContent(),
  width: double.infinity,
  backgroundColor: SpatialTheme.surfaceLight,
  glowing: false,
  useDarkMode: true,
)
```

### Properties

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| child | Widget | required | Content inside the card |
| width | double? | null | Card width (null = content width) |
| height | double? | null | Card height (null = content height) |
| padding | EdgeInsetsGeometry? | spaceMD all sides | Internal padding |
| margin | EdgeInsetsGeometry? | null | External margin |
| borderRadius | BorderRadius? | borderRadiusLarge | Corner radius |
| backgroundColor | Color? | surfaceLight/Dark | Background color |
| glowing | bool | false | Apply glow effect |
| useDarkMode | bool | true | Dark mode styling |

### Design Guidelines

- Use for grouping related content
- Maintain adequate spacing between cards
- Consider hierarchy when using multiple cards
- Use glowing effect only for selected or featured cards

## Gradient Button

A customizable button with gradient background and built-in loading state.

![Gradient Button](https://via.placeholder.com/300x56/4264FB/FFFFFF?text=Gradient+Button)

### Usage

```dart
SpatialComponents.gradientButton(
  text: 'Submit',
  onPressed: () { /* action */ },
  icon: Icons.send,
  loading: false,
  disabled: false,
  gradient: SpatialTheme.primaryGradient,
)
```

### Properties

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| text | String | required | Button text |
| onPressed | VoidCallback | required | Action when pressed |
| width | double? | null | Button width (null = content width) |
| height | double? | 56 | Button height |
| loading | bool | false | Show loading spinner |
| disabled | bool | false | Disable button |
| icon | IconData? | null | Optional leading icon |
| gradient | Gradient? | primaryGradient | Button background gradient |

### Design Guidelines

- Use for primary actions in a screen
- Keep text concise (1-3 words ideal)
- Consider using icons to reinforce meaning
- Use loading state for actions that take time
- Use disabled state appropriately to guide users

## Spatial Text Field

A styled text input field with support for validation and various states.

![Spatial Text Field](https://via.placeholder.com/500x80/4264FB/FFFFFF?text=Spatial+Text+Field)

### Usage

```dart
SpatialComponents.spatialTextField(
  label: 'Email Address',
  hint: 'Enter your email',
  controller: emailController,
  prefixIcon: Icons.email,
  keyboardType: TextInputType.emailAddress,
  validator: (value) {
    if (value == null || !value.contains('@')) {
      return 'Please enter a valid email';
    }
    return null;
  },
  useDarkMode: false,
)
```

### Properties

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| label | String | required | Field label |
| hint | String? | null | Placeholder text |
| controller | TextEditingController? | null | Text controller |
| obscureText | bool | false | Hide text (for passwords) |
| prefixIcon | IconData? | null | Icon before text |
| suffixIcon | Widget? | null | Widget after text |
| validator | Function? | null | Validation function |
| keyboardType | TextInputType? | null | Keyboard type |
| enabled | bool | true | Enable/disable field |
| useDarkMode | bool | false | Dark mode styling |

### Design Guidelines

- Use clear, concise labels
- Add helpful hint text for complex inputs
- Use appropriate keyboard types
- Provide clear validation messages
- Group related fields together

## Icon Button

A simple button for icon-based actions.

![Icon Button](https://via.placeholder.com/56x56/4264FB/FFFFFF?text=Icon)

### Usage

```dart
SpatialComponents.iconButton(
  icon: Icons.favorite,
  onPressed: () { /* action */ },
  color: SpatialTheme.primaryBlue,
  elevated: true,
)
```

### Properties

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| icon | IconData | required | Button icon |
| onPressed | VoidCallback | required | Action when pressed |
| color | Color? | textSecondary | Icon color |
| size | double? | 20 | Icon size |
| elevated | bool | false | Apply shadow |

### Design Guidelines

- Use for secondary or tertiary actions
- Keep icon meaning clear and familiar
- Use color to indicate state or importance
- Group related icon buttons together
- Provide tooltips for less common icons

## Status Badge

A badge for displaying status information.

![Status Badge](https://via.placeholder.com/150x30/4264FB/FFFFFF?text=Status)

### Usage

```dart
SpatialComponents.statusBadge(
  text: 'Active',
  backgroundColor: SpatialTheme.success,
  icon: Icons.check_circle,
)
```

### Properties

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| text | String | required | Status text |
| backgroundColor | Color | required | Badge color |
| textColor | Color? | null | Text color (defaults to backgroundColor) |
| icon | IconData? | null | Optional status icon |

### Design Guidelines

- Keep text concise (1-2 words)
- Use semantic colors (success, warning, etc.)
- Be consistent with status naming
- Consider accessibility and color contrast
- Use icons to enhance meaning

## Spatial FAB

A floating action button with spatial design.

![Spatial FAB](https://via.placeholder.com/56x56/4264FB/FFFFFF?text=+)

### Usage

```dart
SpatialComponents.spatialFAB(
  onPressed: () { /* action */ },
  icon: Icons.add,
  mini: false,
)
```

### Properties

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| onPressed | VoidCallback | required | Action when pressed |
| icon | IconData | required | Button icon |
| backgroundColor | Color? | null | Custom background (uses gradient if null) |
| iconColor | Color? | white | Icon color |
| mini | bool | false | Smaller size variant |

### Design Guidelines

- Use for the primary action on a screen
- Place consistently across screens
- Consider collision with other UI elements
- Use mini variant in dense layouts
- Avoid using multiple FABs on a single screen

## Loading Overlay

A modal loading indicator with glass effect.

![Loading Overlay](https://via.placeholder.com/500x300/4264FB/FFFFFF?text=Loading...)

### Usage

```dart
SpatialComponents.loadingOverlay(
  isLoading: isDataLoading,
  child: YourScreenContent(),
  loadingText: 'Loading data...',
)
```

### Properties

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| isLoading | bool | required | Show/hide overlay |
| child | Widget | required | Content behind overlay |
| loadingText | String? | null | Optional explanatory text |

### Design Guidelines

- Use for operations that block UI interaction
- Add descriptive text for longer operations
- Don't use for quick operations (<300ms)
- Consider using inline loading indicators for non-blocking operations

## Bottom Sheet Container

A styled bottom sheet with a drag handle.

![Bottom Sheet](https://via.placeholder.com/500x300/4264FB/FFFFFF?text=Bottom+Sheet)

### Usage

```dart
SpatialComponents.bottomSheetContainer(
  child: YourBottomSheetContent(),
  height: MediaQuery.of(context).size.height * 0.5,
)
```

### Properties

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| child | Widget | required | Bottom sheet content |
| height | double? | null | Custom height (null = content height) |

### Design Guidelines

- Use for supplementary content or actions
- Keep content focused and relevant
- Consider using modal bottom sheets for critical actions
- Support drag-to-dismiss interaction
- Avoid overly tall bottom sheets

## Background Container

A container with gradient background.

![Background Container](https://via.placeholder.com/500x300/4264FB/FFFFFF?text=Background)

### Usage

```dart
SpatialComponents.backgroundContainer(
  child: YourScreenContent(),
  gradient: SpatialTheme.primaryGradient,
  useDarkMode: true,
)
```

### Properties

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| child | Widget | required | Content to display |
| gradient | Gradient? | null | Custom gradient (uses theme if null) |
| useDarkMode | bool | true | Use dark theme gradient if none provided |

### Design Guidelines

- Use as the base container for full screens
- Consider content readability over the gradient
- Be consistent with gradient direction
- Use appropriate theme-based gradients

---

© 2025 KTC Logistics | Design System Team
