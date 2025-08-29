# Patterns

UI patterns are established solutions to common design problems. The Spatial UI design system includes a set of recommended patterns for creating consistent, intuitive interfaces.

## Navigation

### Bottom Navigation

![Bottom Navigation](https://via.placeholder.com/500x100/4264FB/FFFFFF?text=Bottom+Navigation)

The primary navigation pattern for the KTC Logistics app, providing access to main sections.

#### Implementation Guidelines

```dart
SpatialComponents.glassContainer(
  child: BottomNavigationBar(
    items: [
      BottomNavigationBarItem(
        icon: Icon(Icons.home),
        label: 'Home',
      ),
      // Other navigation items
    ],
    currentIndex: _selectedIndex,
    onTap: _onItemTapped,
    // Styling properties
  ),
  borderRadius: BorderRadius.vertical(top: Radius.circular(SpatialTheme.radiusLG)),
)
```

#### Best Practices

- Limit to 3-5 navigation items
- Use clear, recognizable icons
- Keep labels short and descriptive
- Highlight the active section clearly
- Consider using labels for clarity
- Use the glass container effect for visual distinction
- Implement proper safe area insets for different devices

### App Bar Navigation

![App Bar](https://via.placeholder.com/500x80/4264FB/FFFFFF?text=App+Bar)

Used for screen-level navigation, providing context and actions.

#### Implementation Guidelines

```dart
SpatialComponents.glassContainer(
  child: AppBar(
    title: Text('Screen Title'),
    leading: IconButton(
      icon: Icon(Icons.arrow_back),
      onPressed: () => Navigator.pop(context),
    ),
    actions: [
      // Action buttons
    ],
    backgroundColor: Colors.transparent,
    elevation: 0,
  ),
  borderRadius: BorderRadius.vertical(bottom: Radius.circular(SpatialTheme.radiusMD)),
)
```

#### Best Practices

- Maintain consistent positioning across screens
- Include back navigation when appropriate
- Limit action buttons to 2-3 important functions
- Use clear, recognizable icons for actions
- Consider collapsing behavior for scrolling content
- Adapt title length for different screen sizes
- Use transparent background with the glass container

## Lists & Collections

### Card List

![Card List](https://via.placeholder.com/500x300/4264FB/FFFFFF?text=Card+List)

A common pattern for displaying collections of items with visual hierarchy.

#### Implementation Guidelines

```dart
ListView.builder(
  itemCount: items.length,
  padding: EdgeInsets.all(SpatialTheme.spaceMD),
  itemBuilder: (context, index) {
    return Padding(
      padding: EdgeInsets.only(bottom: SpatialTheme.spaceMD),
      child: SpatialComponents.spatialCard(
        child: ListTile(
          title: Text(items[index].title),
          subtitle: Text(items[index].description),
          leading: Icon(items[index].icon),
          trailing: Icon(Icons.chevron_right),
          onTap: () => _onItemTap(items[index]),
        ),
      ),
    );
  },
)
```

#### Best Practices

- Use consistent spacing between cards
- Include clear tap/interaction areas
- Consider skeleton loading states during data fetch
- Implement proper empty and error states
- Use appropriate list padding for device sizes
- Optimize performance for long lists

### Grid Layout

![Grid Layout](https://via.placeholder.com/500x400/4264FB/FFFFFF?text=Grid+Layout)

Used for displaying collections where visual content is primary.

#### Implementation Guidelines

```dart
GridView.builder(
  gridDelegate: SliverGridDelegateWithFixedCrossAxisCount(
    crossAxisCount: 2,
    childAspectRatio: 1.0,
    crossAxisSpacing: SpatialTheme.spaceMD,
    mainAxisSpacing: SpatialTheme.spaceMD,
  ),
  padding: EdgeInsets.all(SpatialTheme.spaceMD),
  itemCount: items.length,
  itemBuilder: (context, index) {
    return SpatialComponents.spatialCard(
      child: Column(
        children: [
          Image.network(items[index].imageUrl),
          Padding(
            padding: EdgeInsets.all(SpatialTheme.spaceSM),
            child: Text(items[index].title),
          ),
        ],
      ),
    );
  },
)
```

#### Best Practices

- Adjust grid columns based on screen size
- Maintain consistent aspect ratios
- Use proper image loading and error states
- Consider lazy loading for performance
- Implement proper item spacing

## Forms & Data Entry

### Form Layout

![Form Layout](https://via.placeholder.com/500x400/4264FB/FFFFFF?text=Form+Layout)

A structured approach to collecting user input through forms.

#### Implementation Guidelines

```dart
SpatialComponents.spatialCard(
  child: Form(
    key: _formKey,
    child: Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        SpatialComponents.spatialTextField(
          label: 'Email',
          hint: 'Enter your email',
          controller: _emailController,
          keyboardType: TextInputType.emailAddress,
          validator: _validateEmail,
        ),
        SizedBox(height: SpatialTheme.spaceMD),
        SpatialComponents.spatialTextField(
          label: 'Password',
          hint: 'Enter your password',
          controller: _passwordController,
          obscureText: true,
          validator: _validatePassword,
        ),
        SizedBox(height: SpatialTheme.spaceLG),
        SpatialComponents.gradientButton(
          text: 'Submit',
          onPressed: _submitForm,
          width: double.infinity,
        ),
      ],
    ),
  ),
)
```

#### Best Practices

- Group related fields together
- Order fields in a logical sequence
- Provide clear validation feedback
- Use appropriate keyboard types
- Consider form progress indication for multi-step forms
- Implement proper error handling
- Save form state during navigation when appropriate

### Search & Filter

![Search & Filter](https://via.placeholder.com/500x200/4264FB/FFFFFF?text=Search+&+Filter)

Patterns for helping users find and filter content.

#### Implementation Guidelines

```dart
Column(
  children: [
    SpatialComponents.glassContainer(
      child: Row(
        children: [
          Expanded(
            child: TextField(
              controller: _searchController,
              decoration: InputDecoration(
                hintText: 'Search...',
                prefixIcon: Icon(Icons.search),
                border: InputBorder.none,
              ),
              onChanged: _onSearchChanged,
            ),
          ),
          IconButton(
            icon: Icon(Icons.filter_list),
            onPressed: _showFilters,
          ),
        ],
      ),
      padding: EdgeInsets.symmetric(horizontal: SpatialTheme.spaceSM),
    ),
    // Filter chips if active
    if (_activeFilters.isNotEmpty)
      Container(
        height: 40,
        margin: EdgeInsets.only(top: SpatialTheme.spaceSM),
        child: ListView.separated(
          scrollDirection: Axis.horizontal,
          itemCount: _activeFilters.length,
          separatorBuilder: (context, index) => SizedBox(width: SpatialTheme.spaceSM),
          itemBuilder: (context, index) {
            return SpatialComponents.statusBadge(
              text: _activeFilters[index],
              backgroundColor: SpatialTheme.primaryBlue,
              icon: Icons.check,
            );
          },
        ),
      ),
  ],
)
```

#### Best Practices

- Provide clear search input with appropriate keyboard
- Show active filters visibly
- Allow clearing search and filters easily
- Consider recent searches where appropriate
- Implement proper empty and no results states
- Provide feedback during search operations

## Feedback & Notifications

### Toast Messages

![Toast Messages](https://via.placeholder.com/500x100/4264FB/FFFFFF?text=Toast+Message)

Brief notifications that appear temporarily.

#### Implementation Guidelines

```dart
void showToast(String message, {bool isError = false}) {
  final OverlayState overlayState = Overlay.of(context);
  final OverlayEntry overlayEntry = OverlayEntry(
    builder: (context) => Positioned(
      bottom: 50.0,
      left: 20.0,
      right: 20.0,
      child: SpatialComponents.glassContainer(
        child: Padding(
          padding: EdgeInsets.symmetric(vertical: SpatialTheme.spaceSM),
          child: Row(
            children: [
              Icon(
                isError ? Icons.error : Icons.check_circle,
                color: isError ? SpatialTheme.error : SpatialTheme.success,
              ),
              SizedBox(width: SpatialTheme.spaceSM),
              Expanded(
                child: Text(message),
              ),
            ],
          ),
        ),
        borderRadius: SpatialTheme.borderRadiusMedium,
      ),
    ),
  );

  overlayState.insert(overlayEntry);
  Future.delayed(Duration(seconds: 3)).then((_) {
    overlayEntry.remove();
  });
}
```

#### Best Practices

- Keep messages concise and actionable
- Use appropriate icons to reinforce meaning
- Position consistently across the app
- Consider using different styles for different message types
- Implement proper timing (2-4 seconds typically)
- Allow dismissing by tapping

### Dialog Boxes

![Dialog Boxes](https://via.placeholder.com/400x300/4264FB/FFFFFF?text=Dialog)

Modal windows for important decisions or information.

#### Implementation Guidelines

```dart
void showSpatialDialog({
  required String title,
  required String message,
  required VoidCallback onConfirm,
  VoidCallback? onCancel,
  String confirmText = 'Confirm',
  String cancelText = 'Cancel',
}) {
  showDialog(
    context: context,
    builder: (context) => Dialog(
      backgroundColor: Colors.transparent,
      child: SpatialComponents.spatialCard(
        child: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            Text(
              title,
              style: SpatialTheme.textTheme.headlineMedium,
            ),
            SizedBox(height: SpatialTheme.spaceMD),
            Text(
              message,
              style: SpatialTheme.textTheme.bodyMedium,
            ),
            SizedBox(height: SpatialTheme.spaceLG),
            Row(
              mainAxisAlignment: MainAxisAlignment.end,
              children: [
                if (onCancel != null)
                  TextButton(
                    onPressed: () {
                      Navigator.pop(context);
                      onCancel();
                    },
                    child: Text(cancelText),
                  ),
                SizedBox(width: SpatialTheme.spaceSM),
                SpatialComponents.gradientButton(
                  text: confirmText,
                  onPressed: () {
                    Navigator.pop(context);
                    onConfirm();
                  },
                ),
              ],
            ),
          ],
        ),
        padding: EdgeInsets.all(SpatialTheme.spaceLG),
      ),
    ),
  );
}
```

#### Best Practices

- Use for important decisions or blocking information
- Keep content focused and relevant
- Provide clear actions with descriptive labels
- Consider using different styles for different dialog types
- Ensure proper keyboard handling
- Support dismissing for non-critical dialogs

## Data Visualization

### Progress Indicators

![Progress Indicators](https://via.placeholder.com/500x100/4264FB/FFFFFF?text=Progress+Indicators)

Visual representations of process completion or loading states.

#### Implementation Guidelines

```dart
// Linear progress
SpatialComponents.glassContainer(
  child: Column(
    crossAxisAlignment: CrossAxisAlignment.start,
    children: [
      Row(
        mainAxisAlignment: MainAxisAlignment.spaceBetween,
        children: [
          Text('Uploading files'),
          Text('${(_progress * 100).toInt()}%'),
        ],
      ),
      SizedBox(height: SpatialTheme.spaceSM),
      LinearProgressIndicator(
        value: _progress,
        backgroundColor: SpatialTheme.glassLight,
        valueColor: AlwaysStoppedAnimation<Color>(SpatialTheme.primaryBlue),
      ),
    ],
  ),
  padding: EdgeInsets.all(SpatialTheme.spaceMD),
)

// Circular progress
SpatialComponents.spatialCard(
  child: Column(
    children: [
      CircularProgressIndicator(
        valueColor: AlwaysStoppedAnimation<Color>(SpatialTheme.primaryBlue),
        strokeWidth: 5,
      ),
      SizedBox(height: SpatialTheme.spaceMD),
      Text('Processing order'),
    ],
  ),
  padding: EdgeInsets.all(SpatialTheme.spaceLG),
)
```

#### Best Practices

- Use linear progress for processes with known duration
- Use circular progress for indeterminate operations
- Provide context about what's happening
- Consider skeleton loaders for content loading
- Implement proper timeout handling
- Show progress percentage when known

### Status Timeline

![Status Timeline](https://via.placeholder.com/500x200/4264FB/FFFFFF?text=Status+Timeline)

Visual representation of process steps and current status.

#### Implementation Guidelines

```dart
SpatialComponents.spatialCard(
  child: Column(
    children: [
      _buildTimelineStep(
        title: 'Order Placed',
        subtitle: 'May 12, 10:30 AM',
        isCompleted: true,
        isFirst: true,
      ),
      _buildTimelineStep(
        title: 'Processing',
        subtitle: 'May 12, 11:45 AM',
        isCompleted: true,
      ),
      _buildTimelineStep(
        title: 'Out for Delivery',
        subtitle: 'May 13, 09:15 AM',
        isCompleted: true,
      ),
      _buildTimelineStep(
        title: 'Delivered',
        subtitle: 'Estimated: May 13, 02:00 PM',
        isCompleted: false,
        isLast: true,
      ),
    ],
  ),
)

Widget _buildTimelineStep({
  required String title,
  required String subtitle,
  required bool isCompleted,
  bool isFirst = false,
  bool isLast = false,
}) {
  return Row(
    children: [
      Column(
        children: [
          Container(
            width: 20,
            height: 20,
            decoration: BoxDecoration(
              color: isCompleted ? SpatialTheme.success : SpatialTheme.textTertiary,
              shape: BoxShape.circle,
            ),
            child: isCompleted ? Icon(Icons.check, size: 12, color: Colors.white) : null,
          ),
          if (!isLast)
            Container(
              width: 2,
              height: 30,
              color: isCompleted ? SpatialTheme.success : SpatialTheme.textTertiary,
            ),
        ],
      ),
      SizedBox(width: SpatialTheme.spaceMD),
      Expanded(
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              title,
              style: SpatialTheme.textTheme.titleMedium,
            ),
            Text(
              subtitle,
              style: SpatialTheme.textTheme.bodySmall,
            ),
            if (!isLast) SizedBox(height: SpatialTheme.spaceMD),
          ],
        ),
      ),
    ],
  );
}
```

#### Best Practices

- Show clear progression between steps
- Indicate current status prominently
- Include timestamps when relevant
- Use consistent spacing between steps
- Consider horizontal layouts for limited steps
- Use colors and icons to indicate completion

---

© 2025 KTC Logistics | Design System Team
