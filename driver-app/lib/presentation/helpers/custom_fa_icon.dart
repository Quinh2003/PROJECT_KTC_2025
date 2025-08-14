import 'package:flutter/widgets.dart';

/// A custom version of the FaIcon that fixes issues with newer Dart versions
class CustomFaIcon extends StatelessWidget {
  /// The icon to display.
  final IconData icon;

  /// The size of the icon in logical pixels.
  final double? size;

  /// The color to use when drawing the icon.
  final Color? color;

  /// The semantic label for the icon.
  final String? semanticLabel;

  /// The text direction to use when rendering the icon.
  final TextDirection? textDirection;

  const CustomFaIcon(
    this.icon, {
    Key? key,
    this.size,
    this.color,
    this.semanticLabel,
    this.textDirection,
  }) : super(key: key);

  @override
  Widget build(BuildContext context) {
    // Use Icon directly instead of FaIcon
    return Icon(
      icon,
      size: size,
      color: color,
      semanticLabel: semanticLabel,
      textDirection: textDirection,
    );
  }
}


