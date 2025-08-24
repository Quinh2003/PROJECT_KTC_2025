import 'package:flutter/material.dart';

class SpatialBackground extends StatelessWidget {
  final Widget child;
  final Color color;
  final double opacity;

  const SpatialBackground({
    super.key, 
    required this.child, 
    required this.color,
    this.opacity = 0.05,
  });

  @override
  Widget build(BuildContext context) {
    return Stack(
      children: [
        // Custom grid background created with CustomPaint instead of an image
        Positioned.fill(
          child: CustomPaint(
            painter: GridPainter(
              color: color,
              opacity: opacity,
            ),
          ),
        ),
        // Child content
        child,
      ],
    );
  }
}

class GridPainter extends CustomPainter {
  final Color color;
  final double opacity;
  final double gridSize;

  GridPainter({
    required this.color,
    this.opacity = 0.05,
    this.gridSize = 20,
  });

  @override
  void paint(Canvas canvas, Size size) {
    final Paint paint = Paint()
      ..color = color.withOpacity(opacity)
      ..strokeWidth = 0.5
      ..style = PaintingStyle.stroke;

    // Draw horizontal lines
    for (double y = 0; y < size.height; y += gridSize) {
      canvas.drawLine(Offset(0, y), Offset(size.width, y), paint);
    }

    // Draw vertical lines
    for (double x = 0; x < size.width; x += gridSize) {
      canvas.drawLine(Offset(x, 0), Offset(x, size.height), paint);
    }
  }

  @override
  bool shouldRepaint(covariant CustomPainter oldDelegate) => false;
}
