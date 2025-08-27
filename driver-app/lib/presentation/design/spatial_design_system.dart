import 'package:flutter/material.dart';
import 'package:google_fonts/google_fonts.dart';

/// Spatial Design System - Modern glassmorphism and depth-based UI
class SpatialTheme {
  // Primary Color Palette
  static const Color primaryBlue = Color(0xFF4264FB);
  static const Color primaryPurple = Color(0xFF8B5DFF);
  static const Color primaryCyan = Color(0xFF00D4FF);
  
  // Gradient Colors
  static const LinearGradient primaryGradient = LinearGradient(
    begin: Alignment.topLeft,
    end: Alignment.bottomRight,
    colors: [primaryBlue, primaryPurple],
  );
  
  static const LinearGradient accentGradient = LinearGradient(
    begin: Alignment.topLeft,
    end: Alignment.bottomRight,
    colors: [primaryCyan, primaryBlue],
  );
  
  // Background Gradients
  static const LinearGradient backgroundGradient = LinearGradient(
    begin: Alignment.topCenter,
    end: Alignment.bottomCenter,
    colors: [
      Color(0xFFF8F9FF),
      Color(0xFFE6F0FF),
    ],
  );
  
  static const LinearGradient darkBackgroundGradient = LinearGradient(
    begin: Alignment.topCenter,
    end: Alignment.bottomCenter,
    colors: [
      Color(0xFF0A0F1C),
      Color(0xFF1A1F2E),
    ],
  );
  
  // Glassmorphism Colors
  static const Color glassLight = Color.fromRGBO(255, 255, 255, 0.25);
  static const Color glassDark = Color.fromRGBO(255, 255, 255, 0.1);
  static const Color glassBorder = Color.fromRGBO(255, 255, 255, 0.2);
  
  // Semantic Colors
  static const Color success = Color(0xFF00C851);
  static const Color warning = Color(0xFFFF8F00);
  static const Color error = Color(0xFFFF4444);
  static const Color info = Color(0xFF33B5E5);
  
  // Text Colors
  static const Color textPrimary = Color(0xFF1A1A1A);
  static const Color textSecondary = Color(0xFF666666);
  static const Color textTertiary = Color(0xFF999999);
  static const Color textLight = Color(0xFFFFFFFF);
  static const Color textColorSecondary = Color(0xFF757575);
  
  // Surface Colors
  static const Color surfaceLight = Color(0xFFFFFFFF);
  static const Color surfaceDark = Color(0xFF1E1E1E);
  static const Color surfaceElevated = Color(0xFFF5F5F5);
  static const Color surfaceColorLight = Color(0xFFFFFFFF);
  
  // Border Colors
  static const Color borderColorLight = Color(0xFFE0E0E0);
  
  // Shadow definitions
  static List<BoxShadow> get spatialShadow => [
    BoxShadow(
      color: primaryBlue.withValues(alpha: 0.1),
      blurRadius: 20,
      spreadRadius: 0,
      offset: const Offset(0, 8),
    ),
    BoxShadow(
      color: Colors.black.withValues(alpha: 0.05),
      blurRadius: 10,
      spreadRadius: 0,
      offset: const Offset(0, 4),
    ),
  ];
  
  static List<BoxShadow> get elevatedShadow => [
    BoxShadow(
      color: primaryBlue.withValues(alpha: 0.15),
      blurRadius: 30,
      spreadRadius: 0,
      offset: const Offset(0, 12),
    ),
    BoxShadow(
      color: Colors.black.withValues(alpha: 0.08),
      blurRadius: 15,
      spreadRadius: 0,
      offset: const Offset(0, 6),
    ),
  ];
  
  static List<BoxShadow> get glowShadow => [
    BoxShadow(
      color: primaryBlue.withValues(alpha: 0.3),
      blurRadius: 25,
      spreadRadius: 0,
      offset: const Offset(0, 0),
    ),
  ];
  
  // Typography
  static TextTheme get textTheme => TextTheme(
    displayLarge: GoogleFonts.inter(
      fontSize: 32,
      fontWeight: FontWeight.w700,
      letterSpacing: -0.5,
      color: textPrimary,
    ),
    displayMedium: GoogleFonts.inter(
      fontSize: 28,
      fontWeight: FontWeight.w600,
      letterSpacing: -0.25,
      color: textPrimary,
    ),
    displaySmall: GoogleFonts.inter(
      fontSize: 24,
      fontWeight: FontWeight.w600,
      color: textPrimary,
    ),
    headlineLarge: GoogleFonts.inter(
      fontSize: 22,
      fontWeight: FontWeight.w600,
      color: textPrimary,
    ),
    headlineMedium: GoogleFonts.inter(
      fontSize: 20,
      fontWeight: FontWeight.w500,
      color: textPrimary,
    ),
    headlineSmall: GoogleFonts.inter(
      fontSize: 18,
      fontWeight: FontWeight.w500,
      color: textPrimary,
    ),
    titleLarge: GoogleFonts.inter(
      fontSize: 16,
      fontWeight: FontWeight.w600,
      color: textPrimary,
    ),
    titleMedium: GoogleFonts.inter(
      fontSize: 14,
      fontWeight: FontWeight.w500,
      color: textPrimary,
    ),
    titleSmall: GoogleFonts.inter(
      fontSize: 12,
      fontWeight: FontWeight.w500,
      color: textSecondary,
    ),
    bodyLarge: GoogleFonts.inter(
      fontSize: 16,
      fontWeight: FontWeight.w400,
      color: textPrimary,
    ),
    bodyMedium: GoogleFonts.inter(
      fontSize: 14,
      fontWeight: FontWeight.w400,
      color: textSecondary,
    ),
    bodySmall: GoogleFonts.inter(
      fontSize: 12,
      fontWeight: FontWeight.w400,
      color: textTertiary,
    ),
    labelLarge: GoogleFonts.inter(
      fontSize: 14,
      fontWeight: FontWeight.w500,
      color: textPrimary,
    ),
    labelMedium: GoogleFonts.inter(
      fontSize: 12,
      fontWeight: FontWeight.w500,
      color: textSecondary,
    ),
    labelSmall: GoogleFonts.inter(
      fontSize: 10,
      fontWeight: FontWeight.w500,
      color: textTertiary,
    ),
  );
  
  // Border Radius
  static const BorderRadius borderRadiusSmall = BorderRadius.all(Radius.circular(8));
  static const BorderRadius borderRadiusMedium = BorderRadius.all(Radius.circular(12));
  static const BorderRadius borderRadiusLarge = BorderRadius.all(Radius.circular(16));
  static const BorderRadius borderRadiusXL = BorderRadius.all(Radius.circular(24));
  
  // Border Radius Values
  static const double radiusSM = 8.0;
  static const double radiusMD = 12.0;
  static const double radiusLG = 16.0;
  static const double radiusXL = 24.0;
  
  // Spacing
  static const double spaceXS = 4;
  static const double spaceSM = 8;
  static const double spaceMD = 16;
  static const double spaceLG = 24;
  static const double spaceXL = 32;
  static const double space2XL = 48;
  static const double space3XL = 64;
  
  // Animation Durations
  static const Duration animationFast = Duration(milliseconds: 150);
  static const Duration animationNormal = Duration(milliseconds: 300);
  static const Duration animationSlow = Duration(milliseconds: 500);
  
  // Theme Data
  static ThemeData get lightTheme => ThemeData(
    useMaterial3: true,
    colorScheme: ColorScheme.fromSeed(
      seedColor: primaryBlue,
      brightness: Brightness.light,
    ),
    textTheme: textTheme,
    scaffoldBackgroundColor: Colors.transparent,
    appBarTheme: const AppBarTheme(
      backgroundColor: Colors.transparent,
      elevation: 0,
      scrolledUnderElevation: 0,
      titleTextStyle: TextStyle(
        fontSize: 18,
        fontWeight: FontWeight.w600,
        color: textPrimary,
      ),
    ),
    elevatedButtonTheme: ElevatedButtonThemeData(
      style: ElevatedButton.styleFrom(
        elevation: 0,
        padding: const EdgeInsets.symmetric(horizontal: 24, vertical: 16),
        shape: RoundedRectangleBorder(borderRadius: borderRadiusMedium),
        textStyle: const TextStyle(
          fontSize: 14,
          fontWeight: FontWeight.w600,
        ),
      ),
    ),
    inputDecorationTheme: InputDecorationTheme(
      filled: true,
      fillColor: surfaceLight.withValues(alpha: 0.8),
      border: OutlineInputBorder(
        borderRadius: borderRadiusMedium,
        borderSide: BorderSide.none,
      ),
      contentPadding: const EdgeInsets.symmetric(horizontal: 16, vertical: 16),
    ),
  );
  
  static ThemeData get darkTheme => ThemeData(
    useMaterial3: true,
    colorScheme: ColorScheme.fromSeed(
      seedColor: primaryBlue,
      brightness: Brightness.dark,
    ),
    textTheme: textTheme.apply(
      bodyColor: textLight,
      displayColor: textLight,
    ),
    scaffoldBackgroundColor: Colors.transparent,
    appBarTheme: const AppBarTheme(
      backgroundColor: Colors.transparent,
      elevation: 0,
      scrolledUnderElevation: 0,
      titleTextStyle: TextStyle(
        fontSize: 18,
        fontWeight: FontWeight.w600,
        color: textLight,
      ),
    ),
  );
}

/// Status Colors for different states
class StatusColors {
  static const Color pending = Color(0xFFFFA726);
  static const Color inProgress = Color(0xFF42A5F5);
  static const Color completed = Color(0xFF66BB6A);
  static const Color cancelled = Color(0xFFEF5350);
  static const Color onHold = Color(0xFFFF7043);
}

/// Delivery Status Colors
class DeliveryStatusColors {
  static const Color pickup = Color(0xFF9C27B0);
  static const Color inTransit = Color(0xFF2196F3);
  static const Color delivered = Color(0xFF4CAF50);
  static const Color failed = Color(0xFFF44336);
  static const Color returned = Color(0xFFFF9800);
}


