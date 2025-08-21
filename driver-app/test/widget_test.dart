// This is a basic Flutter widget test.
//
// To perform an interaction with a widget in your test, use the WidgetTester
// utility in the flutter_test package. For example, you can send tap and scroll
// gestures. You can also use WidgetTester to find child widgets in the widget
// tree, read text, and verify that the values of widget properties are correct.

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';

import 'package:ktc_logistics_driver/presentation/screens/auth/spatial_login_screen.dart';

void main() {
  testWidgets('LoginScreen widget smoke test', (WidgetTester tester) async {
    // Build LoginScreen widget and trigger a frame.
    await tester.pumpWidget(
      MaterialApp(
        home: SpatialLoginScreen(),
      ),
    );

    // Verify that the login screen loads without errors
    expect(find.byType(SpatialLoginScreen), findsOneWidget);
    
    // Check if login form elements exist
    expect(find.byType(TextFormField), findsWidgets);
    expect(find.text('Log In'), findsAtLeastNWidgets(1));
  });
}
