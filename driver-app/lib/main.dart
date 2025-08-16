import 'package:flutter/material.dart';

// App
import 'app.dart';

// Dependency Injection
import 'injection/dependency_injection.dart';

void main() async {
  WidgetsFlutterBinding.ensureInitialized();
  
  // Setup dependency injection
  await setupDependencyInjection();
  
  runApp(const App());
}


