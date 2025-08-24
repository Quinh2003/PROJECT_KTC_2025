pluginManagement {
    val flutterSdkPath = run {
        val properties = java.util.Properties()
        file("local.properties").inputStream().use { properties.load(it) }
        val flutterSdkPath = properties.getProperty("flutter.sdk")
        require(flutterSdkPath != null) { "flutter.sdk not set in local.properties" }
        flutterSdkPath
    }

    includeBuild("$flutterSdkPath/packages/flutter_tools/gradle")

    repositories {
        // Explicitly add Maven Central and Google repositories first for plugin resolution
        mavenCentral()
        google()
        gradlePluginPortal()
        // Add Flutter's own Maven repository
        maven {
            url = uri("$flutterSdkPath/bin/cache/artifacts/engine/android-gradle-plugin")
        }
    }
}

// Clean the transforms folder to avoid metadata corruption
gradle.beforeProject {
    val cacheDir = gradle.gradleUserHomeDir.resolve("caches")
    if (cacheDir.exists()) {
        val transformsDir = cacheDir.resolve("transforms")
        if (transformsDir.exists() && transformsDir.isDirectory) {
            println("Cleaning Gradle transforms cache to avoid metadata corruption")
            transformsDir.deleteRecursively()
        }
    }
}

plugins {
    id("dev.flutter.flutter-plugin-loader") version "1.0.0"
    id("com.android.application") version "8.7.3" apply false
    id("org.jetbrains.kotlin.android") version "2.1.0" apply false
}

include(":app")