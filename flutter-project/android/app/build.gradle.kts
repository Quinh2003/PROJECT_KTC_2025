plugins {
    id("com.android.application")
    id("kotlin-android")
    // The Flutter Gradle Plugin must be applied after the Android and Kotlin Gradle plugins.
    id("dev.flutter.flutter-gradle-plugin")
    // Google Services plugin for Firebase
    id("com.google.gms.google-services")
}

android {
    namespace = "com.ktc.logistics_driver"
    compileSdk = 35
    // compileSdk = 36
    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_1_8
        targetCompatibility = JavaVersion.VERSION_1_8
        isCoreLibraryDesugaringEnabled = true
    }

    kotlinOptions {
        jvmTarget = JavaVersion.VERSION_1_8.toString()
    }

    defaultConfig {
        // TODO: Specify your own unique Application ID (https://developer.android.com/studio/build/application-id.html).
        applicationId = "com.ktc.logistics_driver"
        // You can update the following values to match your application needs.
        // For more information, see: https://flutter.dev/to/review-gradle-config.
        minSdk = 24
        targetSdk = 35
        versionCode = flutter.versionCode
        versionName = flutter.versionName
    }

    buildTypes {
        release {
            // TODO: Add your own signing config for the release build.
            // Signing with the debug keys for now, so `flutter run --release` works.
            signingConfig = signingConfigs.getByName("debug")
        }
        debug {
            signingConfig = signingConfigs.getByName("debug")
        }
    }

    // Cấu hình đặc biệt để đảm bảo APK được đặt vào thư mục Flutter mong đợi
    applicationVariants.all {
        val variant = this
        variant.outputs.all {
            // QUAN TRỌNG: Không đổi tên APK để Flutter có thể tìm thấy nó dễ dàng
            // Bỏ việc tùy chỉnh tên file APK để sử dụng tên mặc định
            // Các file này sẽ được copy với tên chính xác trong task riêng
        }
    }
}

dependencies {
    coreLibraryDesugaring("com.android.tools:desugar_jdk_libs:2.1.4")

    // Firebase BoM để đảm bảo version compatibility
    implementation(platform("com.google.firebase:firebase-bom:34.1.0"))

    // Firebase products
    implementation("com.google.firebase:firebase-analytics")
    implementation("com.google.firebase:firebase-auth")
    implementation("com.google.firebase:firebase-firestore")
    implementation("com.google.firebase:firebase-storage")
    implementation("com.google.firebase:firebase-messaging")
    implementation("com.google.firebase:firebase-database")
}

flutter {
    source = "../.."
}

// Thêm task copy APK sau khi build hoàn tất
tasks.register("copyApkToFlutterDir") {
    doLast {
        // Sử dụng đường dẫn chính xác của Flutter
        val sourceDir = "${layout.buildDirectory.asFile.get()}/outputs/apk"
        val targetDir = "${rootProject.projectDir}/../build/app/outputs/flutter-apk"
        
        // Tạo thư mục đích nếu chưa tồn tại
        mkdir(targetDir)
        
        // QUAN TRỌNG: Flutter tìm kiếm các file có tên cụ thể, không phải tên tùy chỉnh
        // Copy debug APK với tên mà Flutter mong đợi
        val debugApk = file("${sourceDir}/debug")
        if (debugApk.exists()) {
            debugApk.listFiles()?.firstOrNull { it.name.endsWith(".apk") }?.let { apk ->
                copy {
                    from(apk)
                    into(targetDir)
                    rename { "app-debug.apk" }  // Flutter tìm kiếm chính xác tên này
                }
                println("Copied debug APK to ${targetDir}/app-debug.apk")
            }
        }
        
        // Copy release APK nếu có
        val releaseApk = file("${sourceDir}/release")
        if (releaseApk.exists()) {
            releaseApk.listFiles()?.firstOrNull { it.name.endsWith(".apk") }?.let { apk ->
                copy {
                    from(apk)
                    into(targetDir)
                    rename { "app-release.apk" }  // Flutter tìm kiếm chính xác tên này
                }
                println("Copied release APK to ${targetDir}/app-release.apk")
            }
        }
    }
}

// Gắn task copy vào sau khi build
tasks.whenTaskAdded {
    if (name.contains("assemble") || name.contains("package")) {
        finalizedBy("copyApkToFlutterDir")
    }
}

// Task để hiển thị vị trí APK - hữu ích cho debugging
tasks.register("showApkLocations") {
    doLast {
        println("======== APK LOCATION DEBUG INFO ========")
        println("Standard build location: ${layout.buildDirectory.asFile.get()}/outputs/apk")
        println("Flutter expected location: ${rootProject.projectDir}/../build/app/outputs/flutter-apk")
        println("Flutter expected files: app-debug.apk, app-release.apk")
        println("=========================================")
        
        // Kiểm tra APK mà Flutter đang tìm
        val flutterDebugApk = file("${rootProject.projectDir}/../build/app/outputs/flutter-apk/app-debug.apk")
        if (flutterDebugApk.exists()) {
            println("✓ Flutter debug APK found: ${flutterDebugApk.absolutePath}")
        } else {
            println("✗ Flutter debug APK missing at: ${flutterDebugApk.absolutePath}")
        }
        
        // Liệt kê các APK hiện có để debug
        println("Other APK files in build folders:")
        fileTree(rootProject.projectDir.parentFile) {
            include("**/build/**/*.apk")
            exclude("**/build/app/outputs/flutter-apk/app-debug.apk") // Đã kiểm tra ở trên
            exclude("**/build/app/outputs/flutter-apk/app-release.apk") // Đã kiểm tra ở trên
        }.forEach { file ->
            println("- ${file.absolutePath}")
        }
    }
}

// Thêm hook đặc biệt cho Flutter build process
afterEvaluate {
    tasks.named("assembleDebug").configure {
        finalizedBy("copyApkToFlutterDir", "showApkLocations")
    }
    
    tasks.named("assembleRelease").configure {
        finalizedBy("copyApkToFlutterDir", "showApkLocations")
    }
}