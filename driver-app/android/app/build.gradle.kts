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
    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_21
        targetCompatibility = JavaVersion.VERSION_21
        isCoreLibraryDesugaringEnabled = true
    }

    kotlinOptions {
        jvmTarget = JavaVersion.VERSION_21.toString()
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
            // Chuyển APK đến thư mục Flutter tìm kiếm
            val outputFileName = "app-${variant.buildType.name}.apk"
            val flutterOutputDir = "$rootDir/../build/app/outputs/flutter-apk"
            val debugOutputDir = "$rootDir/app/build/outputs/apk/${variant.buildType.name}"

            // Fix doLast issue by using tasks.named approach
            val outputId = this.name
            tasks.named("package${variant.name.capitalize()}").configure {
                doLast {
                    copy {
                        from("$debugOutputDir/$outputFileName")
                        into(flutterOutputDir)
                    }
                }
            }
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
        // Fix for deprecated buildDir
        val sourceDir = "${layout.buildDirectory.asFile.get()}/outputs/apk"
        val targetDir = "${rootProject.projectDir}/../build/app/outputs/flutter-apk"

        // Tạo thư mục đích nếu chưa tồn tại
        mkdir(targetDir)

        // Copy APK debug
        val debugApk = file("${sourceDir}/debug/app-debug.apk")
        if (debugApk.exists()) {
            copy {
                from(debugApk)
                into(targetDir)
            }
            println("Copied debug APK to ${targetDir}/app-debug.apk")
        }

        // Copy APK release
        val releaseApk = file("${sourceDir}/release/app-release.apk")
        if (releaseApk.exists()) {
            copy {
                from(releaseApk)
                into(targetDir)
            }
            println("Copied release APK to ${targetDir}/app-release.apk")
        }
    }
}

// Gắn task copy vào sau khi build
tasks.whenTaskAdded {
    if (name.contains("assemble")) {
        finalizedBy("copyApkToFlutterDir")
    }
}