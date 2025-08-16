# Build APK and ensure it's in the correct location for Flutter
Write-Host "Building APK..."

# Run flutter build
flutter build apk --debug

# Check if APK was built
$apkSource = "android\app\build\outputs\apk\debug\app-debug.apk"
$apkTarget = "build\app\outputs\flutter-apk\app-debug.apk"

if (Test-Path $apkSource) {
    Write-Host "APK built successfully at: $apkSource"
    
    # Ensure target directory exists
    $targetDir = Split-Path $apkTarget -Parent
    if (!(Test-Path $targetDir)) {
        New-Item -ItemType Directory -Force -Path $targetDir | Out-Null
    }
    
    # Copy APK to Flutter's expected location
    Copy-Item $apkSource $apkTarget -Force
    Write-Host "APK copied to Flutter's expected location: $apkTarget"
    Write-Host "Build completed successfully!"
} else {
    Write-Host "ERROR: APK not found at expected location: $apkSource"
    exit 1
}
