##############################################
# Fix Gradle Crash Script
# This script helps recover from Gradle daemon crashes
# by stopping all daemons and clearing temporary files
##############################################

Write-Host "===========================================" -ForegroundColor Cyan
Write-Host "    GRADLE CRASH RECOVERY TOOL" -ForegroundColor Cyan
Write-Host "===========================================" -ForegroundColor Cyan
Write-Host ""

# Step 1: Stop all running Gradle daemons
Write-Host "Step 1: Stopping all Gradle daemons..." -ForegroundColor Yellow
Push-Location D:\GithubProjects\PROJECT_KTC_2025\flutter-project\android
./gradlew --stop
Pop-Location
Write-Host "All Gradle daemons stopped." -ForegroundColor Green
Write-Host ""

# Step 2: Clean up any .lock files in the .gradle directory
Write-Host "Step 2: Cleaning up lock files..." -ForegroundColor Yellow
$gradleHome = "D:/gradle_home"
$lockFiles = Get-ChildItem -Path $gradleHome -Filter "*.lock" -Recurse -ErrorAction SilentlyContinue
if ($lockFiles) {
    Write-Host "Found $($lockFiles.Count) lock files to remove." -ForegroundColor Magenta
    foreach ($file in $lockFiles) {
        Remove-Item $file.FullName -Force
        Write-Host "Removed: $($file.FullName)" -ForegroundColor DarkGray
    }
} else {
    Write-Host "No lock files found." -ForegroundColor Green
}
Write-Host ""

# Step 3: Clean the project
Write-Host "Step 3: Cleaning Flutter project..." -ForegroundColor Yellow
Push-Location D:\GithubProjects\PROJECT_KTC_2025\flutter-project
flutter clean
Pop-Location
Write-Host "Flutter project cleaned." -ForegroundColor Green
Write-Host ""

# Step 4: Clear Gradle and Flutter caches
Write-Host "Step 4: Cleaning Flutter pub cache for plugins..." -ForegroundColor Yellow

# Always clean the Flutter plugin cache to avoid path conflicts
$flutterPubCache = "$env:USERPROFILE\AppData\Local\Pub\Cache"
if (Test-Path $flutterPubCache) {
    Write-Host "Cleaning Flutter plugin cache at: $flutterPubCache" -ForegroundColor Magenta
    Push-Location D:\GithubProjects\PROJECT_KTC_2025\flutter-project
    flutter pub cache clean
    Pop-Location
    Write-Host "Flutter plugin cache cleaned." -ForegroundColor Green
} else {
    Write-Host "Flutter plugin cache not found at expected location." -ForegroundColor Yellow
}
Write-Host ""

# Ask about Gradle cache
$clearCache = Read-Host "Do you want to clear Gradle caches? This will remove downloaded dependencies (y/n)"
if ($clearCache -eq "y") {
    Write-Host "Clearing Gradle caches..." -ForegroundColor Yellow
    $cachePaths = @(
        "$gradleHome/caches/modules-2",
        "$gradleHome/caches/transforms-3",
        "$gradleHome/daemon"
    )
    foreach ($path in $cachePaths) {
        if (Test-Path $path) {
            Remove-Item -Path $path -Recurse -Force -ErrorAction SilentlyContinue
            Write-Host "Cleared: $path" -ForegroundColor DarkGray
        }
    }
    Write-Host "Gradle caches cleared." -ForegroundColor Green
    Write-Host ""
}

# Step 5: Remind about memory settings
Write-Host "Memory settings in gradle.properties have been updated to:" -ForegroundColor Yellow
Write-Host "  - Max Heap: 4096M (was 1536M)" -ForegroundColor White
Write-Host "  - Max Metaspace: 1024M (was 512M)" -ForegroundColor White
Write-Host ""

# Step 6: Fix path conflict issues with location plugin
Write-Host "Step 6: Fixing location plugin path issues..." -ForegroundColor Yellow
$locationPluginPath = "$env:USERPROFILE\AppData\Local\Pub\Cache\hosted\pub.dev\location-5.0.3\android"
if (Test-Path $locationPluginPath) {
    Write-Host "Location plugin found at: $locationPluginPath" -ForegroundColor Magenta
    # Remove the compiled Kotlin caches that have path conflicts
    $locationKotlinCachePath = "$locationPluginPath\build\kotlin"
    if (Test-Path $locationKotlinCachePath) {
        Remove-Item -Path $locationKotlinCachePath -Recurse -Force -ErrorAction SilentlyContinue
        Write-Host "Removed Kotlin cache with path conflicts at: $locationKotlinCachePath" -ForegroundColor Green
    }
    
    # Also clean the .gradle folder in the plugin
    $locationGradleCachePath = "$locationPluginPath\.gradle"
    if (Test-Path $locationGradleCachePath) {
        Remove-Item -Path $locationGradleCachePath -Recurse -Force -ErrorAction SilentlyContinue
        Write-Host "Removed .gradle cache in location plugin at: $locationGradleCachePath" -ForegroundColor Green
    }
} else {
    Write-Host "Location plugin not found at expected path. This is OK if you're not using this plugin." -ForegroundColor Yellow
}
Write-Host ""

# Step 7: Prepare for rebuild
Write-Host "Step 7: Getting Flutter dependencies..." -ForegroundColor Yellow
Push-Location D:\GithubProjects\PROJECT_KTC_2025\flutter-project
flutter pub get
Pop-Location
Write-Host "Flutter dependencies updated." -ForegroundColor Green
Write-Host ""

Write-Host "===========================================" -ForegroundColor Cyan
Write-Host "    RECOVERY COMPLETE" -ForegroundColor Cyan
Write-Host "===========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "You can now try building your Flutter project again." -ForegroundColor Green
Write-Host "Run the following command to rebuild:" -ForegroundColor Yellow
Write-Host "  cd D:\GithubProjects\PROJECT_KTC_2025\flutter-project" -ForegroundColor White
Write-Host "  flutter build apk --debug" -ForegroundColor White
Write-Host ""
Write-Host "If you continue to experience issues, consider:" -ForegroundColor Yellow
Write-Host "1. Increasing memory settings further" -ForegroundColor White
Write-Host "2. Simplifying the Android build process" -ForegroundColor White
Write-Host "3. Updating Gradle and Flutter" -ForegroundColor White
Write-Host ""
