# Optimized build script
# This script handles common build failures and provides better error reporting

# Function to check disk space
function Check-DiskSpace {
    $cDrive = Get-PSDrive C
    $freeSpaceGB = [math]::Round($cDrive.Free / 1GB, 2)
    
    if ($freeSpaceGB -lt 2) {
        Write-Host "WARNING: Only $freeSpaceGB GB available on C: drive." -ForegroundColor Red
        Write-Host "This is likely not enough for building. Consider freeing up at least 2-3 GB." -ForegroundColor Yellow
        $continue = Read-Host "Do you want to continue anyway? (y/n)"
        if ($continue -ne "y") {
            exit 1
        }
    } else {
        Write-Host "Disk space check passed: $freeSpaceGB GB available on C: drive." -ForegroundColor Green
    }
}

# Clean previous builds
function Clean-Project {
    Write-Host "Cleaning project..." -ForegroundColor Cyan
    flutter clean
    
    # Remove Android build folders specifically
    if (Test-Path "android\app\build") {
        Remove-Item -Recurse -Force "android\app\build"
    }
    
    if (Test-Path "android\.gradle") {
        Remove-Item -Recurse -Force "android\.gradle"
    }
}

# Get dependencies
function Get-Dependencies {
    Write-Host "Getting dependencies..." -ForegroundColor Cyan
    flutter pub get
}

# Build the APK with proper error handling
function Build-Apk {
    param(
        [string]$buildType = "debug"
    )
    
    Write-Host "Building $buildType APK..." -ForegroundColor Cyan
    
    # Build with different strategies based on build type
    if ($buildType -eq "debug") {
        flutter build apk --debug --verbose
    } else {
        flutter build apk --release --verbose
    }
    
    # Check specific locations based on build type
    if ($buildType -eq "debug") {
        $apkPath = "build\app\outputs\flutter-apk\app-debug.apk"
    } else {
        $apkPath = "build\app\outputs\flutter-apk\app-release.apk"
    }
    
    # Verify the build was successful
    if (Test-Path $apkPath) {
        Write-Host "Build successful! APK available at: $apkPath" -ForegroundColor Green
        return $true
    } else {
        Write-Host "APK not found at expected location: $apkPath" -ForegroundColor Red
        Write-Host "Looking for APK in alternative locations..." -ForegroundColor Yellow
        
        # Check alternative locations
        $altPath1 = "android\app\build\outputs\apk\$buildType\app-$buildType.apk"
        $altPath2 = "build\app\outputs\apk\$buildType\app-$buildType.apk"
        
        if (Test-Path $altPath1) {
            Write-Host "Found APK at: $altPath1" -ForegroundColor Green
            # Create directory if it doesn't exist
            $targetDir = Split-Path $apkPath -Parent
            if (!(Test-Path $targetDir)) {
                New-Item -ItemType Directory -Path $targetDir -Force
            }
            # Copy to expected location
            Copy-Item $altPath1 $apkPath -Force
            Write-Host "Copied APK to: $apkPath" -ForegroundColor Green
            return $true
        } elseif (Test-Path $altPath2) {
            Write-Host "Found APK at: $altPath2" -ForegroundColor Green
            return $true
        } else {
            Write-Host "No APK found in any expected location." -ForegroundColor Red
            return $false
        }
    }
}

# Main script execution
try {
    # Step 1: Check disk space
    Check-DiskSpace
    
    # Step 2: Clean
    Clean-Project
    
    # Step 3: Get dependencies
    Get-Dependencies
    
    # Step 4: Build APK (debug by default)
    $buildType = "debug"
    if ($args.Count -gt 0 -and $args[0] -eq "release") {
        $buildType = "release"
    }
    
    $result = Build-Apk -buildType $buildType
    
    if ($result) {
        exit 0
    } else {
        exit 1
    }
} catch {
    Write-Host "Error during build process:" -ForegroundColor Red
    Write-Host $_.Exception.Message -ForegroundColor Red
    exit 1
}
