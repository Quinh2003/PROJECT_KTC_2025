# Script dọn dẹp nâng cao cho KTC Logistics Driver App
# Dọn dẹp triệt để cache, system temp, disk cleanup và cấu hình thư mục cache
# Script này sẽ tự động xin quyền Administrator nếu cần

# Kiểm tra quyền Administrator và tự động xin quyền nếu cần
$currentPrincipal = New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
$isAdmin = $currentPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

if (-not $isAdmin) {
    Write-Host "Script này yêu cầu quyền Administrator để thực hiện đầy đủ các tác vụ dọn dẹp." -ForegroundColor Yellow
    Write-Host "Đang xin quyền Administrator..." -ForegroundColor Cyan
    
    # Khởi chạy lại script này với quyền admin
    try {
        Start-Process powershell.exe -ArgumentList "-ExecutionPolicy Bypass -File `"$PSCommandPath`"" -Verb RunAs
        exit
    }
    catch {
        Write-Host "Không thể xin quyền Administrator. Vui lòng chạy PowerShell với quyền Administrator." -ForegroundColor Red
        exit 1
    }
}

# Hiển thị banner và mục đích của script
Write-Host "=================================================================" -ForegroundColor Cyan
Write-Host "         KTC LOGISTICS DRIVER APP - CLEAN CACHE ADVANCED         " -ForegroundColor Cyan
Write-Host "=================================================================" -ForegroundColor Cyan
Write-Host "Script này sẽ dọn dẹp TRIỆT ĐỂ tất cả cache, system temp," -ForegroundColor Yellow
Write-Host "disk cleanup và cấu hình lại thư mục cache." -ForegroundColor Yellow
Write-Host "=================================================================" -ForegroundColor Cyan

# Hiển thị dung lượng ổ đĩa trước khi dọn dẹp
Write-Host "Dung lượng ổ đĩa trước khi dọn dẹp:" -ForegroundColor Magenta
Get-PSDrive -PSProvider 'FileSystem' | Format-Table -Property Name, Root, @{Name="Used (GB)"; Expression={[math]::Round($_.Used/1GB, 2)}}, @{Name="Free (GB)"; Expression={[math]::Round($_.Free/1GB, 2)}}

# 1. Cấu hình và chuyển thư mục cache sang ổ D
Write-Host "`n[1/8] Đang cấu hình và chuyển thư mục cache sang ổ D..." -ForegroundColor Green
$gradleUserHome = "D:\gradle_home"
New-Item -Path $gradleUserHome -ItemType Directory -Force | Out-Null
[Environment]::SetEnvironmentVariable("GRADLE_USER_HOME", $gradleUserHome, "User")
$androidSdkHome = "D:\Android\Sdk"
[Environment]::SetEnvironmentVariable("ANDROID_HOME", $androidSdkHome, "User")
[Environment]::SetEnvironmentVariable("ANDROID_SDK_ROOT", $androidSdkHome, "User")
Write-Host "  ✓ Đã cấu hình thư mục cache sang ổ D" -ForegroundColor Gray

# 2. Dọn dẹp Gradle cache
Write-Host "`n[2/8] Đang dọn dẹp Gradle cache..." -ForegroundColor Green
Remove-Item -Path "C:\Users\$env:USERNAME\.gradle\caches\*" -Force -Recurse -ErrorAction SilentlyContinue
Remove-Item -Path "C:\Users\$env:USERNAME\.gradle\daemon\*" -Force -Recurse -ErrorAction SilentlyContinue
Remove-Item -Path "C:\Users\$env:USERNAME\.gradle\wrapper\dists\*" -Force -Recurse -ErrorAction SilentlyContinue
Write-Host "  ✓ Đã dọn dẹp Gradle cache" -ForegroundColor Gray

# 3. Dọn dẹp Android cache
Write-Host "`n[3/8] Đang dọn dẹp Android cache..." -ForegroundColor Green
Remove-Item -Path "C:\Users\$env:USERNAME\.android\build-cache\*" -Force -Recurse -ErrorAction SilentlyContinue
Remove-Item -Path "C:\Users\$env:USERNAME\.android\avd\*.lock" -Force -ErrorAction SilentlyContinue
Remove-Item -Path "C:\Users\$env:USERNAME\.android\debug.keystore" -Force -ErrorAction SilentlyContinue
Write-Host "  ✓ Đã dọn dẹp Android cache" -ForegroundColor Gray

# 4. Dọn dẹp Flutter và Dart cache
Write-Host "`n[4/8] Đang dọn dẹp Flutter và Dart cache..." -ForegroundColor Green
flutter clean
Remove-Item -Path "C:\Users\$env:USERNAME\AppData\Local\Pub\Cache\*" -Force -Recurse -ErrorAction SilentlyContinue
Remove-Item -Path "C:\Users\$env:USERNAME\.pub-cache\*" -Force -Recurse -ErrorAction SilentlyContinue
Write-Host "  ✓ Đã dọn dẹp Flutter và Dart cache" -ForegroundColor Gray

# 5. Dọn dẹp Windows temp folders
Write-Host "`n[5/8] Đang dọn dẹp Windows temp folders..." -ForegroundColor Green
Remove-Item -Path "$env:TEMP\*" -Force -Recurse -ErrorAction SilentlyContinue
Remove-Item -Path "C:\Windows\Temp\*" -Force -Recurse -ErrorAction SilentlyContinue
Remove-Item -Path "C:\`$Recycle.Bin\*" -Force -Recurse -ErrorAction SilentlyContinue
Write-Host "  ✓ Đã dọn dẹp Windows temp folders" -ForegroundColor Gray

# 6. Dọn dẹp project folders
Write-Host "`n[6/8] Đang dọn dẹp project folders..." -ForegroundColor Green
$projectPath = Split-Path -Parent $PSCommandPath
Get-ChildItem -Path $projectPath -Recurse -Directory -Filter "build" | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue
Get-ChildItem -Path $projectPath -Recurse -Directory -Filter ".gradle" | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue
Get-ChildItem -Path $projectPath -Recurse -Directory -Filter ".dart_tool" | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue
Get-ChildItem -Path $projectPath -Recurse -Directory -Filter ".cxx" | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue
Get-ChildItem -Path $projectPath -Recurse -File -Filter "*.iml" | Remove-Item -Force -ErrorAction SilentlyContinue
Get-ChildItem -Path $projectPath -Recurse -File -Filter "*.bak" | Remove-Item -Force -ErrorAction SilentlyContinue
Get-ChildItem -Path $projectPath -Recurse -File -Filter "*.log" | Remove-Item -Force -ErrorAction SilentlyContinue
Write-Host "  ✓ Đã dọn dẹp project folders" -ForegroundColor Gray

# 7. Thiết lập và chạy Disk Cleanup
Write-Host "`n[7/8] Đang thiết lập và chạy Disk Cleanup..." -ForegroundColor Green
# Thiết lập registry để Disk Cleanup tự động xóa tất cả các loại tệp
$registryPath = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\VolumeCaches"
$cleanupTypes = @(
    "Active Setup Temp Folders", "BranchCache", "Downloaded Program Files", 
    "Internet Cache Files", "Memory Dump Files", "Offline Pages Files",
    "Old ChkDsk Files", "Previous Installations", "Recycle Bin",
    "Service Pack Cleanup", "Setup Log Files", "System error memory dump files",
    "System error minidump files", "Temporary Files", "Temporary Setup Files",
    "Temporary Sync Files", "Thumbnail Cache", "Update Cleanup",
    "Upgrade Discarded Files", "User file versions", "Windows Defender",
    "Windows Error Reporting Archive Files", "Windows Error Reporting Queue Files",
    "Windows Error Reporting System Archive Files", "Windows Error Reporting System Queue Files",
    "Windows ESD installation files", "Windows Upgrade Log Files"
)
foreach ($type in $cleanupTypes) {
    $typePath = Join-Path -Path $registryPath -ChildPath $type
    if (Test-Path $typePath) {
        Set-ItemProperty -Path $typePath -Name "StateFlags0001" -Value 2 -Type DWORD -ErrorAction SilentlyContinue
    }
}
# Chạy Disk Cleanup
Start-Process -FilePath "cleanmgr.exe" -ArgumentList "/sagerun:1" -NoNewWindow -Wait -ErrorAction SilentlyContinue
Write-Host "  ✓ Đã chạy Disk Cleanup" -ForegroundColor Gray

# 8. Cập nhật cấu hình Gradle trong dự án
Write-Host "`n[8/8] Đang cập nhật cấu hình Gradle trong dự án..." -ForegroundColor Green
$gradlePropertiesPath = Join-Path -Path $projectPath -ChildPath "android\gradle.properties"
if (Test-Path $gradlePropertiesPath) {
    $gradleProperties = Get-Content $gradlePropertiesPath
    $gradleProperties = $gradleProperties -replace "org.gradle.jvmargs=.*", "org.gradle.jvmargs=-Xmx1536M -XX:MaxMetaspaceSize=512m -XX:+HeapDumpOnOutOfMemoryError -Dfile.encoding=UTF-8"
    $gradleProperties = $gradleProperties -replace "org.gradle.user.home=.*", "org.gradle.user.home=D:/gradle_home"
    
    # Thêm các cấu hình nếu chưa có
    if ($gradleProperties -notcontains "org.gradle.caching=true") {
        $gradleProperties += "org.gradle.caching=true"
    }
    if ($gradleProperties -notcontains "org.gradle.parallel=true") {
        $gradleProperties += "org.gradle.parallel=true"
    }
    if ($gradleProperties -notcontains "org.gradle.daemon=true") {
        $gradleProperties += "org.gradle.daemon=true"
    }
    if ($gradleProperties -notcontains "org.gradle.configureondemand=true") {
        $gradleProperties += "org.gradle.configureondemand=true"
    }
    
    $gradleProperties | Set-Content $gradlePropertiesPath
    Write-Host "  ✓ Đã cập nhật cấu hình Gradle trong dự án" -ForegroundColor Gray
} else {
    Write-Host "  ✓ Không tìm thấy file gradle.properties" -ForegroundColor Gray
}

# Hiển thị dung lượng ổ đĩa sau khi dọn dẹp
Write-Host "`nDung lượng ổ đĩa sau khi dọn dẹp:" -ForegroundColor Magenta
Get-PSDrive -PSProvider 'FileSystem' | Format-Table -Property Name, Root, @{Name="Used (GB)"; Expression={[math]::Round($_.Used/1GB, 2)}}, @{Name="Free (GB)"; Expression={[math]::Round($_.Free/1GB, 2)}}

# Hiển thị thông báo hoàn thành
Write-Host "`n=================================================================" -ForegroundColor Cyan
Write-Host "             DỌN DẸP NÂNG CAO HOÀN TẤT THÀNH CÔNG!             " -ForegroundColor Green
Write-Host "=================================================================" -ForegroundColor Cyan
Write-Host "Đã dọn dẹp TRIỆT ĐỂ tất cả cache, system temp, chạy Disk Cleanup" -ForegroundColor Yellow
Write-Host "và cấu hình lại thư mục cache.`n" -ForegroundColor Yellow
Write-Host "Khuyến nghị: Khởi động lại máy tính để hoàn tất quá trình dọn dẹp." -ForegroundColor Yellow
Write-Host "=================================================================" -ForegroundColor Cyan

# Tạm dừng để người dùng có thể đọc thông báo
Write-Host "`nNhấn phím bất kỳ để đóng cửa sổ này..." -ForegroundColor Gray
$null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
