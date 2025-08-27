# Script dọn dẹp thông thường cho KTC Logistics Driver App
# Dọn dẹp cache, build folders và tối ưu không gian lưu trữ
# Chạy file này với PowerShell thông thường

# Hiển thị banner và mục đích của script
Write-Host "=================================================================" -ForegroundColor Cyan
Write-Host "             KTC LOGISTICS DRIVER APP - CLEAN CACHE              " -ForegroundColor Cyan
Write-Host "=================================================================" -ForegroundColor Cyan
Write-Host "Script này sẽ dọn dẹp các file cache, build folders và giải phóng" -ForegroundColor Yellow
Write-Host "không gian lưu trữ cho dự án. Thích hợp để chạy thường xuyên." -ForegroundColor Yellow
Write-Host "=================================================================" -ForegroundColor Cyan

# Hiển thị dung lượng ổ đĩa trước khi dọn dẹp
Write-Host "Dung lượng ổ đĩa trước khi dọn dẹp:" -ForegroundColor Magenta
Get-PSDrive -PSProvider 'FileSystem' | Format-Table -Property Name, Root, @{Name="Used (GB)"; Expression={[math]::Round($_.Used/1GB, 2)}}, @{Name="Free (GB)"; Expression={[math]::Round($_.Free/1GB, 2)}}

# 1. Dọn dẹp build folders và cache của Flutter
Write-Host "`n[1/5] Đang dọn dẹp Flutter cache và build folders..." -ForegroundColor Green
flutter clean
Write-Host "  ✓ Đã dọn dẹp Flutter build folders" -ForegroundColor Gray

# 2. Dọn dẹp Gradle cache trong project (cẩn thận để giữ lại các file quan trọng)
Write-Host "`n[2/5] Đang dọn dẹp Gradle cache trong project..." -ForegroundColor Green
$projectPath = Get-Location

# Tạo backup của settings.gradle.kts nếu tồn tại
$androidDir = Join-Path -Path $projectPath -ChildPath "android"
$settingsGradleKts = Join-Path -Path $androidDir -ChildPath "settings.gradle.kts"
$settingsGradle = Join-Path -Path $androidDir -ChildPath "settings.gradle"

if (Test-Path $settingsGradleKts) {
    Copy-Item $settingsGradleKts "$settingsGradleKts.bak" -Force
}
if (Test-Path $settingsGradle) {
    Copy-Item $settingsGradle "$settingsGradle.bak" -Force
}

# Dọn dẹp .gradle trong project nhưng không xóa thư mục android
Get-ChildItem -Path $projectPath -Recurse -Directory -Filter ".gradle" | 
    Where-Object { $_.FullName -notlike "*\android\*" } | 
    Remove-Item -Recurse -Force -ErrorAction SilentlyContinue

# Dọn dẹp các thư mục build nhưng không xóa thư mục android hoàn toàn
Get-ChildItem -Path $projectPath -Recurse -Directory -Filter "build" | 
    Where-Object { $_.FullName -notlike "*\android\*" } | 
    Remove-Item -Recurse -Force -ErrorAction SilentlyContinue

# Xóa thư mục transforms trong Gradle cache của người dùng hiện tại để khắc phục lỗi metadata
Write-Host "  ! Đang xóa thư mục transforms trong Gradle cache để khắc phục lỗi metadata.bin..." -ForegroundColor Yellow
$transformsDirs = @(
    "C:\Users\$env:USERNAME\.gradle\caches\*\transforms*",
    "C:\Users\$env:USERNAME\.gradle\caches\transforms-*",
    "D:\gradle_home\caches\*\transforms*",
    "D:\gradle_home\caches\transforms-*"
)

foreach ($dir in $transformsDirs) {
    Remove-Item -Path $dir -Force -Recurse -ErrorAction SilentlyContinue
}

# Khôi phục settings.gradle.kts từ backup nếu có
if (Test-Path "$settingsGradleKts.bak") {
    Copy-Item "$settingsGradleKts.bak" $settingsGradleKts -Force
    Remove-Item "$settingsGradleKts.bak" -Force
}
if (Test-Path "$settingsGradle.bak") {
    Copy-Item "$settingsGradle.bak" $settingsGradle -Force
    Remove-Item "$settingsGradle.bak" -Force
}

Write-Host "  ✓ Đã dọn dẹp Gradle cache trong project (bảo vệ các file quan trọng)" -ForegroundColor Gray

# 3. Dọn dẹp thư mục .dart_tool và các file tạm của Flutter
Write-Host "`n[3/5] Đang dọn dẹp .dart_tool và các file tạm của Flutter..." -ForegroundColor Green
Get-ChildItem -Path $projectPath -Recurse -Directory -Filter ".dart_tool" | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue
Remove-Item -Path "$projectPath\.flutter-plugins" -Force -ErrorAction SilentlyContinue
Remove-Item -Path "$projectPath\.flutter-plugins-dependencies" -Force -ErrorAction SilentlyContinue
Remove-Item -Path "$projectPath\.packages" -Force -ErrorAction SilentlyContinue
Write-Host "  ✓ Đã dọn dẹp .dart_tool và các file tạm của Flutter" -ForegroundColor Gray

# 4. Dọn dẹp Android build cache
Write-Host "`n[4/5] Đang dọn dẹp Android build cache..." -ForegroundColor Green
$androidDir = Join-Path -Path $projectPath -ChildPath "android"
Get-ChildItem -Path $androidDir -Recurse -Directory -Filter ".cxx" | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue
Get-ChildItem -Path $androidDir -Recurse -File -Filter "*.iml" | Remove-Item -Force -ErrorAction SilentlyContinue
Get-ChildItem -Path $androidDir -Recurse -File -Filter "*.bak" | Remove-Item -Force -ErrorAction SilentlyContinue
Get-ChildItem -Path "$androidDir\.kotlin\errors" -File -Filter "*.log" -ErrorAction SilentlyContinue | Remove-Item -Force -ErrorAction SilentlyContinue
Write-Host "  ✓ Đã dọn dẹp Android build cache" -ForegroundColor Gray

# 5. Dọn dẹp iOS build cache
Write-Host "`n[5/5] Đang dọn dẹp iOS build cache..." -ForegroundColor Green
$iosDir = Join-Path -Path $projectPath -ChildPath "ios"
if (Test-Path $iosDir) {
    Get-ChildItem -Path $iosDir -Recurse -Directory -Filter "Pods" | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue
    Get-ChildItem -Path $iosDir -Recurse -Directory -Filter "DerivedData" | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue
    Write-Host "  ✓ Đã dọn dẹp iOS build cache" -ForegroundColor Gray
} else {
    Write-Host "  ✓ Không tìm thấy thư mục iOS" -ForegroundColor Gray
}

# Hiển thị dung lượng ổ đĩa sau khi dọn dẹp
Write-Host "`nDung lượng ổ đĩa sau khi dọn dẹp:" -ForegroundColor Magenta
Get-PSDrive -PSProvider 'FileSystem' | Format-Table -Property Name, Root, @{Name="Used (GB)"; Expression={[math]::Round($_.Used/1GB, 2)}}, @{Name="Free (GB)"; Expression={[math]::Round($_.Free/1GB, 2)}}

# Khôi phục các dependencies
Write-Host "`n[+] Đang khôi phục các dependencies..." -ForegroundColor Green
flutter pub get
Write-Host "  ✓ Đã khôi phục dependencies thành công" -ForegroundColor Gray

# Kiểm tra và khôi phục thư mục android nếu cần
$androidDir = Join-Path -Path (Get-Location) -ChildPath "android"
if (-not (Test-Path $androidDir)) {
    Write-Host "`n[!] Thư mục android không tồn tại, đang khôi phục..." -ForegroundColor Yellow
    flutter create --platforms=android .
    Write-Host "  ✓ Đã khôi phục thư mục android thành công" -ForegroundColor Gray
}

# Hiển thị thông báo hoàn thành
Write-Host "`n=================================================================" -ForegroundColor Cyan
Write-Host "             DỌN DẸP HOÀN TẤT THÀNH CÔNG!                      " -ForegroundColor Green
Write-Host "=================================================================" -ForegroundColor Cyan
Write-Host "Đã dọn dẹp thành công các file cache và build folders.`n" -ForegroundColor Yellow
Write-Host "Bạn có thể tiếp tục phát triển dự án bình thường." -ForegroundColor Yellow
Write-Host "Để dọn dẹp sâu hơn, hãy chạy script 'clean_advanced.ps1' với quyền Admin." -ForegroundColor Yellow
Write-Host "=================================================================" -ForegroundColor Cyan
