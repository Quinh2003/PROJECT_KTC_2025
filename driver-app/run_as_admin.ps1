# Chạy script clean_advanced.ps1 với quyền Administrator

$scriptPath = Join-Path -Path $PSScriptRoot -ChildPath "clean_advanced.ps1"

# Kiểm tra xem script có tồn tại không
if (-not (Test-Path $scriptPath)) {
    Write-Host "Không tìm thấy script clean_advanced.ps1!" -ForegroundColor Red
    exit 1
}

# Tạo process với quyền admin
Start-Process powershell.exe -ArgumentList "-ExecutionPolicy Bypass -File `"$scriptPath`"" -Verb RunAs

Write-Host "Đã khởi chạy script dọn dẹp nâng cao với quyền Administrator..." -ForegroundColor Green
