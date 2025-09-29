import csv
import time
import requests
import os

INPUT_FILE = "deliverytracking.csv"
OUTPUT_DIR = "sql_output"
LOG_FILE = "process_log.txt"

# tạo thư mục output nếu chưa có
os.makedirs(OUTPUT_DIR, exist_ok=True)

def load_processed_ids():
    """Đọc danh sách ID đã xử lý từ file log"""
    processed_ids = set()
    if os.path.exists(LOG_FILE):
        with open(LOG_FILE, 'r', encoding='utf-8') as f:
            for line in f:
                if line.startswith("✔"):
                    # Trích xuất ID từ dòng log
                    try:
                        id_str = line.split("id=")[1].split()[0]
                        processed_ids.add(int(id_str))
                    except:
                        continue
    return processed_ids

def log_progress(message):
    """Ghi log với timestamp"""
    timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
    log_message = f"[{timestamp}] {message}"
    print(log_message)
    with open(LOG_FILE, 'a', encoding='utf-8') as f:
        f.write(log_message + "\n")

def get_address(lat, lng):
    url = f"https://nominatim.openstreetmap.org/reverse?format=jsonv2&lat={lat}&lon={lng}&addressdetails=1"
    headers = {"User-Agent": "AddressGeocoder/1.0"}
    response = requests.get(url, headers=headers)
    data = response.json()

    if "address" not in data:
        return None

    addr = data["address"]
    return {
        "full": data.get("display_name"),
        "city": addr.get("city") or addr.get("town") or addr.get("village"),
        "state": addr.get("state"),
        "country": addr.get("country"),
        "region": addr.get("county") or addr.get("suburb") or addr.get("region"),
        "postal_code": addr.get("postcode"),
    }

def main():
    updates = []
    file_count = 1
    processed_ids = load_processed_ids()
    log_progress(f"Bắt đầu xử lý. Đã có {len(processed_ids)} địa chỉ được xử lý từ trước.")

    with open(INPUT_FILE, newline='', encoding="utf-8") as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            # Bỏ qua các ID đã xử lý
            if int(row["id"]) in processed_ids:
                continue
            addr_id = row["id"]
            lat = row["latitude"]
            lng = row["longitude"]

            address_info = get_address(lat, lng)
            if address_info:
                sql = f"""UPDATE delivery_tracking 
SET location = '{(address_info["full"] or "").replace("'", "''")}'
WHERE id = {addr_id};"""
                updates.append(sql)
                log_progress(f"✔ id={addr_id} → {address_info['full']}")
            else:
                log_progress(f"⚠ Không tìm thấy địa chỉ cho id={addr_id}")

            # Nominatim giới hạn ~1 request/giây
            time.sleep(1)

            # mỗi 100 dòng -> ghi ra 1 file mới
            if len(updates) == 100:
                output_file = os.path.join(OUTPUT_DIR, f"update_addresses_{file_count}.sql")
                with open(output_file, "w", encoding="utf-8") as f:
                    f.write("\n".join(updates))
                log_progress(f"\n✅ Đã tạo {output_file} ({len(updates)} lệnh UPDATE)")
                file_count += 1
                updates = []

    # ghi nốt phần còn lại (<100)
    if updates:
        output_file = os.path.join(OUTPUT_DIR, f"update_addresses_{file_count}.sql")
        with open(output_file, "w", encoding="utf-8") as f:
            f.write("\n".join(updates))
        log_progress(f"\n✅ Đã tạo {output_file} ({len(updates)} lệnh UPDATE)")
    
    log_progress("Hoàn thành xử lý")

if __name__ == "__main__":
    main()
