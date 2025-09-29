import csv
import time
import requests
import os

INPUT_FILE = "deliverytracking.csv"
OUTPUT_DIR = "sql_output6"
os.makedirs(OUTPUT_DIR, exist_ok=True)


# Đảm bảo các thư mục output và process_log tồn tại
LOG_DIR = 'process_log3'
os.makedirs(OUTPUT_DIR, exist_ok=True)
os.makedirs(LOG_DIR, exist_ok=True)

def get_log_files():
    """Trả về danh sách file log process_log/process_log_*.txt đã sắp xếp theo số tăng dần"""
    logs = [f for f in os.listdir(LOG_DIR) if f.startswith('process_log_') and f.endswith('.txt')]
    logs_with_num = []
    for fname in logs:
        try:
            num = int(fname.replace('process_log_', '').replace('.txt', ''))
            logs_with_num.append((num, fname))
        except Exception:
            continue
    logs_with_num.sort()
    return [os.path.join(LOG_DIR, fname) for _, fname in logs_with_num]

def get_latest_log_file():
    logs = get_log_files()
    if logs:
        return logs[-1], int(logs[-1].split('_')[-1].replace('.txt',''))
    else:
        return None, 0

def get_oldest_log_file():
    logs = get_log_files()
    if logs:
        return logs[0]
    else:
        return None
    logs = [f for f in os.listdir(LOG_DIR) if f.startswith('process_log_') and f.endswith('.txt')]
    max_num = 0
    for fname in logs:
        try:
            num = int(fname.replace('process_log_', '').replace('.txt', ''))
            if num > max_num:
                max_num = num
        except Exception:
            continue
    if max_num > 0:
        return os.path.join(LOG_DIR, f'process_log_{max_num}.txt'), max_num
    else:
        return None, 0

def get_next_log_file():
    _, max_num = get_latest_log_file()
    return os.path.join(LOG_DIR, f'process_log_{max_num+1}.txt')


def load_last_processed_ids(log_file, n=3):
    """Chỉ lấy 2-3 dòng cuối có id đã xử lý từ file log"""
    max_id = None
    if log_file and os.path.exists(log_file):
        with open(log_file, 'r', encoding='utf-8') as f:
            lines = f.readlines()
        # Lấy các dòng cuối có chứa '✔ id='
        last_lines = [line for line in lines if '✔ id=' in line][-n:]
        for line in last_lines:
            try:
                id_str = line.split('✔ id=')[1].split()[0]
                id_val = int(id_str)
                if (max_id is None) or (id_val > max_id):
                    max_id = id_val
            except Exception:
                continue
    return max_id


def log_progress(message, log_file):
    """Ghi log với timestamp vào file log chỉ định"""
    timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
    log_message = f"[{timestamp}] {message}"
    print(log_message)
    with open(log_file, 'a', encoding='utf-8') as f:
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
    # Tìm số file lớn nhất hiện có trong OUTPUT_DIR để đặt file_count tiếp theo
    existing_files = [f for f in os.listdir(OUTPUT_DIR) if f.startswith("update_addresses_") and f.endswith(".sql")]
    max_count = 0
    for fname in existing_files:
        try:
            num = int(fname.replace("update_addresses_", "").replace(".sql", ""))
            if num > max_count:
                max_count = num
        except Exception:
            continue
    file_count = max_count + 1


    # Lấy file log mới nhất (số lớn nhất) và tạo file log mới cho lần chạy này
    latest_log, _ = get_latest_log_file()
    LOG_FILE = get_next_log_file()

    # Đọc toàn bộ checkpoint từ file log mới nhất
    def load_all_processed_ids(log_file):
        processed_ids = set()
        if log_file and os.path.exists(log_file):
            with open(log_file, 'r', encoding='utf-8') as f:
                for line in f:
                    if '✔ id=' in line:
                        try:
                            id_str = line.split('✔ id=')[1].split()[0]
                            processed_ids.add(int(id_str))
                        except Exception:
                            continue
        return processed_ids

    # processed_ids = load_all_processed_ids(latest_log)

    # last_id = load_last_processed_ids(latest_log, n=20)  # n tuỳ ý, lấy 20 dòng cuối để chắc chắn
    last_id = 92000
    log_progress(f"Bắt đầu xử lý. Đã có id cuối cùng: {last_id}", LOG_FILE)

    with open(INPUT_FILE, newline='', encoding="utf-8") as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            if last_id is not None and int(row["id"]) <= last_id:
                continue
            addr_id = row["id"]
            if int(addr_id) > 100000:
                log_progress(f"Đã đạt đến id=100000, dừng xử lý", LOG_FILE)
                break
            lat = row["latitude"]
            lng = row["longitude"]

            address_info = get_address(lat, lng)
            if address_info:
                sql = f"""UPDATE delivery_tracking 
SET location = '{(address_info["full"] or "").replace("'", "''")}'
WHERE id = {addr_id};"""
                updates.append(sql)
                log_progress(f"✔ id={addr_id} → {address_info['full']}", LOG_FILE)
            else:
                log_progress(f"⚠ Không tìm thấy địa chỉ cho id={addr_id}", LOG_FILE)

            time.sleep(1)

            if len(updates) == 100:
                output_file = os.path.join(OUTPUT_DIR, f"update_addresses_{file_count}.sql")
                with open(output_file, "w", encoding="utf-8") as f:
                    f.write("\n".join(updates))
                log_progress(f"\n✅ Đã tạo {output_file} ({len(updates)} lệnh UPDATE)", LOG_FILE)
                file_count += 1
                updates = []

    if updates:
        output_file = os.path.join(OUTPUT_DIR, f"update_addresses_{file_count}.sql")
        with open(output_file, "w", encoding="utf-8") as f:
            f.write("\n".join(updates))
        log_progress(f"\n✅ Đã tạo {output_file} ({len(updates)} lệnh UPDATE)", LOG_FILE)

    log_progress("Hoàn thành xử lý", LOG_FILE)

if __name__ == "__main__":
    main()
