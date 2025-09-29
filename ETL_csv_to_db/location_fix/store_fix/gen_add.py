import csv
import time
import requests

INPUT_FILE = "stores.csv"
OUTPUT_FILE = "update_stores.sql"

def get_address(lat, lng):
    url = f"https://nominatim.openstreetmap.org/reverse?format=jsonv2&lat={lat}&lon={lng}&addressdetails=1"
    headers = {"User-Agent": "StoreGeocoder/1.0"}  # tránh bị block
    response = requests.get(url, headers=headers)
    data = response.json()
    return data.get("display_name")

def main():
    updates = []
    with open(INPUT_FILE, newline='', encoding="utf-8") as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            store_id = row["id"]
            lat = row["latitude"]
            lng = row["longitude"]

            address = get_address(lat, lng)
            if address:
                sql = f"""UPDATE stores SET address = '{address.replace("'", "''")}' WHERE id = {store_id};"""
                updates.append(sql)
                print(f"✔ id={store_id} → {address}")
            else:
                print(f"⚠ Không tìm thấy địa chỉ cho id={store_id}")

            time.sleep(1)  # tránh bị block (rate limit ~1 request/giây)

    with open(OUTPUT_FILE, "w", encoding="utf-8") as f:
        f.write("\n".join(updates))

    print(f"\n✅ Đã tạo file {OUTPUT_FILE} với {len(updates)} câu lệnh UPDATE")

if __name__ == "__main__":
    main()
