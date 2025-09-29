import asyncio
import aiohttp
import os
import time
import csv
import pandas as pd
from multiprocessing import Pool
from dotenv import load_dotenv

load_dotenv()
INPUT_FILE = os.getenv("INPUT_FILE", "abc.csv")
OUTPUT_DIR = os.getenv("OUTPUT_DIR", "sql_outputtest")
LOG_DIR = os.getenv("LOG_DIR", "process_log1")

os.makedirs(OUTPUT_DIR, exist_ok=True)
os.makedirs(LOG_DIR, exist_ok=True)

address_cache = {}

def get_log_files_info():
    logs = [(int(f.replace('process_log_', '').replace('.txt', '')), f)
            for f in os.listdir(LOG_DIR) 
            if f.startswith('process_log_') and f.endswith('.txt') 
            and f.replace('process_log_', '').replace('.txt', '').isdigit()]
    logs.sort()
    return [(num, os.path.join(LOG_DIR, fname)) for num, fname in logs]

def get_latest_log_file():
    logs = get_log_files_info()
    return (logs[-1][1], logs[-1][0]) if logs else (None, 0)

def get_next_log_file():
    _, max_num = get_latest_log_file()
    return os.path.join(LOG_DIR, f'process_log_{max_num + 1}.txt')

def load_last_processed_ids(log_file, n=20):
    max_id = None
    if log_file and os.path.exists(log_file):
        with open(log_file, 'r', encoding='utf-8') as f:
            lines = [line for line in f if '✔ id=' in line][-n:]
        for line in lines:
            try:
                id_str = line.split('✔ id=')[1].split()[0]
                id_val = int(id_str)
                if max_id is None or id_val > max_id:
                    max_id = id_val
            except Exception:
                continue
    return max_id

def log_progress(message, log_file, log_buffer, flush_interval=100):
    timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
    log_message = f"[{timestamp}] {message}"
    print(log_message)
    log_buffer.append(log_message + "\n")
    if len(log_buffer) >= flush_interval:
        with open(log_file, 'a', encoding='utf-8') as f:
            f.writelines(log_buffer)
        log_buffer.clear()

async def get_address_async(lat, lng, session, semaphore):
    key = (lat, lng)
    if key in address_cache:
        return address_cache[key]
    async with semaphore:
        url = f"https://nominatim.openstreetmap.org/reverse?format=jsonv2&lat={lat}&lon={lng}&addressdetails=1"
        headers = {"User-Agent": "AddressGeocoder/1.0"}
        async with session.get(url, headers=headers) as response:
            data = await response.json()
            await asyncio.sleep(1)
            if "address" not in data:
                return None
            addr = data["address"]
            result = {
                "full": data.get("display_name"),
                "city": addr.get("city") or addr.get("town") or addr.get("village"),
                "state": addr.get("state"),
                "country": addr.get("country"),
                "region": addr.get("county") or addr.get("suburb") or addr.get("region"),
                "postal_code": addr.get("postcode"),
            }
            address_cache[key] = result
            return result

async def process_row(row, session, semaphore, updates, log_file, log_buffer):
    addr_id = int(row["id"])  # Ensure id is integer, no decimal part
    address_info = await get_address_async(row["latitude"], row["longitude"], session, semaphore)
    if address_info:
        sql = f"""UPDATE delivery_tracking 
SET location = '{(address_info["full"] or "").replace("'", "''")}'
WHERE id = {addr_id};"""  # id is integer
        updates.append(sql)
        log_progress(f"✔ id={addr_id} → {address_info['full']}", log_file, log_buffer)
    else:
        log_progress(f"⚠ Không tìm thấy địa chỉ cho id={addr_id}", log_file, log_buffer)

def process_chunk(chunk, last_id, file_count, log_file):
    updates = []
    log_buffer = []
    semaphore = asyncio.Semaphore(1)
    async def process():
        async with aiohttp.ClientSession() as session:
            tasks = []
            for _, row in chunk.iterrows():
                if last_id is not None and int(row["id"]) <= last_id:
                    continue
                tasks.append(process_row(row, session, semaphore, updates, log_file, log_buffer))
                if len(tasks) >= 100:
                    await asyncio.gather(*tasks)
                    tasks = []
            if tasks:
                await asyncio.gather(*tasks)
    asyncio.run(process())
    if log_buffer:
        with open(log_file, 'a', encoding='utf-8') as f:
            f.writelines(log_buffer)
    return updates, file_count

def main():
    df = pd.read_csv(INPUT_FILE)
    chunk_size = 1000
    chunks = [df[i:i + chunk_size] for i in range(0, len(df), chunk_size)]
    latest_log, _ = get_latest_log_file()
    LOG_FILE = get_next_log_file()
    last_id = load_last_processed_ids(latest_log, n=20)
    # last_id = 85800

    log_buffer = []
    log_progress(f"Bắt đầu xử lý. Đã có id cuối cùng: {last_id}", LOG_FILE, log_buffer)

    file_count = max(int(f.replace("update_addresses_", "").replace(".sql", "")) 
                     for f in os.listdir(OUTPUT_DIR) 
                     if f.startswith("update_addresses_") and f.endswith(".sql")) + 1 if os.listdir(OUTPUT_DIR) else 1

    with Pool(processes=4) as pool:
        results = pool.starmap(process_chunk, [(chunk, last_id, file_count + i, LOG_FILE) for i, chunk in enumerate(chunks)])
    
    for updates, fc in results:
        if updates:
            output_file = os.path.join(OUTPUT_DIR, f"update_addresses_{fc}.sql")
            with open(output_file, "w", encoding="utf-8") as f:
                f.writelines(updates)
            log_progress(f"\n✅ Đã tạo {output_file} ({len(updates)} lệnh UPDATE)", LOG_FILE, log_buffer)
    
    if log_buffer:
        with open(LOG_FILE, 'a', encoding='utf-8') as f:
            f.writelines(log_buffer)
    log_progress("Hoàn thành xử lý", LOG_FILE, log_buffer)

if __name__ == "__main__":
    main()