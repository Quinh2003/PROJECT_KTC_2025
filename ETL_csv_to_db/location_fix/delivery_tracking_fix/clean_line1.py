# with open("process_log.txt", "r+", encoding="utf-8") as f:
#     lines = f.readlines()
#     last_index = max(i for i, line in enumerate(lines) if line.startswith("✅ Đã tạo sql_output3/"))
#     # chỉ giữ từ đầu tới dòng cuối cùng "✅ ..."
#     f.seek(0)
#     f.writelines(lines[:last_index + 1])
#     f.truncate()
import os

LOG_DIR = "process_log"
# Tìm file log process_log_*.txt có hậu tố lớn nhất
logs = [f for f in os.listdir(LOG_DIR) if f.startswith("process_log_") and f.endswith(".txt")]
max_num = -1
log_file = None
for fname in logs:
    try:
        num = int(fname.replace("process_log_", "").replace(".txt", ""))
        if num > max_num:
            max_num = num
            log_file = fname
    except Exception:
        continue

if log_file is not None:
    log_path = os.path.join(LOG_DIR, log_file)
    with open(log_path, "r+", encoding="utf-8") as f:
        lines = f.readlines()
        last_index = max(i for i, line in enumerate(lines) if line.startswith("✅ Đã tạo sql_output3/"))
        # chỉ giữ từ đầu tới dòng cuối cùng "✅ ..."
        f.seek(0)
        f.writelines(lines[:last_index + 1])
        f.truncate()
