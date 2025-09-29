lines = []
with open("process_log.txt", "r", encoding="utf-8") as f:
    lines = f.readlines()

# tìm vị trí dòng cuối cùng có "✅ Đã tạo sql_output3/"
last_index = max(i for i, line in enumerate(lines) if line.startswith("✅ Đã tạo sql_output3/"))

# giữ từ đầu đến dòng đó
cleaned = lines[:last_index + 1]

with open("cleaned_log.txt", "w", encoding="utf-8") as f:
    f.writelines(cleaned)
