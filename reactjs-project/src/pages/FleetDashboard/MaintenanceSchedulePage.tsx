import { useState } from "react";
import { Calendar } from "lucide-react";

interface ScheduleForm {
  vehicle: string;
  type: string;
  description: string;
  date: string;
  cost: string;
  nextMaintenance: string;
}

const vehicleOptions = [
  { value: "", label: "Chọn phương tiện" },
  { value: "51A-12345", label: "51A-12345" },
  { value: "51B-67890", label: "51B-67890" },
];

const typeOptions = [
  { value: "", label: "Chọn loại" },
  { value: "Bảo dưỡng định kỳ", label: "Bảo dưỡng định kỳ" },
  { value: "Sửa chữa", label: "Sửa chữa" },
];

export default function MaintenanceSchedulePage() {
  const [form, setForm] = useState<ScheduleForm>({
    vehicle: "",
    type: "",
    description: "",
    date: "",
    cost: "0",
    nextMaintenance: "",
  });

  const handleChange = (e: React.ChangeEvent<HTMLInputElement | HTMLSelectElement | HTMLTextAreaElement>) => {
    setForm({ ...form, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    // Xử lý lưu lịch bảo trì ở đây
    alert("Đã lên lịch bảo trì thành công!");
  };

  return (
    <div className="bg-white/30 backdrop-blur-lg rounded-2xl p-6 border border-white/30 shadow-xl">
      <div className="text-xl font-bold mb-2">Lên lịch bảo trì</div>
      <form className="grid grid-cols-1 md:grid-cols-2 gap-4" onSubmit={handleSubmit}>
        <div>
          <label className="block text-sm font-medium mb-1">Chọn phương tiện <span className='text-red-500'>*</span></label>
          <select
            name="vehicle"
            value={form.vehicle}
            onChange={handleChange}
            className="w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400"
            required
          >
            {vehicleOptions.map(opt => (
              <option key={opt.value} value={opt.value}>{opt.label}</option>
            ))}
          </select>
        </div>
        <div>
          <label className="block text-sm font-medium mb-1">Loại bảo trì <span className='text-red-500'>*</span></label>
          <select
            name="type"
            value={form.type}
            onChange={handleChange}
            className="w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400"
            required
          >
            {typeOptions.map(opt => (
              <option key={opt.value} value={opt.value}>{opt.label}</option>
            ))}
          </select>
        </div>
        <div className="md:col-span-2">
          <label className="block text-sm font-medium mb-1">Mô tả công việc</label>
          <input
            name="description"
            value={form.description}
            onChange={handleChange}
            className="w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400"
            placeholder="Mô tả chi tiết công việc cần thực hiện"
          />
        </div>
        <div>
          <label className="block text-sm font-medium mb-1">Ngày thực hiện <span className='text-red-500'>*</span></label>
          <input
            name="date"
            type="date"
            value={form.date}
            onChange={handleChange}
            className="w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400"
            required
          />
        </div>
        <div>
          <label className="block text-sm font-medium mb-1">Chi phí dự kiến (VNĐ)</label>
          <input
            name="cost"
            type="number"
            min="0"
            value={form.cost}
            onChange={handleChange}
            className="w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400"
          />
        </div>
        <div className="md:col-span-2">
          <label className="block text-sm font-medium mb-1">Bảo trì sắp tới</label>
          <input
            name="nextMaintenance"
            type="date"
            value={form.nextMaintenance}
            onChange={handleChange}
            className="w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400"
            placeholder="Ngày bảo trì tiếp theo"
          />
        </div>
        <div className="md:col-span-2 mt-4">
          <button
            type="submit"
            className="w-full flex items-center justify-center gap-2 bg-violet-600 hover:bg-violet-700 text-white font-semibold py-3 rounded-lg text-base transition"
          >
            <Calendar size={20} /> Lên lịch bảo trì
          </button>
        </div>
      </form>
    </div>
  );
}
