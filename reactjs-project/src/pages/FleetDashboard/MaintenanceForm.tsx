import { useState, useEffect } from "react";
import { useTranslation } from 'react-i18next';
import { Calendar } from "lucide-react";
import { fetchVehiclesRaw, updateVehicleStatus } from "../../services/VehicleListAPI";
import { createVehicleMaintenance } from "../../services/VehicleMaintenanceAPI";
import type { Vehicle } from "../../types/Operations";


interface ScheduleForm {
  vehicle: string;
  type: string;
  description: string;
  date: string;
  cost: string;
  nextMaintenance: string;
  statusId: string;
  notes: string;
}



// Type options will be moved to component to use translations

interface MaintenanceFormProps {
  onAddMaintenance?: (data: ScheduleForm) => void;
  onMaintenanceCreated?: () => void; // Add callback to refresh data
  initialVehicle?: Vehicle;
  initialDescription?: string;
  initialType?: string;
  initialMaintenanceId?: number;
}

import axios from 'axios';

export default function MaintenanceForm({ onAddMaintenance, onMaintenanceCreated, initialVehicle, initialDescription, initialType, initialMaintenanceId }: MaintenanceFormProps) {
  const { t } = useTranslation();
  const isEmergency = Boolean(initialType || initialDescription);

  const getTypeOptions = () => [
    { value: "", label: t('dashboard.fleet.selectType', 'Select type') },
    { value: "scheduled", label: t('fleet.maintenanceTypes.scheduled', 'Scheduled Maintenance') },
    { value: "repair", label: t('fleet.maintenanceTypes.repair', 'Repair') },
    { value: "emergency", label: t('fleet.maintenanceTypes.emergency', 'Emergency Repair') },
    { value: "Sửa chữa khẩn cấp", label: t('fleet.maintenanceTypes.urgent', 'Sửa chữa khẩn cấp') },
  ];
  const [form, setForm] = useState<ScheduleForm>({
    vehicle: initialVehicle?.id?.toString() || "",
    type: initialType || "",
    description: initialDescription || "",
    date: "",
    cost: "0",
    nextMaintenance: "",
    statusId: "19", // Mặc định là "Đang bảo trì"
    notes: "",
  });

  // Khi props thay đổi (mở modal mới), tự động fill lại form

  useEffect(() => {
    console.log('DEBUG MaintenanceForm useEffect triggered with:', {
      initialVehicle: initialVehicle?.licensePlate,
      initialType,
      initialDescription,
      isEmergency: Boolean(initialType || initialDescription)
    });
    setForm(f => ({
      ...f,
      vehicle: initialVehicle?.id?.toString() || "",
      type: initialType || "",
      description: initialDescription || "",
    }));
  }, [initialVehicle, initialDescription, initialType]);
// const statusOptions = [
//   { value: "1", label: "Chờ xử lý" },
//   { value: "2", label: "Đang thực hiện" },
// ];
  const [vehicleOptions, setVehicleOptions] = useState<{ value: string; label: string }[]>([
    { value: "", label: t('dashboard.fleet.selectVehicle', 'Select vehicle') }
  ]);
  const [loadingVehicles, setLoadingVehicles] = useState(false);
  const [vehicleError, setVehicleError] = useState<string | null>(null);

  useEffect(() => {
    setLoadingVehicles(true);
    fetchVehiclesRaw(1, 200)
      .then(({ data }) => {
        setVehicleOptions([
          { value: "", label: t('dashboard.fleet.selectVehicle', 'Select vehicle') },
          ...data.map(v => ({
            value: v.id?.toString() || '',
            label: (v.licensePlate ? v.licensePlate : (v.name || `Xe ${v.id}`)) + ` (ID: ${v.id})`
          }))
        ]);
        setLoadingVehicles(false);
      })
      .catch(() => {
        setVehicleError(t('fleet.errors.loadVehicles', 'Cannot load vehicle list'));
        setLoadingVehicles(false);
      });
  }, []);

  const handleChange = (e: React.ChangeEvent<HTMLInputElement | HTMLSelectElement | HTMLTextAreaElement>) => {
    setForm({ ...form, [e.target.name]: e.target.value });
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    try {
      // Debug: Log để kiểm tra initialMaintenanceId
      console.log('DEBUG MaintenanceForm handleSubmit:', {
        initialMaintenanceId,
        hasId: Boolean(initialMaintenanceId),
        formData: form
      });

      // Gửi dữ liệu lên API để lưu vào database
      // Chuyển đổi ngày sang định dạng yyyy-MM-ddTHH:mm:ss
      const toDateTime = (date: string) => date ? `${date}T00:00:00` : '';
      
      if (initialMaintenanceId) {
        console.log('DEBUG: Updating existing maintenance request with ID:', initialMaintenanceId);
        // PUT cập nhật maintenance request
        await axios.put(`/api/maintenance-requests/${initialMaintenanceId}`, {
          statusId: Number(form.statusId),
          scheduledMaintenanceDate: toDateTime(form.date),
          cost: form.cost ? Number(form.cost) : undefined,
          notes: form.notes,
          nextDueDate: toDateTime(form.nextMaintenance), // Sửa từ next_due_date thành nextDueDate
          maintenanceType: form.type,
          description: form.description,
        }, {
          headers: { Authorization: `Bearer ${localStorage.getItem('token')}` }
        });
        console.log('DEBUG: Successfully updated maintenance request');
      } else {
        console.log('DEBUG: Creating new maintenance request');
        await createVehicleMaintenance({
          vehicle: { id: Number(form.vehicle) },
          maintenanceDate: toDateTime(form.date),
          nextDueDate: toDateTime(form.nextMaintenance),
          maintenanceType: form.type,
          description: form.description,
          cost: form.cost ? Number(form.cost) : undefined,
          notes: form.notes,
          status: { id: Number(form.statusId) },
        });
        console.log('DEBUG: Successfully created new maintenance request');
      }

      // Cập nhật trạng thái xe thành MAINTENANCE sau khi tạo lịch bảo trì thành công
      await updateVehicleStatus(form.vehicle, 'MAINTENANCE');

      // Cập nhật local state vehicles nếu hàm setVehicles được truyền qua props (nếu không thì rely vào refresh)
      if (typeof window !== 'undefined' && window.dispatchEvent) {
        // Gửi custom event để các component khác có thể lắng nghe và cập nhật nếu muốn
        window.dispatchEvent(new CustomEvent('vehicleStatusChanged', { detail: { vehicleId: Number(form.vehicle), status: 'MAINTENANCE' } }));
      }

      if (onAddMaintenance) {
        onAddMaintenance(form);
      }

      // Gọi callback để refresh danh sách xe
      if (onMaintenanceCreated) {
        onMaintenanceCreated();
      }

      alert(t('fleet.maintenance.scheduleSuccess', 'Maintenance scheduled successfully!'));
    } catch (err) {
      alert(t('fleet.maintenance.scheduleError', 'Error saving maintenance schedule!'));
    }
  };

  return (
    <div className="bg-white/30 backdrop-blur-lg rounded-2xl p-6 border border-white/30 shadow-xl">
      <div className="text-xl font-bold mb-2">{t('dashboard.fleet.scheduleMaintenance', 'Schedule Maintenance')}</div>
      <form className="grid grid-cols-1 md:grid-cols-2 gap-4" onSubmit={handleSubmit}>
        <div>
          <label className="block text-sm font-medium mb-1">{t('dashboard.fleet.selectVehicle')} <span className='text-red-500'>*</span></label>
          <select
            name="vehicle"
            value={form.vehicle}
            onChange={handleChange}
            className="w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400"
            required
            disabled={loadingVehicles || isEmergency}
          >
            {vehicleOptions.map(opt => (
              <option key={opt.value} value={opt.value}>{opt.label}</option>
            ))}
          </select>
          {loadingVehicles && <div className="text-xs text-gray-500 mt-1">{t('dashboard.fleet.loadingVehicles', 'Loading vehicle list...')}</div>}
          {vehicleError && <div className="text-xs text-red-500 mt-1">{vehicleError}</div>}
        </div>
        <div>
          <label className="block text-sm font-medium mb-1">{t('dashboard.fleet.maintenanceType', 'Maintenance Type')} <span className='text-red-500'>*</span></label>
          <select
            name="type"
            value={form.type}
            onChange={handleChange}
            className="w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400"
            required
            disabled={isEmergency}
          >
            {getTypeOptions().map(opt => (
              <option key={opt.value} value={opt.value}>{opt.label}</option>
            ))}
          </select>
        </div>
  {/* Ẩn trường trạng thái, chỉ gửi mặc định khi submit */}
        <div className="md:col-span-2">
          <label className="block text-sm font-medium mb-1">{t('dashboard.fleet.workDescription', 'Work Description')}</label>
          <input
            name="description"
            value={form.description}
            onChange={handleChange}
            className="w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400"
            placeholder={t('dashboard.fleet.workDescriptionPlaceholder', 'Describe the work to be performed in detail')}
            disabled={isEmergency}
          />
        </div>
        <div className="md:col-span-2">
          <label className="block text-sm font-medium mb-1">{t('dashboard.fleet.notes')}</label>
          <textarea
            name="notes"
            value={form.notes}
            onChange={handleChange}
            className="w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400"
            placeholder={t('dashboard.fleet.notesPlaceholder', 'Enter notes (if any)')}
            rows={2}
          />
        </div>
        <div>
          <label className="block text-sm font-medium mb-1">{t('dashboard.fleet.implementationDate', 'Implementation Date')} <span className='text-red-500'>*</span></label>
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
          <label className="block text-sm font-medium mb-1">{t('dashboard.fleet.estimatedCost', 'Estimated Cost')} (VNĐ)</label>
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
          <label className="block text-sm font-medium mb-1">{t('dashboard.fleet.nextMaintenance', 'Next Maintenance')}</label>
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
            <Calendar size={20} /> {t('dashboard.fleet.scheduleMaintenance')}
          </button>
        </div>
      </form>
    </div>
  );
}
