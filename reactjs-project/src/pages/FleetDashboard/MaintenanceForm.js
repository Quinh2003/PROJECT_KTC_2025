import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useState, useEffect } from "react";
import { Calendar } from "lucide-react";
import { fetchVehiclesRaw, updateVehicleStatus } from "../../services/VehicleListAPI";
import { createVehicleMaintenance } from "../../services/VehicleMaintenanceAPI";
const typeOptions = [
    { value: "", label: "Chọn loại" },
    { value: "Bảo dưỡng định kỳ", label: "Bảo dưỡng định kỳ" },
    { value: "Sửa chữa", label: "Sửa chữa" },
    { value: "Sửa chữa khẩn cấp", label: "Sửa chữa khẩn cấp" },
];
import axios from 'axios';
export default function MaintenanceForm({ onAddMaintenance, onMaintenanceCreated, initialVehicle, initialDescription, initialType, initialMaintenanceId }) {
    const isEmergency = Boolean(initialType || initialDescription);
    const [form, setForm] = useState({
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
    const [vehicleOptions, setVehicleOptions] = useState([
        { value: "", label: "Chọn phương tiện" }
    ]);
    const [loadingVehicles, setLoadingVehicles] = useState(false);
    const [vehicleError, setVehicleError] = useState(null);
    useEffect(() => {
        setLoadingVehicles(true);
        fetchVehiclesRaw(1, 200)
            .then(({ data }) => {
            setVehicleOptions([
                { value: "", label: "Chọn phương tiện" },
                ...data.map(v => ({
                    value: v.id?.toString() || '',
                    label: (v.licensePlate ? v.licensePlate : (v.name || `Xe ${v.id}`)) + ` (ID: ${v.id})`
                }))
            ]);
            setLoadingVehicles(false);
        })
            .catch(() => {
            setVehicleError("Không thể tải danh sách xe");
            setLoadingVehicles(false);
        });
    }, []);
    const handleChange = (e) => {
        setForm({ ...form, [e.target.name]: e.target.value });
    };
    const handleSubmit = async (e) => {
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
            const toDateTime = (date) => date ? `${date}T00:00:00` : '';
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
            }
            else {
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
            alert("Đã lên lịch bảo trì thành công!");
        }
        catch (err) {
            alert("Lỗi khi lưu lịch bảo trì!");
        }
    };
    return (_jsxs("div", { className: "bg-white/30 backdrop-blur-lg rounded-2xl p-6 border border-white/30 shadow-xl", children: [_jsx("div", { className: "text-xl font-bold mb-2", children: "L\u00EAn l\u1ECBch b\u1EA3o tr\u00EC" }), _jsxs("form", { className: "grid grid-cols-1 md:grid-cols-2 gap-4", onSubmit: handleSubmit, children: [_jsxs("div", { children: [_jsxs("label", { className: "block text-sm font-medium mb-1", children: ["Ch\u1ECDn ph\u01B0\u01A1ng ti\u1EC7n ", _jsx("span", { className: 'text-red-500', children: "*" })] }), _jsx("select", { name: "vehicle", value: form.vehicle, onChange: handleChange, className: "w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400", required: true, disabled: loadingVehicles || isEmergency, children: vehicleOptions.map(opt => (_jsx("option", { value: opt.value, children: opt.label }, opt.value))) }), loadingVehicles && _jsx("div", { className: "text-xs text-gray-500 mt-1", children: "\u0110ang t\u1EA3i danh s\u00E1ch xe..." }), vehicleError && _jsx("div", { className: "text-xs text-red-500 mt-1", children: vehicleError })] }), _jsxs("div", { children: [_jsxs("label", { className: "block text-sm font-medium mb-1", children: ["Lo\u1EA1i b\u1EA3o tr\u00EC ", _jsx("span", { className: 'text-red-500', children: "*" })] }), _jsx("select", { name: "type", value: form.type, onChange: handleChange, className: "w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400", required: true, disabled: isEmergency, children: typeOptions.map(opt => (_jsx("option", { value: opt.value, children: opt.label }, opt.value))) })] }), _jsxs("div", { className: "md:col-span-2", children: [_jsx("label", { className: "block text-sm font-medium mb-1", children: "M\u00F4 t\u1EA3 c\u00F4ng vi\u1EC7c" }), _jsx("input", { name: "description", value: form.description, onChange: handleChange, className: "w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400", placeholder: "M\u00F4 t\u1EA3 chi ti\u1EBFt c\u00F4ng vi\u1EC7c c\u1EA7n th\u1EF1c hi\u1EC7n", disabled: isEmergency })] }), _jsxs("div", { className: "md:col-span-2", children: [_jsx("label", { className: "block text-sm font-medium mb-1", children: "Ghi ch\u00FA" }), _jsx("textarea", { name: "notes", value: form.notes, onChange: handleChange, className: "w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400", placeholder: "Nh\u1EADp ghi ch\u00FA (n\u1EBFu c\u00F3)", rows: 2 })] }), _jsxs("div", { children: [_jsxs("label", { className: "block text-sm font-medium mb-1", children: ["Ng\u00E0y th\u1EF1c hi\u1EC7n ", _jsx("span", { className: 'text-red-500', children: "*" })] }), _jsx("input", { name: "date", type: "date", value: form.date, onChange: handleChange, className: "w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400", required: true })] }), _jsxs("div", { children: [_jsx("label", { className: "block text-sm font-medium mb-1", children: "Chi ph\u00ED d\u1EF1 ki\u1EBFn (VN\u0110)" }), _jsx("input", { name: "cost", type: "number", min: "0", value: form.cost, onChange: handleChange, className: "w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400" })] }), _jsxs("div", { className: "md:col-span-2", children: [_jsx("label", { className: "block text-sm font-medium mb-1", children: "B\u1EA3o tr\u00EC s\u1EAFp t\u1EDBi" }), _jsx("input", { name: "nextMaintenance", type: "date", value: form.nextMaintenance, onChange: handleChange, className: "w-full border rounded-lg px-3 py-2 focus:outline-none focus:ring-2 focus:ring-violet-400", placeholder: "Ng\u00E0y b\u1EA3o tr\u00EC ti\u1EBFp theo" })] }), _jsx("div", { className: "md:col-span-2 mt-4", children: _jsxs("button", { type: "submit", className: "w-full flex items-center justify-center gap-2 bg-violet-600 hover:bg-violet-700 text-white font-semibold py-3 rounded-lg text-base transition", children: [_jsx(Calendar, { size: 20 }), " L\u00EAn l\u1ECBch b\u1EA3o tr\u00EC"] }) })] })] }));
}
