import { jsx as _jsx, jsxs as _jsxs, Fragment as _Fragment } from "react/jsx-runtime";
import React, { useState, useEffect } from "react";
import { Plus, X, AlertCircle, CheckCircle, Truck, Save } from "lucide-react";
import { addVehicle } from "../../services/VehicleListAPI";
const VEHICLE_TYPES = [
    { value: "TRUCK", label: "Xe tải" },
    { value: "VAN", label: "Xe van" },
    { value: "MOTORCYCLE", label: "Xe máy" },
    { value: "CAR", label: "Xe con" },
];
export default function AddVehicleForm({ onSuccess, onCancel, initialValues, mode = "add", editingVehicle, onUpdate }) {
    const isEditMode = mode === "edit" && editingVehicle;
    const [form, setForm] = useState({
        licensePlate: initialValues?.licensePlate || (isEditMode ? editingVehicle.licensePlate : ""),
        type: initialValues?.type || (isEditMode ? editingVehicle.type : ""),
        capacityWeightKg: initialValues?.capacityWeightKg || (isEditMode ? editingVehicle.capacityWeightKg?.toString() || "" : ""),
        capacityVolumeM3: initialValues?.capacityVolumeM3 || (isEditMode ? editingVehicle.capacityVolumeM3?.toString() || "" : ""),
        notes: initialValues?.notes || "",
    });
    const [errors, setErrors] = useState({});
    const [touched, setTouched] = useState({
        licensePlate: false,
        type: false,
        capacityWeightKg: false,
        capacityVolumeM3: false,
        notes: false,
    });
    const [isLoading, setIsLoading] = useState(false);
    const [submitStatus, setSubmitStatus] = useState("idle");
    // Reset form when editing vehicle changes
    useEffect(() => {
        if (isEditMode) {
            setForm({
                licensePlate: editingVehicle.licensePlate || "",
                type: editingVehicle.type || "",
                capacityWeightKg: editingVehicle.capacityWeightKg?.toString() || "",
                capacityVolumeM3: editingVehicle.capacityVolumeM3?.toString() || "",
                notes: "",
            });
            setErrors({});
            setTouched({
                licensePlate: false,
                type: false,
                capacityWeightKg: false,
                capacityVolumeM3: false,
                notes: false,
            });
        }
    }, [editingVehicle, isEditMode]);
    // -------- Validation ----------
    const validateLicensePlate = (plate) => {
        if (!plate.trim())
            return "Biển số xe là bắt buộc";
        const cleanPlate = plate.trim().toUpperCase();
        if (cleanPlate.length < 6)
            return "Biển số xe phải có ít nhất 6 ký tự";
        if (cleanPlate.length > 15)
            return "Biển số xe không được vượt quá 15 ký tự";
        const hasNumber = /\d/.test(cleanPlate);
        const hasLetter = /[A-Z]/.test(cleanPlate);
        if (!hasNumber || !hasLetter) {
            return "Biển số xe phải có cả số và chữ (VD: 30A-12345)";
        }
        return undefined;
    };
    const validateRequired = (value, fieldName) => {
        if (!value.trim())
            return `${fieldName} là bắt buộc`;
        return undefined;
    };
    const validateNumber = (value, fieldName) => {
        if (!value.trim())
            return undefined;
        const num = parseFloat(value);
        if (isNaN(num))
            return `${fieldName} phải là số`;
        if (num < 0)
            return `${fieldName} không được âm`;
        if (num > 999999)
            return `${fieldName} quá lớn`;
        return undefined;
    };
    const validateNotes = (notes) => {
        if (notes.length > 500)
            return "Ghi chú không được vượt quá 500 ký tự";
        return undefined;
    };
    const handleChange = (field, value) => {
        setForm((prev) => ({ ...prev, [field]: value }));
        if (errors[field]) {
            setErrors((prev) => ({ ...prev, [field]: undefined }));
        }
        if (submitStatus === "success") {
            setSubmitStatus("idle");
        }
    };
    const handleFieldBlur = (field) => {
        setTouched((prev) => ({ ...prev, [field]: true }));
        const currentValue = form[field];
        let error;
        switch (field) {
            case "licensePlate":
                error = validateLicensePlate(currentValue);
                break;
            case "type":
                error = validateRequired(currentValue, "Loại xe");
                break;
            case "capacityWeightKg":
                error = validateNumber(currentValue, "Tải trọng");
                break;
            case "capacityVolumeM3":
                error = validateNumber(currentValue, "Thể tích");
                break;
            case "notes":
                error = validateNotes(currentValue);
                break;
        }
        setErrors((prev) => ({ ...prev, [field]: error }));
    };
    const validateForm = () => {
        const newErrors = {
            licensePlate: validateLicensePlate(form.licensePlate),
            type: validateRequired(form.type, "Loại xe"),
            capacityWeightKg: validateNumber(form.capacityWeightKg, "Tải trọng"),
            capacityVolumeM3: validateNumber(form.capacityVolumeM3, "Thể tích"),
            notes: validateNotes(form.notes),
        };
        setErrors(newErrors);
        setTouched({
            licensePlate: true,
            type: true,
            capacityWeightKg: true,
            capacityVolumeM3: true,
            notes: true,
        });
        return !Object.values(newErrors).some((error) => error !== undefined);
    };
    const handleSubmit = async (e) => {
        e.preventDefault();
        if (isLoading)
            return;
        const isValid = validateForm();
        if (!isValid)
            return;
        setIsLoading(true);
        setSubmitStatus("idle");
        try {
            if (isEditMode && onUpdate && editingVehicle) {
                // Edit mode
                const updatedData = {
                    licensePlate: form.licensePlate.trim().toUpperCase().replace(/\s+/g, ""),
                    type: form.type,
                    capacityWeightKg: form.capacityWeightKg ? parseFloat(form.capacityWeightKg) : undefined,
                    capacityVolumeM3: form.capacityVolumeM3 ? parseFloat(form.capacityVolumeM3) : undefined,
                };
                await onUpdate(editingVehicle.id, updatedData);
                setSubmitStatus("success");
            }
            else {
                // Add mode
                const vehicleData = {
                    licensePlate: form.licensePlate.trim().toUpperCase().replace(/\s+/g, ""),
                    vehicleType: form.type,
                    capacity: form.capacityWeightKg ? parseFloat(form.capacityWeightKg) : 0,
                    capacityVolumeM3: form.capacityVolumeM3 ? parseFloat(form.capacityVolumeM3) : undefined,
                    notes: form.notes.trim() || undefined,
                    statusId: 1,
                };
                await addVehicle(vehicleData);
                setSubmitStatus("success");
                handleReset();
                // Call onSuccess with proper data format
                const successData = {
                    licensePlate: form.licensePlate.trim().toUpperCase().replace(/\s+/g, ""),
                    type: form.type,
                    capacityWeightKg: form.capacityWeightKg ? parseFloat(form.capacityWeightKg) : undefined,
                    capacityVolumeM3: form.capacityVolumeM3 ? parseFloat(form.capacityVolumeM3) : undefined,
                };
                onSuccess?.(successData);
            }
        }
        catch (error) {
            console.error(`Error ${isEditMode ? 'updating' : 'adding'} vehicle:`, error);
            setSubmitStatus("error");
            setErrors((prev) => ({
                ...prev,
                licensePlate: `Có lỗi xảy ra khi ${isEditMode ? 'cập nhật' : 'thêm'} xe. Vui lòng thử lại.`,
            }));
        }
        finally {
            setIsLoading(false);
        }
    };
    const handleReset = () => {
        setForm({
            licensePlate: "",
            type: "",
            capacityWeightKg: "",
            capacityVolumeM3: "",
            notes: "",
        });
        setErrors({});
        setTouched({
            licensePlate: false,
            type: false,
            capacityWeightKg: false,
            capacityVolumeM3: false,
            notes: false,
        });
        setSubmitStatus("idle");
    };
    const isFormValid = !Object.values(errors).some((error) => error !== undefined) &&
        form.licensePlate.trim() !== "" &&
        form.type !== "";
    // ---- UI ----
    return (_jsxs("div", { className: "bg-white rounded-lg shadow-sm border border-gray-200 p-6", children: [_jsxs("div", { className: "flex items-center gap-3 mb-6", children: [_jsx("div", { className: "p-2 bg-blue-50 rounded-lg", children: _jsx(Truck, { className: "w-5 h-5 text-blue-600" }) }), _jsxs("div", { children: [_jsx("h2", { className: "text-xl font-semibold text-gray-900", children: isEditMode ? "Chỉnh sửa phương tiện" : "Thêm phương tiện mới" }), _jsx("p", { className: "text-sm text-gray-600", children: isEditMode
                                    ? `Cập nhật thông tin phương tiện ${editingVehicle?.licensePlate}`
                                    : "Nhập thông tin phương tiện vào hệ thống" })] })] }), submitStatus === "success" && (_jsx("div", { className: "mb-6 p-4 bg-green-50 border border-green-200 rounded-lg", children: _jsxs("div", { className: "flex items-center gap-2 text-green-700", children: [_jsx(CheckCircle, { size: 20 }), _jsx("span", { className: "font-medium", children: isEditMode ? "Cập nhật phương tiện thành công!" : "Thêm phương tiện thành công!" })] }) })), _jsxs("form", { onSubmit: handleSubmit, className: "space-y-6", children: [_jsxs("div", { className: "grid grid-cols-1 md:grid-cols-2 gap-6", children: [_jsxs("div", { className: "space-y-2", children: [_jsxs("label", { className: "block text-sm font-medium text-gray-700", children: ["Bi\u1EC3n s\u1ED1 xe ", _jsx("span", { className: "text-red-500", children: "*" })] }), _jsx("input", { type: "text", placeholder: "51A-12345", value: form.licensePlate, onChange: (e) => handleChange("licensePlate", e.target.value), onBlur: () => handleFieldBlur("licensePlate"), disabled: isLoading, maxLength: 15, className: `w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 ${touched.licensePlate && errors.licensePlate
                                            ? "border-red-500 focus:ring-red-500 focus:border-red-500"
                                            : "border-gray-300"}` }), touched.licensePlate && errors.licensePlate && (_jsxs("div", { className: "flex items-center gap-1 text-sm text-red-600", children: [_jsx(AlertCircle, { size: 14 }), _jsx("span", { children: errors.licensePlate })] }))] }), _jsxs("div", { className: "space-y-2", children: [_jsxs("label", { className: "block text-sm font-medium text-gray-700", children: ["Lo\u1EA1i xe ", _jsx("span", { className: "text-red-500", children: "*" })] }), _jsxs("select", { value: form.type, onChange: (e) => handleChange("type", e.target.value), onBlur: () => handleFieldBlur("type"), disabled: isLoading, className: `w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 bg-white ${touched.type && errors.type
                                            ? "border-red-500 focus:ring-red-500 focus:border-red-500"
                                            : "border-gray-300"}`, children: [_jsx("option", { value: "", children: "-- Ch\u1ECDn lo\u1EA1i xe --" }), VEHICLE_TYPES.map((type) => (_jsx("option", { value: type.value, children: type.label }, type.value)))] }), touched.type && errors.type && (_jsxs("div", { className: "flex items-center gap-1 text-sm text-red-600", children: [_jsx(AlertCircle, { size: 14 }), _jsx("span", { children: errors.type })] }))] }), _jsxs("div", { className: "space-y-2", children: [_jsx("label", { className: "block text-sm font-medium text-gray-700", children: "T\u1EA3i tr\u1ECDng (kg)" }), _jsx("input", { type: "number", step: "0.01", min: "0", placeholder: "1500", value: form.capacityWeightKg, onChange: (e) => handleChange("capacityWeightKg", e.target.value), onBlur: () => handleFieldBlur("capacityWeightKg"), disabled: isLoading, className: `w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 ${touched.capacityWeightKg && errors.capacityWeightKg
                                            ? "border-red-500 focus:ring-red-500 focus:border-red-500"
                                            : "border-gray-300"}` }), touched.capacityWeightKg && errors.capacityWeightKg && (_jsxs("div", { className: "flex items-center gap-1 text-sm text-red-600", children: [_jsx(AlertCircle, { size: 14 }), _jsx("span", { children: errors.capacityWeightKg })] }))] }), _jsxs("div", { className: "space-y-2", children: [_jsx("label", { className: "block text-sm font-medium text-gray-700", children: "Th\u1EC3 t\u00EDch (m\u00B3)" }), _jsx("input", { type: "number", step: "0.01", min: "0", placeholder: "15.5", value: form.capacityVolumeM3, onChange: (e) => handleChange("capacityVolumeM3", e.target.value), onBlur: () => handleFieldBlur("capacityVolumeM3"), disabled: isLoading, className: `w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 ${touched.capacityVolumeM3 && errors.capacityVolumeM3
                                            ? "border-red-500 focus:ring-red-500 focus:border-red-500"
                                            : "border-gray-300"}` }), touched.capacityVolumeM3 && errors.capacityVolumeM3 && (_jsxs("div", { className: "flex items-center gap-1 text-sm text-red-600", children: [_jsx(AlertCircle, { size: 14 }), _jsx("span", { children: errors.capacityVolumeM3 })] }))] })] }), _jsxs("div", { className: "space-y-2", children: [_jsx("label", { className: "block text-sm font-medium text-gray-700", children: "Ghi ch\u00FA" }), _jsx("textarea", { rows: 3, placeholder: "Nh\u1EADp th\u00F4ng tin b\u1ED5 sung v\u1EC1 xe...", value: form.notes, onChange: (e) => handleChange("notes", e.target.value), onBlur: () => handleFieldBlur("notes"), disabled: isLoading, maxLength: 500, className: `w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 resize-none ${touched.notes && errors.notes
                                    ? "border-red-500 focus:ring-red-500 focus:border-red-500"
                                    : "border-gray-300"}` }), _jsxs("div", { className: "text-right text-xs text-gray-500 mt-1", children: [form.notes.length, "/500 k\u00FD t\u1EF1"] }), touched.notes && errors.notes && (_jsxs("div", { className: "flex items-center gap-1 text-sm text-red-600", children: [_jsx(AlertCircle, { size: 14 }), _jsx("span", { children: errors.notes })] }))] }), _jsxs("div", { className: "flex flex-col sm:flex-row gap-3 pt-6 border-t border-gray-200", children: [_jsx("button", { type: "submit", disabled: !isFormValid || isLoading, className: `flex items-center justify-center gap-2 px-6 py-3 rounded-lg font-medium transition-all ${isFormValid && !isLoading
                                    ? "bg-blue-600 hover:bg-blue-700 text-white"
                                    : "bg-gray-300 text-gray-500 cursor-not-allowed"}`, children: isLoading ? (_jsxs(_Fragment, { children: [_jsx("div", { className: "w-4 h-4 border-2 border-white border-t-transparent rounded-full animate-spin" }), _jsx("span", { children: isEditMode ? "Đang cập nhật..." : "Đang thêm..." })] })) : (_jsxs(_Fragment, { children: [isEditMode ? _jsx(Save, { size: 18 }) : _jsx(Plus, { size: 18 }), _jsx("span", { children: isEditMode ? "Cập nhật" : "Thêm phương tiện" })] })) }), _jsxs("button", { type: "button", onClick: handleReset, disabled: isLoading, className: "flex items-center justify-center gap-2 px-6 py-3 rounded-lg font-medium border border-gray-300 text-gray-700 hover:bg-gray-50", children: [_jsx(X, { size: 18 }), _jsx("span", { children: "\u0110\u1EB7t l\u1EA1i" })] }), onCancel && (_jsx("button", { type: "button", onClick: onCancel, disabled: isLoading, className: "flex items-center justify-center gap-2 px-6 py-3 rounded-lg font-medium text-gray-600 hover:text-gray-800", children: "H\u1EE7y b\u1ECF" }))] }), isFormValid && submitStatus === "idle" && (_jsxs("div", { className: "flex items-center gap-2 text-green-600 text-sm", children: [_jsx(CheckCircle, { size: 16 }), _jsxs("span", { children: ["Form h\u1EE3p l\u1EC7 - s\u1EB5n s\u00E0ng ", isEditMode ? "cập nhật" : "thêm", " ph\u01B0\u01A1ng ti\u1EC7n"] })] }))] })] }));
}
