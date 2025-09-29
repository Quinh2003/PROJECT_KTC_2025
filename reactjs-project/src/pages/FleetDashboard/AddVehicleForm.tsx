import React, { useState, useEffect } from "react";
import { useTranslation } from 'react-i18next';
import { Plus, X, AlertCircle, CheckCircle, Truck, Save } from "lucide-react";
import { addVehicle } from "../../services/VehicleListAPI";
import type { Vehicle } from "../../types/Operations";

interface VehicleFormData {
  licensePlate: string;
  type: string;
  capacityWeightKg: string;
  capacityVolumeM3: string;
  notes: string;
}

interface FormErrors {
  licensePlate?: string;
  type?: string;
  capacityWeightKg?: string;
  capacityVolumeM3?: string;
  notes?: string;
}

type AddVehicleFormProps = {
  onSuccess?: (data: Pick<Vehicle, "licensePlate" | "type" | "capacityWeightKg" | "capacityVolumeM3">) => void | Promise<void>;
  onCancel?: () => void;
  initialValues?: Partial<VehicleFormData>;
  mode?: "add" | "edit";
  editingVehicle?: Vehicle;
  onUpdate?: (vehicleId: number, data: Partial<Vehicle>) => Promise<void>;
};

// Vehicle types will be translated in component

export default function AddVehicleForm({ 
  onSuccess, 
  onCancel, 
  initialValues,
  mode = "add",
  editingVehicle,
  onUpdate
}: AddVehicleFormProps) {
  const { t } = useTranslation();
  const isEditMode = mode === "edit" && editingVehicle;

  const VEHICLE_TYPES = [
    { value: "TRUCK", label: t('fleet.vehicleTypes.truck', 'Xe tải') },
    { value: "VAN", label: t('fleet.vehicleTypes.van', 'Xe van') },
    { value: "MOTORCYCLE", label: t('fleet.vehicleTypes.motorcycle', 'Xe máy') },
    { value: "CAR", label: t('fleet.vehicleTypes.car', 'Xe con') },
  ] as const;

  const [form, setForm] = useState<VehicleFormData>({
    licensePlate: initialValues?.licensePlate || (isEditMode ? editingVehicle.licensePlate : ""),
    type: initialValues?.type || (isEditMode ? (editingVehicle.type || "") : ""),
    capacityWeightKg: initialValues?.capacityWeightKg || (isEditMode ? editingVehicle.capacityWeightKg?.toString() || "" : ""),
    capacityVolumeM3: initialValues?.capacityVolumeM3 || (isEditMode ? editingVehicle.capacityVolumeM3?.toString() || "" : ""),
    notes: initialValues?.notes || "",
  });

  const [errors, setErrors] = useState<FormErrors>({});
  const [touched, setTouched] = useState<Record<keyof VehicleFormData, boolean>>({
    licensePlate: false,
    type: false,
    capacityWeightKg: false,
    capacityVolumeM3: false,
    notes: false,
  });

  const [isLoading, setIsLoading] = useState(false);
  const [submitStatus, setSubmitStatus] = useState<"idle" | "success" | "error">("idle");

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
  const validateLicensePlate = (plate: string): string | undefined => {
    if (!plate.trim()) return t('validation.required');
    const cleanPlate = plate.trim().toUpperCase();
    if (cleanPlate.length < 6) return t('fleet.validation.licensePlateMinLength', 'License plate must have at least 6 characters');
    if (cleanPlate.length > 15) return t('fleet.validation.licensePlateMaxLength', 'License plate cannot exceed 15 characters');

    const hasNumber = /\d/.test(cleanPlate);
    const hasLetter = /[A-Z]/.test(cleanPlate);
    if (!hasNumber || !hasLetter) {
      return t('fleet.validation.licensePlateFormat', 'License plate must contain both numbers and letters (e.g., 30A-12345)');
    }
    return undefined;
  };

  const validateRequired = (value: string, fieldName: string): string | undefined => {
    if (!value.trim()) return t('validation.required');
    return undefined;
  };

  const validateNumber = (value: string, fieldName: string): string | undefined => {
    if (!value.trim()) return undefined;
    const num = parseFloat(value);
    if (isNaN(num)) return t('fleet.validation.mustBeNumber', '{{field}} must be a valid number', { field: fieldName });
    if (num < 0) return t('fleet.validation.mustBePositive', '{{field}} must be positive', { field: fieldName });
    if (num > 999999) return t('fleet.validation.tooLarge', '{{field}} is too large', { field: fieldName });
    return undefined;
  };

  const validateNotes = (notes: string): string | undefined => {
    if (notes.length > 500) return t('fleet.validation.notesMaxLength', 'Notes cannot exceed 500 characters');
    return undefined;
  };

  const handleChange = (field: keyof VehicleFormData, value: string) => {
    setForm((prev) => ({ ...prev, [field]: value }));
    if (errors[field]) {
      setErrors((prev) => ({ ...prev, [field]: undefined }));
    }
    if (submitStatus === "success") {
      setSubmitStatus("idle");
    }
  };

  const handleFieldBlur = (field: keyof VehicleFormData) => {
    setTouched((prev) => ({ ...prev, [field]: true }));

    const currentValue = form[field];
    let error: string | undefined;
    switch (field) {
      case "licensePlate":
        error = validateLicensePlate(currentValue);
        break;
      case "type":
        error = validateRequired(currentValue, t('dashboard.fleet.vehicleType'));
        break;
      case "capacityWeightKg":
        error = validateNumber(currentValue, t('dashboard.fleet.capacity'));
        break;
      case "capacityVolumeM3":
        error = validateNumber(currentValue, t('dashboard.fleet.volume'));
        break;
      case "notes":
        error = validateNotes(currentValue);
        break;
    }
    setErrors((prev) => ({ ...prev, [field]: error }));
  };

  const validateForm = (): boolean => {
    const newErrors: FormErrors = {
      licensePlate: validateLicensePlate(form.licensePlate),
      type: validateRequired(form.type, t('dashboard.fleet.vehicleType')),
      capacityWeightKg: validateNumber(form.capacityWeightKg, t('dashboard.fleet.capacity')),
      capacityVolumeM3: validateNumber(form.capacityVolumeM3, t('dashboard.fleet.volume')),
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

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (isLoading) return;
    const isValid = validateForm();
    if (!isValid) return;

    setIsLoading(true);
    setSubmitStatus("idle");
    try {
      if (isEditMode && onUpdate && editingVehicle) {
        // Edit mode
        const updatedData: Partial<Vehicle> = {
          licensePlate: form.licensePlate.trim().toUpperCase().replace(/\s+/g, ""),
          type: form.type as 'TRUCK' | 'VAN' | 'MOTORCYCLE' | 'CAR' | undefined,
          capacityWeightKg: form.capacityWeightKg ? parseFloat(form.capacityWeightKg) : undefined,
          capacityVolumeM3: form.capacityVolumeM3 ? parseFloat(form.capacityVolumeM3) : undefined,
        };
        await onUpdate(Number(editingVehicle.id), updatedData);
        setSubmitStatus("success");
      } else {
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
          type: form.type as 'TRUCK' | 'VAN' | 'MOTORCYCLE' | 'CAR' | undefined,
          capacityWeightKg: form.capacityWeightKg ? parseFloat(form.capacityWeightKg) : undefined,
          capacityVolumeM3: form.capacityVolumeM3 ? parseFloat(form.capacityVolumeM3) : undefined,
        };
        onSuccess?.(successData);
      }
    } catch (error) {
      console.error(`Error ${isEditMode ? 'updating' : 'adding'} vehicle:`, error);
      setSubmitStatus("error");
      setErrors((prev) => ({
        ...prev,
        licensePlate: t('fleet.errors.submitError', 'Error occurred while {{action}} vehicle. Please try again.', {
          action: isEditMode ? t('common.update', 'updating') : t('common.add', 'adding')
        }),
      }));
    } finally {
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

  const isFormValid =
    !Object.values(errors).some((error) => error !== undefined) &&
    form.licensePlate.trim() !== "" &&
    form.type !== "";

  // ---- UI ----
  return (
    <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6">
      <div className="flex items-center gap-3 mb-6">
        <div className="p-2 bg-blue-50 rounded-lg">
          <Truck className="w-5 h-5 text-blue-600" />
        </div>
        <div>
          <h2 className="text-xl font-semibold text-gray-900">
{isEditMode ? t('fleet.form.editVehicle', 'Edit Vehicle') : t('fleet.form.addVehicle', 'Add New Vehicle')}
          </h2>
          <p className="text-sm text-gray-600">
            {isEditMode 
              ? t('fleet.form.updateVehicleSubtitle', 'Update vehicle information for {{plate}}', { plate: editingVehicle?.licensePlate })
              : t('fleet.form.addVehicleSubtitle', 'Enter vehicle information into the system')
            }
          </p>
        </div>
      </div>

      {submitStatus === "success" && (
        <div className="mb-6 p-4 bg-green-50 border border-green-200 rounded-lg">
          <div className="flex items-center gap-2 text-green-700">
            <CheckCircle size={20} />
            <span className="font-medium">
{isEditMode ? t('fleet.form.updateSuccess', 'Vehicle updated successfully!') : t('fleet.form.addSuccess', 'Vehicle added successfully!')}
            </span>
          </div>
        </div>
      )}

      <form onSubmit={handleSubmit} className="space-y-6">
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          {/* License Plate */}
          <div className="space-y-2">
            <label className="block text-sm font-medium text-gray-700">
              {t('fleet.form.licensePlate', 'License Plate')} <span className="text-red-500">*</span>
            </label>
            <input
              type="text"
              placeholder="51A-12345"
              value={form.licensePlate}
              onChange={(e) => handleChange("licensePlate", e.target.value)}
              onBlur={() => handleFieldBlur("licensePlate")}
              disabled={isLoading}
              maxLength={15}
              className={`w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 ${
                touched.licensePlate && errors.licensePlate
                  ? "border-red-500 focus:ring-red-500 focus:border-red-500"
                  : "border-gray-300"
              }`}
            />
            {touched.licensePlate && errors.licensePlate && (
              <div className="flex items-center gap-1 text-sm text-red-600">
                <AlertCircle size={14} />
                <span>{errors.licensePlate}</span>
              </div>
            )}
          </div>

          {/* Vehicle Type */}
          <div className="space-y-2">
            <label className="block text-sm font-medium text-gray-700">
              {t('fleet.vehicleType', 'Loại xe')} <span className="text-red-500">*</span>
            </label>
            <select
              value={form.type}
              onChange={(e) => handleChange("type", e.target.value)}
              onBlur={() => handleFieldBlur("type")}
              disabled={isLoading}
              className={`w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 bg-white ${
                touched.type && errors.type
                  ? "border-red-500 focus:ring-red-500 focus:border-red-500"
                  : "border-gray-300"
              }`}
            >
              <option value="">{t('fleet.selectType', '-- Chọn loại xe --')}</option>
              {VEHICLE_TYPES.map((type) => (
                <option key={type.value} value={type.value}>
                  {type.label}
                </option>
              ))}
            </select>
            {touched.type && errors.type && (
              <div className="flex items-center gap-1 text-sm text-red-600">
                <AlertCircle size={14} />
                <span>{errors.type}</span>
              </div>
            )}
          </div>

          {/* Capacity Weight */}
          <div className="space-y-2">
            <label className="block text-sm font-medium text-gray-700">{t('fleet.capacity', 'Trọng tải')} (kg)</label>
            <input
              type="number"
              step="0.01"
              min="0"
              placeholder="1500"
              value={form.capacityWeightKg}
              onChange={(e) => handleChange("capacityWeightKg", e.target.value)}
              onBlur={() => handleFieldBlur("capacityWeightKg")}
              disabled={isLoading}
              className={`w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 ${
                touched.capacityWeightKg && errors.capacityWeightKg
                  ? "border-red-500 focus:ring-red-500 focus:border-red-500"
                  : "border-gray-300"
              }`}
            />
            {touched.capacityWeightKg && errors.capacityWeightKg && (
              <div className="flex items-center gap-1 text-sm text-red-600">
                <AlertCircle size={14} />
                <span>{errors.capacityWeightKg}</span>
              </div>
            )}
          </div>

          {/* Capacity Volume */}
          <div className="space-y-2">
            <label className="block text-sm font-medium text-gray-700">{t('fleet.volume', 'Thể tích')} (m³)</label>
            <input
              type="number"
              step="0.01"
              min="0"
              placeholder="15.5"
              value={form.capacityVolumeM3}
              onChange={(e) => handleChange("capacityVolumeM3", e.target.value)}
              onBlur={() => handleFieldBlur("capacityVolumeM3")}
              disabled={isLoading}
              className={`w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 ${
                touched.capacityVolumeM3 && errors.capacityVolumeM3
                  ? "border-red-500 focus:ring-red-500 focus:border-red-500"
                  : "border-gray-300"
              }`}
            />
            {touched.capacityVolumeM3 && errors.capacityVolumeM3 && (
              <div className="flex items-center gap-1 text-sm text-red-600">
                <AlertCircle size={14} />
                <span>{errors.capacityVolumeM3}</span>
              </div>
            )}
          </div>
        </div>

        {/* Notes */}
        <div className="space-y-2">
          <label className="block text-sm font-medium text-gray-700">{t('fleet.notes', 'Ghi chú')}</label>
          <textarea
            rows={3}
placeholder={t('fleet.form.notesPlaceholder', 'Enter additional vehicle information...')}
            value={form.notes}
            onChange={(e) => handleChange("notes", e.target.value)}
            onBlur={() => handleFieldBlur("notes")}
            disabled={isLoading}
            maxLength={500}
            className={`w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500 resize-none ${
              touched.notes && errors.notes
                ? "border-red-500 focus:ring-red-500 focus:border-red-500"
                : "border-gray-300"
            }`}
          />
          <div className="text-right text-xs text-gray-500 mt-1">
{form.notes.length}/500 {t('common.characters', 'ký tự')}
          </div>
          {touched.notes && errors.notes && (
            <div className="flex items-center gap-1 text-sm text-red-600">
              <AlertCircle size={14} />
              <span>{errors.notes}</span>
            </div>
          )}
        </div>

        {/* Buttons */}
        <div className="flex flex-col sm:flex-row gap-3 pt-6 border-t border-gray-200">
          <button
            type="submit"
            disabled={!isFormValid || isLoading}
            className={`flex items-center justify-center gap-2 px-6 py-3 rounded-lg font-medium transition-all ${
              isFormValid && !isLoading
                ? "bg-blue-600 hover:bg-blue-700 text-white"
                : "bg-gray-300 text-gray-500 cursor-not-allowed"
            }`}
          >
            {isLoading ? (
              <>
                <div className="w-4 h-4 border-2 border-white border-t-transparent rounded-full animate-spin" />
                <span>{isEditMode ? t('fleet.form.updating', 'Updating...') : t('fleet.form.adding', 'Adding...')}</span>
              </>
            ) : (
              <>
                {isEditMode ? <Save size={18} /> : <Plus size={18} />}
                <span>{isEditMode ? t('common.update') : t('fleet.form.addVehicle', 'Add Vehicle')}</span>
              </>
            )}
          </button>

          <button
            type="button"
            onClick={handleReset}
            disabled={isLoading}
            className="flex items-center justify-center gap-2 px-6 py-3 rounded-lg font-medium border border-gray-300 text-gray-700 hover:bg-gray-50"
          >
            <X size={18} />
            <span>{t('common.reset', 'Đặt lại')}</span>
          </button>

          {onCancel && (
            <button
              type="button"
              onClick={onCancel}
              disabled={isLoading}
              className="flex items-center justify-center gap-2 px-6 py-3 rounded-lg font-medium text-gray-600 hover:text-gray-800"
            >
{t('common.cancel')}
            </button>
          )}
        </div>

        {isFormValid && submitStatus === "idle" && (
          <div className="flex items-center gap-2 text-green-600 text-sm">
            <CheckCircle size={16} />
            <span>{t('fleet.form.validForm', 'Form is valid - ready to {{action}} vehicle', { 
              action: isEditMode ? t('common.edit', 'update') : t('common.add', 'add')
            })}</span>
          </div>
        )}
      </form>
    </div>
  );
}
