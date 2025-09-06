import React, { useState, useCallback } from "react";
import { Plus, X, AlertCircle, CheckCircle } from "lucide-react";
import type { FleetVehicle } from "../../types/dashboard";

type AddVehicleFormProps = {
  onAdd: (vehicle: Omit<FleetVehicle, "id" | "status" | "lastMaintenance" | "nextMaintenance" | "driver" | "mileage">) => void;
  isLoading?: boolean;
};

interface FormData {
  licensePlate: string;
  type: string;
  brand: string;
  model: string;
  year: string;
}

interface FormErrors {
  licensePlate?: string;
  type?: string;
  brand?: string;
  model?: string;
  year?: string;
}

const VEHICLE_TYPES = [
  "Xe tải nhỏ",
  "Xe tải trung",
  "Xe tải lớn",
  "Xe đầu kéo",
] as const;

const POPULAR_BRANDS = [
  "Hyundai",
  "Isuzu",
  "Fuso",
  "Hino",
  "Dongfeng",
  "JAC",
  "Thaco",
  "Daewoo",
  "Mercedes-Benz",
  "Volvo",
] as const;

export default function AddVehicleForm({ onAdd, isLoading = false }: AddVehicleFormProps) {
  const [form, setForm] = useState<FormData>({
    licensePlate: "",
    type: "",
    brand: "",
    model: "",
    year: "",
  });

  const [errors, setErrors] = useState<FormErrors>({});
  const [touched, setTouched] = useState<Record<keyof FormData, boolean>>({
    licensePlate: false,
    type: false,
    brand: false,
    model: false,
    year: false,
  });

  // Validation functions
  const validateLicensePlate = (plate: string): string | undefined => {
    if (!plate.trim()) return "Biển số xe là bắt buộc";
    
    // Vietnamese license plate format validation
    const plateRegex = /^[0-9]{2}[A-Z]-[0-9]{4,5}$/;
    if (!plateRegex.test(plate.trim())) {
      return "Biển số không đúng định dạng (VD: 51A-12345)";
    }
    
    return undefined;
  };

  const validateYear = (year: string): string | undefined => {
    if (!year.trim()) return "Năm sản xuất là bắt buộc";
    
    const yearNum = parseInt(year);
    const currentYear = new Date().getFullYear();
    
    if (isNaN(yearNum)) return "Năm sản xuất phải là số";
    if (yearNum < 1980) return "Năm sản xuất không được nhỏ hơn 1980";
    if (yearNum > currentYear + 1) return "Năm sản xuất không được lớn hơn năm hiện tại";
    
    return undefined;
  };

  const validateRequired = (value: string, fieldName: string): string | undefined => {
    if (!value.trim()) return `${fieldName} là bắt buộc`;
    return undefined;
  };

  // Handle field changes with real-time validation
  const handleFieldChange = useCallback((field: keyof FormData, value: string) => {
    setForm(prev => ({ ...prev, [field]: value }));
    
    // Clear error when user starts typing
    if (errors[field]) {
      setErrors(prev => ({ ...prev, [field]: undefined }));
    }
  }, [errors]);

  // Handle field blur for validation
  const handleFieldBlur = useCallback((field: keyof FormData) => {
    setTouched(prev => ({ ...prev, [field]: true }));
    
    let error: string | undefined;
    const value = form[field];
    
    switch (field) {
      case 'licensePlate':
        error = validateLicensePlate(value);
        break;
      case 'type':
        error = validateRequired(value, "Loại xe");
        break;
      case 'brand':
        error = validateRequired(value, "Hãng xe");
        break;
      case 'model':
        error = validateRequired(value, "Model");
        break;
      case 'year':
        error = validateYear(value);
        break;
    }
    
    setErrors(prev => ({ ...prev, [field]: error }));
  }, [form]);

  // Validate entire form
  const validateForm = (): boolean => {
    const newErrors: FormErrors = {
      licensePlate: validateLicensePlate(form.licensePlate),
      type: validateRequired(form.type, "Loại xe"),
      brand: validateRequired(form.brand, "Hãng xe"),
      model: validateRequired(form.model, "Model"),
      year: validateYear(form.year),
    };

    setErrors(newErrors);
    setTouched({
      licensePlate: true,
      type: true,
      brand: true,
      model: true,
      year: true,
    });

    return !Object.values(newErrors).some(error => error !== undefined);
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm() || isLoading) return;

    onAdd({
      licensePlate: form.licensePlate.trim().toUpperCase(),
      type: form.type,
      brand: form.brand.trim(),
      model: form.model.trim(),
      year: Number(form.year),
    });

    // Reset form
    setForm({
      licensePlate: "",
      type: "",
      brand: "",
      model: "",
      year: "",
    });
    setErrors({});
    setTouched({
      licensePlate: false,
      type: false,
      brand: false,
      model: false,
      year: false,
    });
  };

  const handleReset = () => {
    setForm({
      licensePlate: "",
      type: "",
      brand: "",
      model: "",
      year: "",
    });
    setErrors({});
    setTouched({
      licensePlate: false,
      type: false,
      brand: false,
      model: false,
      year: false,
    });
  };

  // Field component for consistent styling
  const FormField: React.FC<{
    label: string;
    required?: boolean;
    error?: string;
    touched?: boolean;
    children: React.ReactNode;
  }> = ({ label, required, error, touched, children }) => (
    <div className="space-y-1">
      <label className="block text-sm font-medium text-gray-700">
        {label} {required && <span className="text-red-500">*</span>}
      </label>
      {children}
      {touched && error && (
        <div className="flex items-center gap-1 text-sm text-red-600">
          <AlertCircle size={14} />
          <span>{error}</span>
        </div>
      )}
    </div>
  );

  const isFormValid = !Object.values(errors).some(error => error !== undefined) && 
                     Object.values(form).every(value => value.trim() !== "");

  return (
    <form onSubmit={handleSubmit} className="space-y-6">
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
        {/* License Plate */}
        <FormField
          label="Biển số xe"
          required
          error={errors.licensePlate}
          touched={touched.licensePlate}
        >
          <input
            type="text"
            className={`w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-violet-500 focus:border-violet-500 transition-colors ${
              touched.licensePlate && errors.licensePlate
                ? 'border-red-500 focus:ring-red-500 focus:border-red-500'
                : 'border-gray-300'
            }`}
            placeholder="51A-12345"
            value={form.licensePlate}
            onChange={(e) => handleFieldChange('licensePlate', e.target.value.toUpperCase())}
            onBlur={() => handleFieldBlur('licensePlate')}
            disabled={isLoading}
            maxLength={10}
          />
        </FormField>

        {/* Vehicle Type */}
        <FormField
          label="Loại xe"
          required
          error={errors.type}
          touched={touched.type}
        >
          <select
            className={`w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-violet-500 focus:border-violet-500 transition-colors bg-white ${
              touched.type && errors.type
                ? 'border-red-500 focus:ring-red-500 focus:border-red-500'
                : 'border-gray-300'
            }`}
            value={form.type}
            onChange={(e) => handleFieldChange('type', e.target.value)}
            onBlur={() => handleFieldBlur('type')}
            disabled={isLoading}
          >
            <option value="">Chọn loại xe</option>
            {VEHICLE_TYPES.map(type => (
              <option key={type} value={type}>{type}</option>
            ))}
          </select>
        </FormField>

        {/* Brand */}
        <FormField
          label="Hãng xe"
          required
          error={errors.brand}
          touched={touched.brand}
        >
          <input
            type="text"
            className={`w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-violet-500 focus:border-violet-500 transition-colors ${
              touched.brand && errors.brand
                ? 'border-red-500 focus:ring-red-500 focus:border-red-500'
                : 'border-gray-300'
            }`}
            placeholder="Chọn hoặc nhập hãng xe"
            value={form.brand}
            onChange={(e) => handleFieldChange('brand', e.target.value)}
            onBlur={() => handleFieldBlur('brand')}
            disabled={isLoading}
            list="brands"
          />
          <datalist id="brands">
            {POPULAR_BRANDS.map(brand => (
              <option key={brand} value={brand} />
            ))}
          </datalist>
        </FormField>

        {/* Model */}
        <FormField
          label="Model"
          required
          error={errors.model}
          touched={touched.model}
        >
          <input
            type="text"
            className={`w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-violet-500 focus:border-violet-500 transition-colors ${
              touched.model && errors.model
                ? 'border-red-500 focus:ring-red-500 focus:border-red-500'
                : 'border-gray-300'
            }`}
            placeholder="Porter, Canter, FVM34W..."
            value={form.model}
            onChange={(e) => handleFieldChange('model', e.target.value)}
            onBlur={() => handleFieldBlur('model')}
            disabled={isLoading}
          />
        </FormField>

        {/* Year */}
        <FormField
          label="Năm sản xuất"
          required
          error={errors.year}
          touched={touched.year}
        >
          <input
            type="number"
            className={`w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-violet-500 focus:border-violet-500 transition-colors ${
              touched.year && errors.year
                ? 'border-red-500 focus:ring-red-500 focus:border-red-500'
                : 'border-gray-300'
            }`}
            placeholder="2020"
            value={form.year}
            onChange={(e) => handleFieldChange('year', e.target.value)}
            onBlur={() => handleFieldBlur('year')}
            disabled={isLoading}
            min="1980"
            max={new Date().getFullYear() + 1}
          />
        </FormField>
      </div>

      {/* Action Buttons */}
      <div className="flex flex-col sm:flex-row gap-3 pt-4 border-t border-gray-200">
        <button
          type="submit"
          disabled={!isFormValid || isLoading}
          className={`flex items-center justify-center gap-2 px-6 py-3 rounded-lg font-medium transition-all duration-200 ${
            isFormValid && !isLoading
              ? 'bg-violet-600 hover:bg-violet-700 text-white shadow-md hover:shadow-lg'
              : 'bg-gray-300 text-gray-500 cursor-not-allowed'
          }`}
        >
          {isLoading ? (
            <>
              <div className="w-4 h-4 border-2 border-white border-t-transparent rounded-full animate-spin" />
              <span>Đang thêm...</span>
            </>
          ) : (
            <>
              <Plus size={18} />
              <span>Thêm phương tiện</span>
            </>
          )}
        </button>

        <button
          type="button"
          onClick={handleReset}
          disabled={isLoading}
          className="flex items-center justify-center gap-2 px-6 py-3 rounded-lg font-medium border border-gray-300 text-gray-700 hover:bg-gray-50 transition-colors duration-200 disabled:opacity-50 disabled:cursor-not-allowed"
        >
          <X size={18} />
          <span>Đặt lại</span>
        </button>

        {isFormValid && (
          <div className="flex items-center gap-2 text-green-600 text-sm">
            <CheckCircle size={16} />
            <span>Form hợp lệ</span>
          </div>
        )}
      </div>
    </form>
  );
}
