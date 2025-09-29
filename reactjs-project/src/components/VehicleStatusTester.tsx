import React, { useState } from 'react';

interface VehicleStatusTesterProps {
  onStatusChange?: () => void;
}

const VehicleStatusTester: React.FC<VehicleStatusTesterProps> = ({ onStatusChange }) => {
  const [isLoading, setIsLoading] = useState(false);
  const [message, setMessage] = useState('');

  const updateVehicleStatus = async (vehicleId: number, statusId: number, statusName: string) => {
    setIsLoading(true);
    setMessage('');
    
    try {
      const token = localStorage.getItem('token');
      const response = await fetch(`http://localhost:8080/api/vehicles/${vehicleId}/status`, {
        method: 'PATCH',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${token}`
        },
        body: JSON.stringify({ statusId })
      });

      if (response.ok) {
        // Check if response has content before parsing JSON
        const contentType = response.headers.get('content-type');
        if (contentType && contentType.includes('application/json')) {
          const result = await response.json();
          setMessage(`✅ Xe ${vehicleId} đã chuyển thành ${statusName}`);
        } else {
          setMessage(`✅ Xe ${vehicleId} đã chuyển thành ${statusName} (no JSON response)`);
        }
        onStatusChange?.();
      } else {
        // Handle error response
        let errorMessage = `HTTP ${response.status}`;
        try {
          const contentType = response.headers.get('content-type');
          if (contentType && contentType.includes('application/json')) {
            const error = await response.json();
            errorMessage = error.error || error.message || errorMessage;
          } else {
            const textError = await response.text();
            errorMessage = textError || errorMessage;
          }
        } catch (parseError) {
          console.warn('Could not parse error response:', parseError);
        }
        setMessage(`❌ Lỗi: ${errorMessage}`);
      }
    } catch (error) {
      console.error('Error updating vehicle status:', error);
      setMessage(`❌ Lỗi kết nối: ${error}`);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="bg-white rounded-lg shadow-lg p-6 mb-6">
      <h3 className="text-lg font-semibold text-gray-800 mb-4">🧪 Test SSE Realtime</h3>
      <p className="text-sm text-gray-600 mb-4">
        Nhấn các nút bên dưới để thay đổi status xe và xem số liệu cập nhật realtime (không cần reload trang)
      </p>
      
      <div className="grid grid-cols-2 gap-4 mb-4">
        <div>
          <h4 className="font-medium text-gray-700 mb-2">Xe ID: 1</h4>
          <div className="space-y-2">
            <button
              onClick={() => updateVehicleStatus(1, 18, 'ĐANG SỬ DỤNG')}
              disabled={isLoading}
              className="w-full px-4 py-2 bg-red-500 text-white rounded hover:bg-red-600 disabled:opacity-50"
            >
              Set ĐANG SỬ DỤNG (18)
            </button>
            <button
              onClick={() => updateVehicleStatus(1, 17, 'SẴN SÀNG')}
              disabled={isLoading}
              className="w-full px-4 py-2 bg-green-500 text-white rounded hover:bg-green-600 disabled:opacity-50"
            >
              Set SẴN SÀNG (17)
            </button>
          </div>
        </div>
        
        <div>
          <h4 className="font-medium text-gray-700 mb-2">Xe ID: 2</h4>
          <div className="space-y-2">
            <button
              onClick={() => updateVehicleStatus(2, 18, 'ĐANG SỬ DỤNG')}
              disabled={isLoading}
              className="w-full px-4 py-2 bg-red-500 text-white rounded hover:bg-red-600 disabled:opacity-50"
            >
              Set ĐANG SỬ DỤNG (18)
            </button>
            <button
              onClick={() => updateVehicleStatus(2, 17, 'SẴN SÀNG')}
              disabled={isLoading}
              className="w-full px-4 py-2 bg-green-500 text-white rounded hover:bg-green-600 disabled:opacity-50"
            >
              Set SẴN SÀNG (17)
            </button>
          </div>
        </div>
      </div>

      {isLoading && (
        <div className="text-center py-2">
          <span className="text-blue-600">⏳ Đang cập nhật...</span>
        </div>
      )}

      {message && (
        <div className="mt-4 p-3 rounded-md bg-gray-50 border">
          <p className="text-sm">{message}</p>
        </div>
      )}
    </div>
  );
};

export default VehicleStatusTester;