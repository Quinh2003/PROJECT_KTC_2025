// import React from 'react';
// import type { TrackingPoint } from '../types';
export {};
// interface VehicleListProps {
//   tracking: TrackingPoint[];
//   selectedVehicle: number | null;
//   onVehicleClick: (point: TrackingPoint) => void;
// }
// export const VehicleList = React.memo(({ tracking, selectedVehicle, onVehicleClick }: VehicleListProps) => {
//   // Safe check for tracking data
//   const safeTracking = tracking || [];
//   return (
//     <div className="mt-4 flex-shrink-0">
//       <h3 className="text-lg font-semibold text-gray-800 mb-2">Active Vehicles</h3>
//       <div className="flex flex-wrap gap-2">
//         {safeTracking.map((point) => (
//           <div 
//             key={point.id} 
//             onClick={() => onVehicleClick(point)}
//             className={`px-3 py-1 rounded-full text-xs font-medium cursor-pointer transition-all duration-300 ${
//               selectedVehicle === point.vehicleId
//                 ? 'bg-blue-500 text-white ring-2 ring-blue-300 ring-offset-2 scale-105'
//                 : 'bg-blue-100 text-blue-800 hover:bg-blue-200'
//             }`}
//             title={`Click to view Vehicle ${point.vehicleId} on map`}
//           >
//             🚚 Vehicle {point.vehicleId}: <span className="font-semibold">{point.status}</span>
//           </div>
//         ))}
//         {safeTracking.length === 0 && (
//           <div className="text-gray-500 text-sm italic">No active vehicles</div>
//         )}
//       </div>
//     </div>
//   );
// });
// export default VehicleList;
