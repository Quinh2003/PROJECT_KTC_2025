export default function DriverList() {
  const drivers = [
    { name: "Tài xế Demo", phone: "0901234567", vehicle: "51A-12345", status: "Available" },
    { name: "Trần Văn B", phone: "0901234568", vehicle: "51B-67890", status: "Busy" },
    { name: "Lê Văn C", phone: "0901234569", vehicle: "51C-11111", status: "Available" },
  ];
  return (
    <div className="bg-white/30 backdrop-blur-lg rounded-2xl p-6 border border-white/30 shadow-lg">
      <div className="text-xl font-bold mb-2 text-gray-800">Driver List</div>
      <div className="text-gray-600 mb-4">Status and driver information</div>
      <div className="flex flex-col gap-4">
        {drivers.map((d, idx) => (
          <div key={idx} className="rounded-xl bg-white/40 backdrop-blur-sm border border-white/40 p-4 flex justify-between items-center hover:bg-white/50 transition-all duration-300 shadow-md hover:shadow-lg">
            <div>
              <div className="font-semibold text-gray-800">{d.name}</div>
              <div className="text-sm text-gray-600">📞 {d.phone}</div>
              <div className="text-sm text-gray-600">Xe: {d.vehicle}</div>
            </div>
            <span className={`px-3 py-1 rounded-full text-sm font-semibold backdrop-blur-sm ${d.status === "Available" ? "bg-green-200/60 text-green-800 border border-green-300/50" : "bg-red-200/60 text-red-800 border border-red-300/50"}`}>
              {d.status}
            </span>
          </div>
        ))}
      </div>
    </div>
  );
}