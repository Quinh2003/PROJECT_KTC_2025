export default function DriverList() {
  const drivers = [
    { name: "Tài xế Demo", phone: "0901234567", vehicle: "51A-12345", status: "Available" },
    { name: "Trần Văn B", phone: "0901234568", vehicle: "51B-67890", status: "Busy" },
    { name: "Lê Văn C", phone: "0901234569", vehicle: "51C-11111", status: "Available" },
  ];
  return (
    <div className="bg-white rounded-xl p-6 shadow">
      <div className="text-xl font-bold mb-2">Driver List</div>
      <div className="text-gray-500 mb-4">Status and driver information</div>
      <div className="flex flex-col gap-4">
        {drivers.map((d, idx) => (
          <div key={idx} className="rounded-xl border border-gray-200 p-4 flex justify-between items-center">
            <div>
              <div className="font-semibold">{d.name}</div>
              <div className="text-sm text-gray-500">📞 {d.phone}</div>
              <div className="text-sm text-gray-500">Xe: {d.vehicle}</div>
            </div>
            <span className={`px-3 py-1 rounded-full text-sm font-semibold ${d.status === "Available" ? "bg-green-100 text-green-700" : "bg-red-100 text-red-700"}`}>
              {d.status}
            </span>
          </div>
        ))}
      </div>
    </div>
  );
}