export default function VehicleList() {
  const vehicles = [
    { plate: "51A-12345", type: "Small truck", driver: "Demo Driver", status: "Available" },
    { plate: "51B-67890", type: "Medium truck", driver: "Tran Van B", status: "Busy" },
    { plate: "51C-11111", type: "Small truck", driver: "Le Van C", status: "Available" },
  ];
  return (
    <div className="bg-white rounded-xl p-6 shadow">
      <div className="text-xl font-bold mb-2">Vehicle List</div>
      <div className="text-gray-500 mb-4">Status and vehicle information</div>
      <div className="flex flex-col gap-4">
        {vehicles.map((v, idx) => (
          <div key={idx} className="rounded-xl border border-gray-200 p-4 flex justify-between items-center">
            <div>
              <div className="font-semibold">{v.plate}</div>
              <div className="text-sm text-gray-500">{v.type}</div>
              <div className="text-sm text-gray-500">Driver: {v.driver}</div>
            </div>
            <span className={`px-3 py-1 rounded-full text-sm font-semibold ${v.status === "Available" ? "bg-green-100 text-green-700" : "bg-red-100 text-red-700"}`}>
              {v.status}
            </span>
          </div>
        ))}
      </div>
    </div>
  );
}