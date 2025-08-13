export default function VehicleList() {
  const vehicles = [
    { plate: "51A-12345", type: "Small truck", driver: "Demo Driver", status: "Available" },
    { plate: "51B-67890", type: "Medium truck", driver: "Tran Van B", status: "Busy" },
    { plate: "51C-11111", type: "Small truck", driver: "Le Van C", status: "Available" },
  ];
  return (
    <div className="bg-white/30 backdrop-blur-lg rounded-2xl p-6 border border-white/30 shadow-lg">
      <div className="text-xl font-bold mb-2 text-gray-800">Vehicle List</div>
      <div className="text-gray-600 mb-4">Status and vehicle information</div>
      <div className="flex flex-col gap-4">
        {vehicles.map((v, idx) => (
          <div key={idx} className="rounded-xl bg-white/40 backdrop-blur-sm border border-white/40 p-4 flex justify-between items-center hover:bg-white/50 transition-all duration-300 shadow-md hover:shadow-lg">
            <div>
              <div className="font-semibold text-gray-800">{v.plate}</div>
              <div className="text-sm text-gray-600">{v.type}</div>
              <div className="text-sm text-gray-600">Driver: {v.driver}</div>
            </div>
            <span className={`px-3 py-1 rounded-full text-sm font-semibold backdrop-blur-sm ${v.status === "Available" ? "bg-green-200/60 text-green-800 border border-green-300/50" : "bg-red-200/60 text-red-800 border border-red-300/50"}`}>
              {v.status}
            </span>
          </div>
        ))}
      </div>
    </div>
  );
}