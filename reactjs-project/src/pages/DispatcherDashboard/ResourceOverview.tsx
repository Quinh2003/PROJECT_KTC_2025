export default function VehicleList() {
  const vehicles = [
    { plate: "51A-12345", type: "Small truck", driver: "Demo Driver", status: "Available" },
    { plate: "51B-67890", type: "Medium truck", driver: "Tran Van B", status: "Busy" },
    { plate: "51C-11111", type: "Small truck", driver: "Le Van C", status: "Available" },
  ];
  return (
    <div className="bg-black/30 backdrop-blur-xl rounded-2xl p-6 border border-white/10 shadow-lg hover:bg-black/40 transition-all duration-300">
      <div className="text-xl font-bold mb-2 text-white">Vehicle List</div>
      <div className="text-slate-300 mb-4">Status and vehicle information</div>
      <div className="flex flex-col gap-4">
        {vehicles.map((v, idx) => (
          <div key={idx} className="rounded-xl bg-black/20 border border-white/10 p-4 flex justify-between items-center hover:bg-black/30 transition-all duration-300 shadow-md hover:shadow-lg">
            <div>
              <div className="font-semibold text-white">{v.plate}</div>
              <div className="text-sm text-slate-400">{v.type}</div>
              <div className="text-sm text-slate-400">Driver: {v.driver}</div>
            </div>
            <span className={`px-3 py-1 rounded-full text-sm font-semibold ${v.status === "Available" ? "bg-green-500/20 text-green-300 border border-green-400/30" : "bg-red-500/20 text-red-300 border border-red-400/30"}`}>
              {v.status}
            </span>
          </div>
        ))}
      </div>
    </div>
  );
}