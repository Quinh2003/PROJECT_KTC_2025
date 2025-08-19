
import { MdInventory2, MdLocalShipping, MdHourglassEmpty, MdCheckCircle } from "react-icons/md";

const stats = [
  { label: "Total shipments", value: 32, icon: MdInventory2 },
  { label: "Pickup packages", value: 24, icon: MdLocalShipping },
  { label: "Pending packages", value: 8, icon: MdHourglassEmpty },
  { label: "Packages delivered", value: 4, icon: MdCheckCircle },
];

export default function StatsCards() {
  return (
    <div className="grid grid-cols-2 md:grid-cols-4 gap-6 mb-6">
      {stats.map((s, i) => {
        const Icon = s.icon;
        return (
          <div key={i} className="bg-black/30 backdrop-blur-xl rounded-2xl p-6 flex items-center justify-between gap-4 border border-white/10 shadow-lg hover:shadow-xl transition-all duration-300 hover:scale-105 hover:bg-black/40">
            <div>
              <div className="text-2xl font-bold text-white">{s.value}</div>
              <div className="text-slate-300 font-medium">{s.label}</div>
            </div>
            <Icon className="text-3xl text-cyan-400 drop-shadow-sm" />
          </div>
        );
      })}
    </div>
  );
}