const stats = [
  { label: "Total shipments", value: 32, icon: "📦" },
  { label: "Pickup packages", value: 24, icon: "🚚" },
  { label: "Pending packages", value: 8, icon: "⏳" },
  { label: "Packages delivered", value: 4, icon: "✅" },
];

export default function StatsCards() {
  return (
    <div className="grid grid-cols-2 md:grid-cols-4 gap-6 mb-6">
      {stats.map((s, i) => (
        <div key={i} className="bg-white/30 backdrop-blur-lg rounded-2xl p-6 flex items-center gap-4 border border-white/30 shadow-lg hover:shadow-xl transition-all duration-300 hover:bg-white/40">
          <span className="text-3xl filter drop-shadow-sm">{s.icon}</span>
          <div>
            <div className="text-2xl font-bold text-gray-800">{s.value}</div>
            <div className="text-gray-600 font-medium">{s.label}</div>
          </div>
        </div>
      ))}
    </div>
  );
}