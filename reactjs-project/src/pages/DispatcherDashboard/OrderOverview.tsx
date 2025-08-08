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
        <div key={i} className="bg-white rounded-xl p-6 flex items-center gap-4 shadow">
          <span className="text-3xl">{s.icon}</span>
          <div>
            <div className="text-2xl font-bold">{s.value}</div>
            <div className="text-gray-500">{s.label}</div>
          </div>
        </div>
      ))}
    </div>
  );
}