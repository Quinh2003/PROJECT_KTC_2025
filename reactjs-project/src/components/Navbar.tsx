import type { User } from "../types/User";

interface NavbarProps {
  user: User;
  onLogout: () => void;
  title: string;
  subtitle: string;
}

export default function Navbar({ user, onLogout, title, subtitle }: NavbarProps) {
  return (
    <header className="bg-white/30 backdrop-blur-lg border-b border-white/30 px-6 py-4 flex items-center justify-between sticky top-0 z-30">
      <div>
        <h1 className="text-3xl font-bold text-gray-800 mb-1">
          {title}
        </h1>
        <div className="text-gray-600 text-base">
          {subtitle}
        </div>
      </div>
      <div className="flex items-center gap-4">
        <span className="text-gray-700 font-medium">Hello, {user.name}</span>
        <button
          className="px-4 py-2 bg-white/40 backdrop-blur-sm border border-white/50 text-red-700 rounded-lg hover:bg-white/60 transition-all duration-300 shadow-lg hover:shadow-xl"
          onClick={onLogout}
        >
          Logout
        </button>
      </div>
    </header>
  );
}