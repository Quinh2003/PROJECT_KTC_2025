import type { User } from "../types/User";

interface NavbarProps {
  user: User;
  onLogout: () => void;
  title: string;
  subtitle: string;
}

export default function Navbar({ user, onLogout, title, subtitle }: NavbarProps) {
  return (
    <header className="bg-white backdrop-blur-lg border-b border-white/30 px-6 py-4 flex items-center justify-between sticky top-0 z-30">
      <div>
        <h1 className="text-3xl font-bold text-gray-800 mb-1">
          {title}
        </h1>
        <div className="text-gray-600 text-base">
          {subtitle}
        </div>
      </div>
      <div className="flex items-center gap-4">
        <span className="text-gray-700 font-medium">Hello, {user.fullName}</span>
        <button
          className="px-4 py-2 bg-gradient-to-r from-red-400/20 to-pink-400/20 backdrop-blur-lg border border-red-300/30 text-red-700 rounded-lg hover:from-red-400/30 hover:to-pink-400/30 hover:border-red-300/50 transition-all duration-300 shadow-lg hover:shadow-xl hover:scale-105 font-medium"
          onClick={onLogout}
        >
          Logout
        </button>
      </div>
    </header>
  );
}