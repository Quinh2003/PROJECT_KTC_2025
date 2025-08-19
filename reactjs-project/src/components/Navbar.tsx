import type { User } from "../types/User";

interface NavbarProps {
  user: User;
  onLogout: () => void;
  title: string;
  subtitle: string;
}

export default function Navbar({ user, onLogout, title, subtitle }: NavbarProps) {
  return (
    <header className="px-6 py-4 flex items-center justify-between  rounded-tl-[32px]">
      <div>
        <h1 className="text-3xl font-bold text-black mb-1">
          {title}
        </h1>
        <div className="text-neutral-600 text-base">
          {subtitle}
        </div>
      </div>
      <div className="flex items-center gap-4">
        <span className="text-neutral-600 font-medium">Hello, {user.name}</span>
        <button
  className="px-4 py-2 
             bg-gradient-to-r from-red-500 to-red-600
             hover:from-red-400 hover:to-red-500
             text-white rounded-lg 
             transition-all duration-300 
             shadow-lg hover:shadow-xl hover:scale-105 
             font-medium"
  onClick={onLogout}
>
  Logout
</button>

      </div>
    </header>
  );
}