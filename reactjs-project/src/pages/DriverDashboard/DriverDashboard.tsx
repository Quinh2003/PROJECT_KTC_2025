import { useTranslation } from 'react-i18next';
import type { User } from "../../types/User";

interface DriverDashboardProps {
  user: User;
  onLogout: () => void;
}

export default function DriverDashboard({ user, onLogout }: DriverDashboardProps) {
  const { t } = useTranslation();
  
  return (
    <div>
      <h2>{t('dashboard.driver.title', 'Driver Dashboard')}</h2>
      <p>{t('dashboard.driver.welcome', 'Hello')} {user.name} ({user.email})</p>
      <button onClick={onLogout}>{t('navigation.logout')}</button>
    </div>
  );
}