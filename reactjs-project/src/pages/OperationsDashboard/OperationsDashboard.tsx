import { useState } from 'react';
import type { User } from "../../types/User";
import Sidebar, { type OperationsTab } from '../../components/Sidebar';
import Navbar from '../../components/Navbar';
import ResourceMonitoring from './ResourceMonitoring';
import PerformanceAnalytics from './PerformanceAnalytics';
import StaffManagement from './StaffManagement';
import OperationsOverview from './OperationsOverview';

interface OperationsDashboardProps {
  user: User;
  onLogout: () => void;
}

export default function OperationsDashboard({ user, onLogout }: OperationsDashboardProps) {
  const [tab, setTab] = useState<OperationsTab>("overview");

  return (
    <div className="min-h-screen flex bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100">
      <Sidebar<OperationsTab>
        activeTab={tab}
        onTabChange={tab => setTab(tab as OperationsTab)}
        role="operations"
      />

      {/* Main content */}
      <div className="flex-1 flex flex-col">
        <Navbar 
          user={user}
          onLogout={onLogout}
          title="Operations Manager Dashboard"
          subtitle="Monitor and manage logistics operations efficiently"
        />
        <main className="flex-1 p-6">
          {tab === "overview" && <OperationsOverview />}
          {tab === "monitoring" && <ResourceMonitoring />}
          {tab === "performance" && <PerformanceAnalytics />}
          {tab === "staff" && <StaffManagement />}
        </main>
      </div>
    </div>
  );
}
