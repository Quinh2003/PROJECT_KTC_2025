'use client'

import { TruckIcon, ClipboardDocumentListIcon, UserGroupIcon, CheckCircleIcon } from '@heroicons/react/24/outline'

const stats = [
  {
    name: 'Total Orders',
    value: '1,234',
    change: '+12.5%',
    changeType: 'positive' as const,
    icon: ClipboardDocumentListIcon,
  },
  {
    name: 'Active Deliveries',
    value: '89',
    change: '+4.3%',
    changeType: 'positive' as const,
    icon: TruckIcon,
  },
  {
    name: 'Available Drivers',
    value: '24',
    change: '-2.1%',
    changeType: 'negative' as const,
    icon: UserGroupIcon,
  },
  {
    name: 'Completed Today',
    value: '156',
    change: '+8.7%',
    changeType: 'positive' as const,
    icon: CheckCircleIcon,
  },
]

function classNames(...classes: string[]) {
  return classes.filter(Boolean).join(' ')
}

export default function StatsCards() {
  return (
    <div className="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-4">
      {stats.map((stat) => (
        <div
          key={stat.name}
          className="relative overflow-hidden rounded-lg bg-white px-4 py-5 shadow sm:px-6 sm:py-6"
        >
          <dt>
            <div className="absolute rounded-md bg-indigo-500 p-3">
              <stat.icon className="h-6 w-6 text-white" aria-hidden="true" />
            </div>
            <p className="ml-16 truncate text-sm font-medium text-gray-500">{stat.name}</p>
          </dt>
          <dd className="ml-16 flex items-baseline">
            <p className="text-2xl font-semibold text-gray-900">{stat.value}</p>
            <p
              className={classNames(
                stat.changeType === 'positive' ? 'text-green-600' : 'text-red-600',
                'ml-2 flex items-baseline text-sm font-semibold'
              )}
            >
              {stat.change}
            </p>
          </dd>
        </div>
      ))}
    </div>
  )
}
