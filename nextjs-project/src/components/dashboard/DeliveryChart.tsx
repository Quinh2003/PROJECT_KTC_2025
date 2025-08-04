'use client'

import { AreaChart, Area, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from 'recharts'

const data = [
  { name: 'Mon', deliveries: 45, completed: 42 },
  { name: 'Tue', deliveries: 52, completed: 48 },
  { name: 'Wed', deliveries: 38, completed: 35 },
  { name: 'Thu', deliveries: 64, completed: 58 },
  { name: 'Fri', deliveries: 78, completed: 72 },
  { name: 'Sat', deliveries: 56, completed: 52 },
  { name: 'Sun', deliveries: 43, completed: 39 },
]

export default function DeliveryChart() {
  return (
    <div className="rounded-lg bg-white p-6 shadow">
      <div className="mb-4">
        <h3 className="text-lg font-medium text-gray-900">Weekly Delivery Overview</h3>
        <p className="text-sm text-gray-500">Total vs Completed deliveries this week</p>
      </div>
      <div className="h-64">
        <ResponsiveContainer width="100%" height="100%">
          <AreaChart data={data}>
            <CartesianGrid strokeDasharray="3 3" />
            <XAxis dataKey="name" />
            <YAxis />
            <Tooltip />
            <Area
              type="monotone"
              dataKey="deliveries"
              stackId="1"
              stroke="#8884d8"
              fill="#8884d8"
              fillOpacity={0.6}
            />
            <Area
              type="monotone"
              dataKey="completed"
              stackId="1"
              stroke="#82ca9d"
              fill="#82ca9d"
              fillOpacity={0.8}
            />
          </AreaChart>
        </ResponsiveContainer>
      </div>
      <div className="mt-4 flex justify-center space-x-6">
        <div className="flex items-center">
          <div className="h-3 w-3 rounded-full bg-[#8884d8]"></div>
          <span className="ml-2 text-sm text-gray-600">Total Deliveries</span>
        </div>
        <div className="flex items-center">
          <div className="h-3 w-3 rounded-full bg-[#82ca9d]"></div>
          <span className="ml-2 text-sm text-gray-600">Completed</span>
        </div>
      </div>
    </div>
  )
}
