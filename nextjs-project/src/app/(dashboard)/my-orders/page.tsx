import React from "react";
import { getServerSession } from "next-auth";
import { redirect } from "next/navigation";

export default async function MyOrders() {
  const session = await getServerSession();

  if (!session) {
    redirect("/login");
  }

  return (
    <div className="container mx-auto p-6">
      <h1 className="text-2xl font-bold mb-4">My Orders</h1>
      <div className="bg-white rounded-lg shadow p-6">
        {/* Order list content will go here */}
        <p>Welcome {session.user?.name || session.user?.email}</p>
      </div>
    </div>
  );
}
