
import type { Vehicle } from "../types";

const API_URL = "http://localhost:8080/api/vehicles";

function getAuthHeaders(): HeadersInit | undefined {
  const token = localStorage.getItem("token");
  return token ? { Authorization: `Bearer ${token}` } : undefined;
}


export async function fetchVehicles(): Promise<Vehicle[]> {
  const headers = getAuthHeaders();
  const res = await fetch(API_URL, headers ? { headers } : undefined);
  if (!res.ok) throw new Error("Failed to fetch vehicles");
  return res.json();
}


export async function addVehicle(vehicle: Partial<Vehicle>): Promise<Vehicle> {
  const token = localStorage.getItem("token");
  const res = await fetch(API_URL, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${token}`,
    },
    body: JSON.stringify(vehicle),
  });
  if (!res.ok) throw new Error("Failed to add vehicle");
  return res.json();
}


export async function editVehicle(id: string | number, vehicle: Partial<Vehicle>): Promise<Vehicle> {
  const token = localStorage.getItem("token");
  const res = await fetch(`${API_URL}/${id}`, {
    method: "PATCH",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${token}`,
    },
    body: JSON.stringify(vehicle),
  });
  if (!res.ok) throw new Error("Failed to update vehicle");
  return res.json();
}


export async function deleteVehicle(id: string | number): Promise<boolean> {
  const token = localStorage.getItem("token");
  const res = await fetch(`${API_URL}/${id}`, {
    method: "DELETE",
    headers: {
      "Authorization": `Bearer ${token}`,
    },
  });
  if (!res.ok) throw new Error("Failed to delete vehicle");
  return true;
}


export async function fetchVehicleById(id: string | number): Promise<Vehicle> {
  const headers = getAuthHeaders();
  const res = await fetch(`${API_URL}/${id}`, headers ? { headers } : undefined);
  if (!res.ok) throw new Error("Failed to fetch vehicle");
  return res.json();
}


export async function updateVehicleStatus(vehicleId: string | number, status: string): Promise<Vehicle> {
  const token = localStorage.getItem("token");
  const res = await fetch(`${API_URL}/${vehicleId}/status`, {
    method: "PATCH",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${token}`,
    },
    body: JSON.stringify({ status }),
  });
  if (!res.ok) throw new Error("Failed to update vehicle status");
  return res.json();
}
// Gán tài xế cho xe (dùng endpoint riêng)
export async function assignDriverToVehicle(vehicleId: string | number, driverId: string | number | null): Promise<Vehicle> {
  const token = localStorage.getItem("token");
  const res = await fetch(`${API_URL}/${vehicleId}/assign-driver`, {
    method: "PATCH",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${token}`,
    },
    body: JSON.stringify({ driverId }),
  });
  if (!res.ok) throw new Error("Failed to assign driver to vehicle");
  return res.json();
}
