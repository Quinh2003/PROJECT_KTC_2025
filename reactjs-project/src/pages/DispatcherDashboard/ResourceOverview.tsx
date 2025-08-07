import { useState } from "react";
import DriverList from "./DriverList";
import VehicleList from "./VehicleList";
import { MdPeople, MdDirectionsCar } from "react-icons/md";

export default function ResourceOverview() {
  const [tab, setTab] = useState<"drivers" | "vehicles">("drivers");
  return (
    <div>
      <div className="flex gap-2 mb-6">
        <button
          className={`flex items-center gap-2 px-4 py-2 rounded font-semibold border ${
            tab === "drivers"
              ? "bg-white shadow text-[#58ADCC] border-[#58ADCC]"
              : "bg-gray-100 text-gray-600 border-transparent"
          }`}
          onClick={() => setTab("drivers")}
        >
          <MdPeople className="text-xl" />
          Driver List
        </button>
        <button
          className={`flex items-center gap-2 px-4 py-2 rounded font-semibold border ${
            tab === "vehicles"
              ? "bg-white shadow text-[#58ADCC] border-[#58ADCC]"
              : "bg-gray-100 text-gray-600 border-transparent"
          }`}
          onClick={() => setTab("vehicles")}
        >
          <MdDirectionsCar className="text-xl" />
          Vehicle List
        </button>
      </div>
      <div className=" gap-6">
        {tab === "drivers" && <DriverList />}
        {tab === "vehicles" && <VehicleList />}
      </div>
    </div>
  );
}