import { useState, useEffect } from 'react';
export const useSimpleTracking = () => {
    const [tracking, setTracking] = useState([]);
    const [error, setError] = useState(null);
    useEffect(() => {
        // Simple mock data - no async complications
        const mockData = [
            {
                id: 1,
                vehicleId: 1,
                latitude: 10.7769,
                longitude: 106.7009,
                status: "En Route",
                timestamp: new Date().toISOString()
            },
            {
                id: 2,
                vehicleId: 2,
                latitude: 10.7851,
                longitude: 106.6959,
                status: "Delivered",
                timestamp: new Date().toISOString()
            },
            {
                id: 3,
                vehicleId: 3,
                latitude: 10.7626,
                longitude: 106.6822,
                status: "Loading",
                timestamp: new Date().toISOString()
            },
            {
                id: 4,
                vehicleId: 4,
                latitude: 10.7460,
                longitude: 106.7198,
                status: "En Route",
                timestamp: new Date().toISOString()
            },
            {
                id: 5,
                vehicleId: 5,
                latitude: 10.8142,
                longitude: 106.6438,
                status: "Available",
                timestamp: new Date().toISOString()
            }
        ];
        setTracking(mockData);
        setError(null);
    }, []); // No dependencies - only run once
    return {
        tracking,
        error
    };
};
export default useSimpleTracking;
