const API_BASE_URL = 'http://localhost:8080';
/**
 * Get store information by ID
 */
export async function getStoreById(storeId) {
    const token = localStorage.getItem("token");
    const response = await fetch(`${API_BASE_URL}/api/stores/${storeId}`, {
        method: 'GET',
        headers: {
            'Authorization': `Bearer ${token}`,
        },
    });
    if (response.status === 404) {
        return null;
    }
    if (!response.ok) {
        throw new Error(`Failed to get store: ${response.status} ${response.statusText}`);
    }
    return response.json();
}
/**
 * Get all stores
 */
export async function getAllStores() {
    const token = localStorage.getItem("token");
    const response = await fetch(`${API_BASE_URL}/api/stores`, {
        method: 'GET',
        headers: {
            'Authorization': `Bearer ${token}`,
        },
    });
    if (!response.ok) {
        throw new Error(`Failed to get stores: ${response.status} ${response.statusText}`);
    }
    return response.json();
}
/**
 * Get store coordinates for tracking start point
 */
export async function getStoreCoordinates(storeId) {
    try {
        const store = await getStoreById(storeId);
        if (store && store.latitude && store.longitude) {
            return {
                latitude: store.latitude,
                longitude: store.longitude
            };
        }
        return null;
    }
    catch (error) {
        console.error('Error getting store coordinates:', error);
        return null;
    }
}
