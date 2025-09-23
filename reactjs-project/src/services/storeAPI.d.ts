export interface Store {
    id: number;
    externalId?: number;
    storeName: string;
    email?: string;
    phone: string;
    address: string;
    latitude?: number;
    longitude?: number;
    isActive?: boolean;
    createdAt: string;
    updatedAt: string;
    createdBy?: number;
    notes?: string;
}
/**
 * Get store information by ID
 */
export declare function getStoreById(storeId: number): Promise<Store | null>;
/**
 * Get all stores
 */
export declare function getAllStores(): Promise<Store[]>;
/**
 * Get store coordinates for tracking start point
 */
export declare function getStoreCoordinates(storeId: number): Promise<{
    latitude: number;
    longitude: number;
} | null>;
