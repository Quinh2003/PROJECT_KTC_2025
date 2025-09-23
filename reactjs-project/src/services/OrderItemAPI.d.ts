export declare function fetchOrdersTotalQuantityBatch(orderIds: (number | string)[]): Promise<{
    [orderId: string]: number;
}>;
export declare function fetchOrderItemsByOrderIdPaged(orderId: number | string, page?: number, size?: number): Promise<{
    content: ProductItem[];
    totalElements: number;
    totalPages: number;
}>;
export declare function fetchAllOrderItemsPaged(page?: number, size?: number): Promise<{
    content: ProductItem[];
    totalElements: number;
    totalPages: number;
}>;
export interface ProductItem {
    id: number;
    product: {
        name: string;
    };
    quantity: number;
    shippingFee?: number;
    notes?: string;
}
export declare function fetchOrderItemsByOrderId(orderId: number | string): Promise<ProductItem[]>;
export declare function fetchOrderTotalQuantity(orderId: number | string): Promise<number>;
