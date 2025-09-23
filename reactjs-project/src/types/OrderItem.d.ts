export interface OrderItem {
    id: number;
    orderId: number;
    productName?: string;
    quantity: number;
    product?: {
        id: number;
        name: string;
    };
}
