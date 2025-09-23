interface ProductItem {
    id: number;
    product: {
        name: string;
        weight?: number;
        volume?: number;
        fragile?: boolean;
    };
    quantity: number;
    shippingFee?: number;
    notes?: string;
}
interface OrderDetailModalProps {
    open: boolean;
    onClose: () => void;
    orderItem: {
        code: string;
        customer: string;
        status: string;
        date: string;
        address: string;
        from: string;
        to: string;
        note?: string;
        description?: string;
        assignedVehicle?: {
            licensePlate: string;
            vehicleType: string;
        };
        currentDriver?: {
            fullName?: string;
            username: string;
        };
    } | null;
    products?: ProductItem[];
    deliveryFee?: number;
    productsPage?: number;
    productsTotalPages?: number;
    onProductsPageChange?: (page: number) => void;
}
export default function OrderDetailModal({ open, onClose, orderItem, products, deliveryFee, productsPage, productsTotalPages, onProductsPageChange }: OrderDetailModalProps): import("react/jsx-runtime").JSX.Element | null;
export {};
