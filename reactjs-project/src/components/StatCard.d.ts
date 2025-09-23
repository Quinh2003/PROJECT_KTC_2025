interface StatCardProps {
    title: string;
    value: string | number;
    subtitle?: string;
    trend?: {
        value: number;
        isPositive: boolean;
    };
    icon?: string;
}
export default function StatCard({ title, value, subtitle, trend, icon }: StatCardProps): import("react/jsx-runtime").JSX.Element;
export {};
