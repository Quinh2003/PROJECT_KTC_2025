interface StatCardData {
    metric: string;
    current: number;
    target: number;
    trend: number;
}
interface PerformanceStatCardsProps {
    performanceData: StatCardData[];
}
export default function PerformanceStatCards({ performanceData }: PerformanceStatCardsProps): import("react/jsx-runtime").JSX.Element;
export {};
