import { Component } from 'react';
import type { ErrorInfo, ReactNode } from 'react';
interface Props {
    children: ReactNode;
}
interface State {
    hasError: boolean;
    error?: Error;
    retryCount: number;
}
export declare class MapErrorBoundary extends Component<Props, State> {
    state: State;
    private retryTimeout?;
    static getDerivedStateFromError(error: Error): Partial<State>;
    componentDidCatch(error: Error, errorInfo: ErrorInfo): void;
    componentWillUnmount(): void;
    render(): string | number | bigint | boolean | Iterable<ReactNode> | Promise<string | number | bigint | boolean | import("react").ReactPortal | import("react").ReactElement<unknown, string | import("react").JSXElementConstructor<any>> | Iterable<ReactNode> | null | undefined> | import("react/jsx-runtime").JSX.Element | null | undefined;
}
export default MapErrorBoundary;
