import { jsx as _jsx, jsxs as _jsxs, Fragment as _Fragment } from "react/jsx-runtime";
import { Component } from 'react';
export class MapErrorBoundary extends Component {
    state = {
        hasError: false,
        retryCount: 0
    };
    retryTimeout;
    static getDerivedStateFromError(error) {
        // Update state so the next render will show the fallback UI
        return { hasError: true, error };
    }
    componentDidCatch(error, errorInfo) {
        console.warn('Map Error Boundary caught an error:', error, errorInfo);
        // Auto-retry for DOM manipulation errors (common with Mapbox)
        if (error.name === 'NotFoundError' && this.state.retryCount < 2) {
            this.retryTimeout = window.setTimeout(() => {
                this.setState(prevState => ({
                    hasError: false,
                    error: undefined,
                    retryCount: prevState.retryCount + 1
                }));
            }, 2000); // Auto-retry after 2 seconds
        }
    }
    componentWillUnmount() {
        if (this.retryTimeout) {
            clearTimeout(this.retryTimeout);
        }
    }
    render() {
        if (this.state.hasError) {
            const isAutoRetrying = this.state.error?.name === 'NotFoundError' && this.state.retryCount < 2;
            return (_jsx("div", { className: "bg-white rounded-xl shadow-lg p-6 h-full min-h-[300px] w-full flex flex-col items-center justify-center", children: _jsxs("div", { className: "text-center max-w-md", children: [_jsx("div", { className: "text-blue-500 mb-4", children: isAutoRetrying ? (_jsx("div", { className: "animate-spin rounded-full h-16 w-16 border-b-2 border-blue-600 mx-auto" })) : (_jsx("svg", { className: "w-16 h-16 mx-auto", fill: "none", stroke: "currentColor", viewBox: "0 0 24 24", children: _jsx("path", { strokeLinecap: "round", strokeLinejoin: "round", strokeWidth: 2, d: "M9 20l-5.447-2.724A1 1 0 013 16.382V5.618a1 1 0 011.447-.894L9 7m0 13l6-3m-6 3V7m6 10l4.553 2.276A1 1 0 0021 18.382V7.618a1 1 0 00-.553-.894L15 4m0 13V4m0 0L9 7" }) })) }), _jsx("h3", { className: "text-xl font-semibold text-gray-800 mb-3", children: isAutoRetrying ? 'Reloading Map...' : 'Map Temporarily Unavailable' }), _jsx("p", { className: "text-gray-600 mb-2 text-sm", children: isAutoRetrying
                                ? 'The map is automatically recovering from a temporary issue.'
                                : 'The map experienced a temporary loading issue.' }), !isAutoRetrying && (_jsxs(_Fragment, { children: [_jsx("p", { className: "text-gray-500 mb-6 text-xs", children: "This can happen due to network connectivity or DOM conflicts." }), _jsxs("div", { className: "space-y-3", children: [_jsx("button", { onClick: () => this.setState({ hasError: false, error: undefined, retryCount: 0 }), className: "w-full px-6 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors font-medium", children: "Retry Map Loading" }), _jsx("p", { className: "text-xs text-gray-400", children: "Map functionality will work normally after successful loading" })] })] })), isAutoRetrying && (_jsx("p", { className: "text-gray-500 text-xs", children: "Please wait, the map will reload automatically..." }))] }) }));
        }
        return this.props.children;
    }
}
export default MapErrorBoundary;
