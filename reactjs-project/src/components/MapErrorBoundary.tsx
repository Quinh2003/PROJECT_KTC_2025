import { Component } from 'react';
import type { ErrorInfo, ReactNode } from 'react';
import { withTranslation, WithTranslation } from 'react-i18next';

interface Props extends WithTranslation {
  children: ReactNode;
}

interface State {
  hasError: boolean;
  error?: Error;
  retryCount: number;
}

export class MapErrorBoundary extends Component<Props, State> {
  public state: State = {
    hasError: false,
    retryCount: 0
  };

  private retryTimeout?: number;

  public static getDerivedStateFromError(error: Error): Partial<State> {
    // Update state so the next render will show the fallback UI
    return { hasError: true, error };
  }

  public componentDidCatch(error: Error, errorInfo: ErrorInfo) {
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

  public componentWillUnmount() {
    if (this.retryTimeout) {
      clearTimeout(this.retryTimeout);
    }
  }

  public render() {
    if (this.state.hasError) {
      const isAutoRetrying = this.state.error?.name === 'NotFoundError' && this.state.retryCount < 2;
      
      return (
        <div className="bg-white rounded-xl shadow-lg p-6 h-full min-h-[300px] w-full flex flex-col items-center justify-center">
          <div className="text-center max-w-md">
            <div className="text-blue-500 mb-4">
              {isAutoRetrying ? (
                <div className="animate-spin rounded-full h-16 w-16 border-b-2 border-blue-600 mx-auto"></div>
              ) : (
                <svg className="w-16 h-16 mx-auto" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 20l-5.447-2.724A1 1 0 013 16.382V5.618a1 1 0 011.447-.894L9 7m0 13l6-3m-6 3V7m6 10l4.553 2.276A1 1 0 0021 18.382V7.618a1 1 0 00-.553-.894L15 4m0 13V4m0 0L9 7" />
                </svg>
              )}
            </div>
            
            <h3 className="text-xl font-semibold text-gray-800 mb-3">
              {isAutoRetrying ? this.props.t('common.loading', 'Loading') + '...' : this.props.t('errors.mapUnavailable', 'Map Temporarily Unavailable')}
            </h3>
            
            <p className="text-gray-600 mb-2 text-sm">
              {isAutoRetrying 
                ? 'The map is automatically recovering from a temporary issue.'
                : 'The map experienced a temporary loading issue.'
              }
            </p>
            
            {!isAutoRetrying && (
              <>
                <p className="text-gray-500 mb-6 text-xs">This can happen due to network connectivity or DOM conflicts.</p>
                <div className="space-y-3">
                  <button 
                    onClick={() => this.setState({ hasError: false, error: undefined, retryCount: 0 })}
                    className="w-full px-6 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors font-medium"
                  >
                    Retry Map Loading
                  </button>
                  <p className="text-xs text-gray-400">
                    Map functionality will work normally after successful loading
                  </p>
                </div>
              </>
            )}
            
            {isAutoRetrying && (
              <p className="text-gray-500 text-xs">
                Please wait, the map will reload automatically...
              </p>
            )}
          </div>
        </div>
      );
    }

    return this.props.children;
  }
}

export default withTranslation()(MapErrorBoundary);
