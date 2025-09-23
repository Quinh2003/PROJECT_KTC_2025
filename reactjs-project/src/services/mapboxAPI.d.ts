export declare function searchLocation(query: string): Promise<any>;
export declare function getRoute(start: [number, number], waypoints: [number, number][], end: [number, number], token: string): Promise<any>;
export declare function postRoute(route: any, waypoints: [number, number][]): Promise<any>;
export declare function postTruckPosition(position: [number, number]): Promise<any>;
