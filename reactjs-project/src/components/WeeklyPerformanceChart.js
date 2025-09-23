import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { Line } from 'react-chartjs-2';
import { Chart as ChartJS, CategoryScale, LinearScale, PointElement, LineElement, Title, Tooltip, Legend, Filler } from 'chart.js';
import { WEEKLY_PERFORMANCE_DATA } from '../data/chartData';
ChartJS.register(CategoryScale, LinearScale, PointElement, LineElement, Title, Tooltip, Legend, Filler);
function WeeklyPerformanceChart() {
    // Chart configuration
    const options = {
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
            legend: {
                position: 'top',
                labels: {
                    color: '#6b7280',
                    usePointStyle: true,
                    padding: 20,
                    font: {
                        size: 12,
                        weight: 500
                    }
                }
            },
            tooltip: {
                backgroundColor: 'rgba(0, 0, 0, 0.8)',
                titleColor: '#fff',
                bodyColor: '#fff',
                borderColor: 'rgba(255, 255, 255, 0.3)',
                borderWidth: 1,
                cornerRadius: 8,
                displayColors: true,
                callbacks: {
                    title: (context) => `${context[0].label}`,
                    label: (context) => `${context.dataset.label}: ${context.parsed.y}%`
                }
            }
        },
        scales: {
            x: {
                grid: {
                    display: false
                },
                ticks: {
                    color: '#9ca3af',
                    font: {
                        size: 11,
                        weight: 500
                    }
                }
            },
            y: {
                beginAtZero: true,
                max: 100,
                grid: {
                    color: 'rgba(156, 163, 175, 0.2)'
                },
                ticks: {
                    color: '#9ca3af',
                    font: {
                        size: 11,
                        weight: 500
                    },
                    callback: function (value) {
                        return value + '%';
                    }
                }
            }
        },
        elements: {
            point: {
                radius: 4,
                hoverRadius: 6,
                borderWidth: 2
            },
            line: {
                borderWidth: 3
            }
        },
        interaction: {
            intersect: false,
            mode: 'index'
        }
    };
    // Create gradient data
    const chartData = {
        labels: WEEKLY_PERFORMANCE_DATA.labels,
        datasets: WEEKLY_PERFORMANCE_DATA.datasets.map((dataset, index) => {
            const colors = [
                { primary: '#3b82f6', secondary: '#93c5fd' }, // Blue
                { primary: '#10b981', secondary: '#86efac' }, // Green  
                { primary: '#f59e0b', secondary: '#fbbf24' } // Amber
            ];
            const color = colors[index] || colors[0];
            return {
                ...dataset,
                borderColor: color.primary,
                backgroundColor: `${color.secondary}40`,
                pointBackgroundColor: color.primary,
                pointBorderColor: '#ffffff',
                pointHoverBackgroundColor: color.primary,
                pointHoverBorderColor: '#ffffff'
            };
        })
    };
    return (_jsxs("div", { className: "bg-white/40 backdrop-blur-md rounded-xl p-6 border border-white/20 shadow-lg", children: [_jsxs("div", { className: "flex items-center justify-between mb-6", children: [_jsx("h3", { className: "text-lg font-semibold text-gray-800", children: "Hi\u1EC7u su\u1EA5t tu\u1EA7n n\u00E0y" }), _jsxs("div", { className: "flex gap-2", children: [_jsx("button", { className: "px-3 py-1 text-xs rounded-lg bg-blue-100 text-blue-600 hover:bg-blue-200 transition-colors", children: "7 ng\u00E0y" }), _jsx("button", { className: "px-3 py-1 text-xs rounded-lg bg-gray-100 text-gray-600 hover:bg-gray-200 transition-colors", children: "30 ng\u00E0y" })] })] }), _jsx("div", { className: "h-80", children: _jsx(Line, { data: chartData, options: options }) }), _jsxs("div", { className: "grid grid-cols-3 gap-4 mt-6 pt-4 border-t border-gray-200/50", children: [_jsxs("div", { className: "text-center", children: [_jsx("div", { className: "text-2xl font-bold text-blue-600", children: "84.3%" }), _jsx("div", { className: "text-sm text-gray-600", children: "Trung b\u00ECnh \u0111\u01A1n h\u00E0ng" })] }), _jsxs("div", { className: "text-center", children: [_jsx("div", { className: "text-2xl font-bold text-green-600", children: "87.0%" }), _jsx("div", { className: "text-sm text-gray-600", children: "Hi\u1EC7u su\u1EA5t xe" })] }), _jsxs("div", { className: "text-center", children: [_jsx("div", { className: "text-2xl font-bold text-amber-600", children: "86.7%" }), _jsx("div", { className: "text-sm text-gray-600", children: "\u0110\u00E1nh gi\u00E1 t\u00E0i x\u1EBF" })] })] })] }));
}
export default WeeklyPerformanceChart;
