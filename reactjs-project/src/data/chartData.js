// Fake data for charts - centralized for easy tracking and reuse
export const WEEKLY_PERFORMANCE_DATA = {
    labels: ['Thứ 2', 'Thứ 3', 'Thứ 4', 'Thứ 5', 'Thứ 6', 'Thứ 7', 'CN'],
    datasets: [
        {
            label: 'Đơn hàng hoàn thành',
            data: [65, 78, 85, 92, 88, 95, 72],
            fill: true,
            tension: 0.4,
        },
        {
            label: 'Hiệu suất xe',
            data: [70, 82, 90, 88, 92, 89, 78],
            fill: true,
            tension: 0.4,
        },
        {
            label: 'Đánh giá tài xế',
            data: [80, 85, 88, 90, 87, 92, 85],
            fill: true,
            tension: 0.4,
        }
    ]
};
