import { ImageResponse } from 'next/og';

export const alt = 'Chia sẻ từ hệ thống KTC 2025';
export const size = { width: 1200, height: 630 };
export const contentType = 'image/png';

export default function Image() {
  return new ImageResponse(
    (
      <div
        style={{
          fontSize: 48,
          background: 'white',
          width: '100%',
          height: '100%',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          flexDirection: 'column',
        }}
      >
        <span style={{ fontWeight: 'bold', color: '#0070f3' }}>
          KTC 2025 PROJECT 
        </span>
        <span style={{ fontSize: 24, marginTop: 16, color: '#333' }}>
          Chia sẻ thông tin, quản lý vận tải hiện đại
        </span>
      </div>
    ),
    {
      width: 1200,
      height: 630,
    }
  );
}
