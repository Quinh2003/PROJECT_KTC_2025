import { NextRequest, NextResponse } from 'next/server';

const API_URL = process.env.BACKEND_API_URL || 'http://localhost:8080/api/addresses';

// GET: Lấy tất cả địa chỉ
export async function GET() {
  try {
    const res = await fetch(API_URL);
    if (!res.ok) throw new Error('Failed to fetch addresses');
    const data = await res.json();
    return NextResponse.json(data);
  } catch (error: any) {
    return NextResponse.json({ error: error.message }, { status: 500 });
  }
}

// POST: Tạo mới địa chỉ
export async function POST(req: NextRequest) {
  try {
    const body = await req.json();
    console.log('[NextJS API] POST /api/address payload:', body);
    const res = await fetch(API_URL, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
    });
    const text = await res.text();
    if (!res.ok) {
      console.error('[NextJS API] Backend error:', text);
      return NextResponse.json({ error: `Backend error: ${res.status} - ${text}` }, { status: res.status });
    }
    let data;
    try {
      data = JSON.parse(text);
    } catch {
      data = text;
    }
    return NextResponse.json(data, { status: 201 });
  } catch (error: any) {
    console.error('[NextJS API] Exception:', error);
    return NextResponse.json({ error: error.message }, { status: 500 });
  }
}
