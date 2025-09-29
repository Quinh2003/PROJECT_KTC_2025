import { NextRequest, NextResponse } from 'next/server';

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    console.log('📦 NextJS API: Creating delivery with payload:', body);

    // Validate required fields
    if (!body.orderId) {
      return NextResponse.json(
        { error: 'Order ID is required' },
        { status: 400 }
      );
    }

    if (!body.serviceType) {
      return NextResponse.json(
        { error: 'Service type is required' },
        { status: 400 }
      );
    }

    // Forward request to Spring Boot backend
    const response = await fetch('http://localhost:8080/api/deliveries', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(body),
    });

    if (!response.ok) {
      const errorData = await response.text();
      console.error('❌ Spring Boot error:', errorData);
      return NextResponse.json(
        { error: `Backend error: ${errorData}` },
        { status: response.status }
      );
    }

    const result = await response.json();
    console.log('✅ Delivery created successfully:', result);
    
    return NextResponse.json(result, { status: 201 });
  } catch (error: any) {
    console.error('💥 NextJS API error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function GET() {
  try {
    const response = await fetch('http://localhost:8080/api/deliveries', {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json',
      },
    });

    if (!response.ok) {
      const errorData = await response.text();
      return NextResponse.json(
        { error: `Backend error: ${errorData}` },
        { status: response.status }
      );
    }

    const result = await response.json();
    return NextResponse.json(result);
  } catch (error: any) {
    console.error('💥 NextJS API error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
