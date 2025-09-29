"use client";

import { useState, useEffect } from "react";
import Image from "next/image";

interface SlideData {
  id: number;
  image: string;
  buttonText: string;
  buttonAction: () => void;
}

interface SliderProps {
  onCreateOrder: () => void;
  onLogin: () => void;
}

export default function Slider({ onCreateOrder, onLogin }: SliderProps) {
  const [currentSlide, setCurrentSlide] = useState(0);
  const [isAutoPlaying, setIsAutoPlaying] = useState(true);

  const slides: SlideData[] = [
    {
      id: 1,
      image: "/images/slider/slide1.jpg",
      buttonText: "TẠO ĐƠN NGAY",
      buttonAction: onCreateOrder,
    },
    {
      id: 2,
      image: "/images/slider/slide2.jpg",
      buttonText: "TÌM HIỂU THÊM",
      buttonAction: onLogin,
    },
    {
      id: 3,
      image: "/images/slider/slide3.jpg",
      buttonText: "LIÊN HỆ NGAY",
      buttonAction: onLogin,
    },
    {
      id: 4,
      image: "/images/slider/slide4.jpg",
      buttonText: "XEM BẢNG GIÁ",
      buttonAction: onLogin,
    },
    {
      id: 5,
      image: "/images/slider/slide5.png",
      buttonText: "TẢI ỨNG DỤNG",
      buttonAction: onLogin,
    },
  ];

  // Auto slide
  useEffect(() => {
    if (!isAutoPlaying) return;

    const interval = setInterval(() => {
      setCurrentSlide((prev) => (prev + 1) % slides.length);
    }, 5000);

    return () => clearInterval(interval);
  }, [isAutoPlaying, slides.length]);

  const goToSlide = (index: number) => {
    setCurrentSlide(index);
    setIsAutoPlaying(false);
    // Resume auto-play after 10 seconds
    setTimeout(() => setIsAutoPlaying(true), 10000);
  };

  return (
    <div className="relative w-full h-[400px] sm:h-[500px] lg:h-[600px] overflow-hidden bg-gradient-to-r from-green-100 to-green-50">
      {/* Slides */}
      <div
        className="flex transition-transform duration-500 ease-in-out h-full"
        style={{ transform: `translateX(-${currentSlide * 100}%)` }}
      >
        {slides.map((slide) => (
          <div key={slide.id} className="relative min-w-full h-full">
            {/* Background Image */}
            <div className="absolute inset-0">
              <Image
                src={slide.image}
                alt={`Slide ${slide.id}`}
                fill
                className="object-cover"
                priority={slide.id === 1}
              />
              {/* Overlay */}
              <div className="absolute inset-0 bg-black/20"></div>
            </div>

            {/* Content */}
            {/* <div className="relative h-full flex items-center">
              <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 w-full">
                <div className="max-w-2xl">
                  <button
                    onClick={slide.buttonAction}
                    className="bg-green-600 hover:bg-green-700 text-white px-8 py-4 rounded-full font-bold text-lg transition-all duration-300 transform hover:scale-105 shadow-lg"
                  >
                    {slide.buttonText}
                  </button>
                </div>
              </div>
            </div> */}
          </div>
        ))}
      </div>

      {/* Dots Indicator */}
      <div className="absolute bottom-6 left-1/2 transform -translate-x-1/2 flex space-x-2">
        {slides.map((_, index) => (
          <button
            key={index}
            onClick={() => goToSlide(index)}
            className={`w-3 h-3 rounded-full transition-all duration-200 ${
              currentSlide === index
                ? "bg-white scale-125"
                : "bg-white/50 hover:bg-white/75"
            }`}
            aria-label={`Go to slide ${index + 1}`}
          />
        ))}
      </div>
    </div>
  );
}
