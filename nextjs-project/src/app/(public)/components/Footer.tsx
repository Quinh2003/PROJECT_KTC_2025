import React from "react";

interface FooterColProps {
  title: string;
  items: string[];
  highlight?: boolean;
}

const FooterCol = ({ title, items, highlight = false }: FooterColProps) => (
  <div>
    <h4
      className={`font-semibold mb-4 ${
        highlight ? "text-base sm:text-lg text-green-600" : "text-green-600"
      }`}
    >
      {title}
    </h4>
    <ul className="space-y-2 text-gray-400 text-sm sm:text-base">
      {items.map((item, idx) => (
        <li
          key={idx}
          className="hover:text-white cursor-pointer transition-colors"
        >
          {item}
        </li>
      ))}
    </ul>
  </div>
);

export default function Footer() {
  return (
    <footer className="bg-gradient-to-r from-gray-900 to-black text-white py-12 mt-16">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-8">
          <FooterCol
            title="Fast Route"
            items={[
              "Fast and reliable shipping service",
              "Top quality commitment",
              "24/7 customer service",
            ]}
            highlight
          />
          <FooterCol
            title="Services"
            items={[
              "Local delivery",
              "Inter-city delivery",
              "International shipping",
              "Express delivery",
              "COD delivery",
            ]}
          />
          <FooterCol
            title="Customer Support"
            items={[
              "Hotline: 1900-xxxx",
              "Email: support@fastroute.com",
              "Support center",
              "FAQ",
              "User guide",
            ]}
          />
          <FooterCol
            title="Connect with us"
            items={["Facebook", "Instagram", "LinkedIn", "YouTube", "Zalo"]}
          />
        </div>

        {/* Divider */}
        <div className="border-t border-gray-700 mt-8 pt-8">
          <div className="flex flex-col sm:flex-row justify-between items-center">
            <div className="text-center sm:text-left text-gray-400 text-xs sm:text-sm mb-4 sm:mb-0">
              &copy; 2025 Fast Route. All rights reserved.
            </div>
            <div className="flex items-center gap-6 text-xs sm:text-sm text-gray-400">
              <span className="hover:text-white cursor-pointer transition-colors">
                Privacy Policy
              </span>
              <span className="hover:text-white cursor-pointer transition-colors">
                Terms of Use
              </span>
              <span className="hover:text-white cursor-pointer transition-colors">
                Sitemap
              </span>
            </div>
          </div>
        </div>
      </div>
    </footer>
  );
}
