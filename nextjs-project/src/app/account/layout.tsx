"use client";

import AccountLayout from "@/components/layout/AccountLayout";

export default function Layout({ children }: { children: React.ReactNode }) {
  return <AccountLayout>{children}</AccountLayout>;
}
