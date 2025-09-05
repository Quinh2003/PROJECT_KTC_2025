import { cookies } from "next/headers";

export const getAuthToken = () => {
  const cookieStore = cookies();
  return cookieStore.get("auth_token")?.value;
};

export const setAuthToken = (token: string) => {
  const cookieStore = cookies();
  cookieStore.set("auth_token", token, {
    httpOnly: true,
    secure: process.env.NODE_ENV === "production",
    sameSite: "strict",
    path: "/",
  });
};

export const clearAuthToken = () => {
  const cookieStore = cookies();
  cookieStore.delete("auth_token");
};
