import { cookies } from "next/headers";

export const getAuthToken = async () => {
  const cookieStore = await cookies();
  return cookieStore.get("auth_token")?.value;
};

export const setAuthToken = async (token: string) => {
  const cookieStore = await cookies();
  cookieStore.set("auth_token", token, {
    httpOnly: true,
    secure: process.env.NODE_ENV === "production",
    sameSite: "strict",
    path: "/",
  });
};

export const clearAuthToken = async () => {
  const cookieStore = await cookies();
  cookieStore.delete("auth_token");
};
