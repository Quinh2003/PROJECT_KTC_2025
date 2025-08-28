import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import { orderApi } from "@/services/orderService";
import type { Order } from "@/types/orders";

export const useOrders = () => {
  return useQuery<Order[]>({
    queryKey: ["orders"],
    queryFn: orderApi.getOrders,
  });
};

export const useOrder = (id: string) => {
  return useQuery<Order>({
    queryKey: ["orders", id],
    queryFn: () => orderApi.getOrderById(id),
    enabled: !!id,
  });
};

export const useOrderTracking = (id: string) => {
  return useQuery({
    queryKey: ["orders", id, "tracking"],
    queryFn: () => orderApi.getOrderTracking(id),
    enabled: !!id,
  });
};

export const useCreateOrder = () => {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: orderApi.createOrder,
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["orders"] });
    },
  });
};

export const useUpdateOrder = () => {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: (params: { id: string; data: Partial<Order> }) =>
      orderApi.updateOrder(params.id, params.data),
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: ["orders"] });
      queryClient.invalidateQueries({ queryKey: ["orders", variables.id] });
    },
  });
};

export const useDeleteOrder = () => {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: orderApi.deleteOrder,
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["orders"] });
    },
  });
};
