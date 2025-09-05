"use client";

import { useSession } from "next-auth/react";
import { Card, Typography, Space, Tag } from "antd";

const { Text, Paragraph } = Typography;

export default function DebugSession() {
  const { data: session, status } = useSession();

  return (
    <Card
      title="Debug Session Info"
      style={{ margin: "20px 0", backgroundColor: "#f5f5f5" }}
    >
      <Space direction="vertical" style={{ width: "100%" }}>
        <div>
          <Text strong>Status: </Text>
          <Tag
            color={
              status === "authenticated"
                ? "green"
                : status === "loading"
                ? "blue"
                : "red"
            }
          >
            {status}
          </Tag>
        </div>

        <div>
          <Text strong>Session Data:</Text>
          <Paragraph>
            <pre>{JSON.stringify(session, null, 2)}</pre>
          </Paragraph>
        </div>

        {session?.user && (
          <div>
            <Text strong>User ID: </Text>
            <Text code>{session.user.id || "undefined"}</Text>
          </div>
        )}
      </Space>
    </Card>
  );
}
