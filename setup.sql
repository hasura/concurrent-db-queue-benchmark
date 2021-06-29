DROP TABLE IF EXISTS queue;
CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE TABLE queue(
  id uuid NOT NULL DEFAULT gen_random_uuid(),
  status TEXT NOT NULL DEFAULT 'created',
  stuff TEXT NOT NULL,
  CHECK (status IN ('created', 'processing', 'completed', 'error'))
);
