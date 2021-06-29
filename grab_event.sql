UPDATE queue
SET status = 'processing'
WHERE id = (
  SELECT id
  FROM queue
  WHERE status = 'created'
  ORDER BY id
  FOR UPDATE SKIP LOCKED
  LIMIT 1
)
RETURNING id;
