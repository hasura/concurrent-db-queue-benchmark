DROP TABLE IF EXISTS queue;
CREATE TABLE queue(
  id INTEGER PRIMARY KEY,
  status TEXT NOT NULL DEFAULT 'created',
  CHECK (status IN ('created', 'processing', 'completed', 'error'))
);

INSERT INTO queue(id, status)
SELECT x, 'created' FROM generate_series(1,10) x;
