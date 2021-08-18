UPDATE hdb_cron_events 
SET status = 'locked' 
WHERE 
  id IN (
    SELECT t.id 
    FROM hdb_cron_events t 
    WHERE 
      ( t.status = 'scheduled' 
        AND ( ( t.next_retry_at is NULL and t.scheduled_time <= now()) 
          OR ( t.next_retry_at is not NULL and t.next_retry_at <= now())
        ) AND project_id = ?
      ) FOR UPDATE SKIP LOCKED 
    LIMIT ?
  ) AND project_id = ?;
