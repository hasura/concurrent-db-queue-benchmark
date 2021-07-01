DROP TABLE IF EXISTS hdb_cron_events CASCADE;
DROP EXTENSION IF EXISTS pgcrypto CASCADE;
CREATE EXTENSION pgcrypto;
CREATE TABLE hdb_cron_events
(
  id TEXT DEFAULT gen_random_uuid() PRIMARY KEY,
  project_id TEXT NOT NULL,
  trigger_name TEXT NOT NULL,
  scheduled_time TIMESTAMPTZ NOT NULL,
  status TEXT NOT NULL DEFAULT 'scheduled',
  tries INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  next_retry_at TIMESTAMPTZ,

  CONSTRAINT valid_status CHECK (status IN ('scheduled','locked','delivered','error','dead'))
);

CREATE INDEX hdb_cron_events_project_id_status ON hdb_cron_events (project_id, status);

CREATE UNIQUE INDEX hdb_cron_events_unique_scheduled
ON hdb_cron_events (project_id, trigger_name, scheduled_time)
WHERE status = 'scheduled';

-- for project1
insert into hdb_cron_events (trigger_name, project_id, scheduled_time)
select 'test_cron_trigger', 'project1', generate_series('2008-01-01 00:00'::timestamp, '2009-01-01 00:00', '1 minute') as scheduled_time;

-- for project2
insert into hdb_cron_events (trigger_name, project_id, scheduled_time)
select 'test_cron_trigger', 'project2', generate_series('2008-01-01 00:00'::timestamp, '2009-01-01 00:00', '1 minute') as scheduled_time;
