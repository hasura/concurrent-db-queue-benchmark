insert into hdb_cron_events (trigger_name, project_id, scheduled_time)
select 'test_cron_trigger', ?,
  generate_series(
    '2008-01-01 00:00'::timestamp, 
    '2008-01-01 00:00'::timestamp + ? * '1 minute'::interval - '1 minute'::interval, '1 minute') 
  as scheduled_time;
