DELETE FROM hdb_catalog.hdb_cron_events
WHERE trigger_name = $1 AND scheduled_time > now() AND tries = 0;
