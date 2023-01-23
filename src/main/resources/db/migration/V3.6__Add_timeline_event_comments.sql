ALTER TABLE timeline_events ADD comments TEXT;
ALTER TABLE timeline_events RENAME COLUMN timeline_id TO case_event_id;
ALTER TABLE timeline_events RENAME TO case_events;