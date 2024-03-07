CREATE TABLE timeline_events (
  timeline_id                   INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY
, subject                       VARCHAR(50)
, scap_id                       INT
, version_number                INT
, event_time                    TIMESTAMP
, event_by                      INT
, CONSTRAINT fk_timeline_events_scap_id
  FOREIGN KEY (scap_id) REFERENCES scaps(scap_id)
);

CREATE INDEX idx_timeline_events_scap_id ON timeline_events(scap_id);