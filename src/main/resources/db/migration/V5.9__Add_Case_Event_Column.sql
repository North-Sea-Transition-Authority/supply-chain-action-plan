ALTER TABLE scap_update_requests ADD case_event_id INTEGER;
ALTER TABLE scap_update_requests ADD CONSTRAINT fk_update_request_case_event FOREIGN KEY (case_event_id) REFERENCES case_events(case_event_id);
CREATE INDEX idx_scap_update_requests_scap_details_fk ON scap_update_requests(scap_detail_id);
