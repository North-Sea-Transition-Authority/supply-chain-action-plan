ALTER TABLE scap_update_requests DROP CONSTRAINT fk_scap_outstanding_scap_details_id;
ALTER TABLE scap_update_requests ADD CONSTRAINT fk_scap_outstanding_scap_id FOREIGN KEY (scap_id) REFERENCES scaps (scap_id);
