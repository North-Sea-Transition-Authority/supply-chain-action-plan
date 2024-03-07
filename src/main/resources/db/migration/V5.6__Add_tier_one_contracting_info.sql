ALTER TABLE scap_details
  ADD COLUMN tier_one_contractor BOOLEAN,
  ADD COLUMN parent_scap_id INT,
  ADD CONSTRAINT fk_scap_details_parent_scap_id FOREIGN KEY (parent_scap_id) REFERENCES scaps(scap_id);

CREATE INDEX ON scap_details(parent_scap_id);
