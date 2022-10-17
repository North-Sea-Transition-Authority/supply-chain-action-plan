CREATE TABLE scap_planned_tenders (
  id INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY
, scap_detail_id INT NOT NULL
, has_planned_tenders BOOLEAN
, created_timestamp TIMESTAMP NOT NULL
, CONSTRAINT fk_scap_planned_tenders_scap_details_id FOREIGN KEY (scap_detail_id) REFERENCES scap_details(id)
);

CREATE INDEX idx_scap_planned_tenders_scap_details_id ON scap_planned_tenders(scap_detail_id);

CREATE TABLE scap_planned_tender_details (
  id INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY
, scap_planned_tender_id INT NOT NULL
, scope_description TEXT
, estimated_value DECIMAL
, remuneration_model TEXT
, remuneration_model_name TEXT
, award_rationale TEXT
, created_timestamp TIMESTAMP NOT NULL
, CONSTRAINT fk_scap_planned_tender_details_scap_planned_tender_id
  FOREIGN KEY (scap_planned_tender_id) REFERENCES scap_planned_tenders(id)
);

CREATE INDEX idx_scap_planned_tender_details_scap_planned_tender_id ON scap_planned_tender_details(scap_planned_tender_id)
