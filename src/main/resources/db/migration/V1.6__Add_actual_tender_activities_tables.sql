CREATE TABLE actual_tenders (
  id                        INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY
, scap_detail_id            INT NOT NULL
, has_actual_tenders        BOOLEAN
, all_actual_tenders_added  BOOLEAN
, created_timestamp         TIMESTAMP NOT NULL
, CONSTRAINT fk_actual_tenders_scap_details_id FOREIGN KEY (scap_detail_id) REFERENCES scap_details(id)
);

CREATE INDEX idx_actual_tenders_scap_details_id ON actual_tenders(scap_detail_id);

CREATE TABLE actual_tender_activities (
  id                        INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY
, actual_tender_id          INT NOT NULL
, scope_title               TEXT
, scope_description         TEXT
, remuneration_model        TEXT
, remuneration_model_name   TEXT
, contract_stage            TEXT
, created_timestamp         TIMESTAMP NOT NULL
, CONSTRAINT fk_actual_tender_details_actual_tender_id
  FOREIGN KEY (actual_tender_id) REFERENCES actual_tenders(id)
);

CREATE INDEX idx_actual_tender_activities_actual_tender_id ON actual_tender_activities(actual_tender_id);

CREATE TABLE invitation_to_tender_participants (
  id                        INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY
, actual_tender_activity_id INT NOT NULL
, company_name              TEXT
, created_timestamp         TIMESTAMP NOT NULL
, CONSTRAINT fk_invitation_to_tender_participants_actual_tender_activity_id
  FOREIGN KEY (actual_tender_activity_id) REFERENCES actual_tender_activities(id)
);

CREATE INDEX idx_invitation_to_tender_participants_actual_tender_detail_id ON invitation_to_tender_participants(actual_tender_activity_id);
