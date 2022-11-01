CREATE TABLE scap_project_details (
  id INT GENERATED ALWAYS AS IDENTITY
, scap_detail_id INT NOT NULL
, project_name TEXT
, project_cost_estimate DECIMAL
, estimated_value_local_content DECIMAL
, field_id INT
, field_name TEXT
, planned_execution_start_date DATE
, planned_completion_date DATE
, created_timestamp TIMESTAMP
, CONSTRAINT fk_scap_project_details_scap_detail_id
  FOREIGN KEY (scap_detail_id) REFERENCES scap_details(id)
);

CREATE INDEX idx_scap_project_details_scap_detail_id ON scap_project_details(scap_detail_id);

CREATE TABLE scap_project_detail_types (
  id INT GENERATED ALWAYS AS IDENTITY
, scap_project_detail_id INT NOT NULL
, project_type TEXT
, created_timestamp TIMESTAMP
);

CREATE INDEX idx_scap_project_detail_types_scap_project_detail_id ON scap_project_detail_types(scap_project_detail_id);
