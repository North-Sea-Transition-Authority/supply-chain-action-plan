CREATE TABLE project_performances (
  id                    INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY
, scap_detail_id        INT NOT NULL
, is_project_completed  BOOLEAN
, start_date            DATE
, completion_date       DATE
, outturn_cost          DECIMAL
, created_timestamp     TIMESTAMP
, CONSTRAINT fk_project_performances_scap_detail_id
  FOREIGN KEY (scap_detail_id) REFERENCES scap_details(id)
);

CREATE INDEX idx_project_performances_scap_detail_id ON project_performances(scap_detail_id);
