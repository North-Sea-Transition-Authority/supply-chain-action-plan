ALTER TABLE project_details ADD COLUMN has_facilities BOOLEAN;

CREATE TABLE project_facilities (
  id                            INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY
, project_details_id            INT NOT NULL
, facility_id                   INT NOT NULL
, created_timestamp             TIMESTAMP NOT NULL
, CONSTRAINT fk_project_facilities_project_details_id
  FOREIGN KEY (project_details_id) REFERENCES project_details(id)
);

CREATE INDEX idx_project_facilities_project_details_id ON  project_facilities(project_details_id);
