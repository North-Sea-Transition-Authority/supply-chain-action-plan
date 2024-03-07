CREATE TABLE project_fields (
  id                  UUID PRIMARY KEY
, project_details_id  INT NOT NULL
, field_id            INT NOT NULL
, created_timestamp   TIMESTAMP NOT NULL
, CONSTRAINT fk_project_fields_project_details_id FOREIGN KEY (project_details_id) REFERENCES project_details(id)
);

CREATE INDEX idx_project_fields_project_details_id ON project_fields(project_details_id);

INSERT INTO project_fields(id, project_details_id, field_id, created_timestamp)
  SELECT gen_random_uuid(), id, field_id, created_timestamp FROM project_details;

ALTER TABLE project_details DROP COLUMN field_id;
ALTER TABLE project_details DROP COLUMN field_name;
