CREATE TABLE pathfinder_projects_overviews (
  id UUID PRIMARY KEY
, scap_detail_id INT NOT NULL
, has_related_pathfinder_projects BOOLEAN
, no_pathfinder_projects_rationale TEXT
, created_timestamp TIMESTAMP NOT NULL
, CONSTRAINT fk_pathfinder_project_overviews_scap_detail_id
  FOREIGN KEY (scap_detail_id) REFERENCES scap_details(id)
);

CREATE INDEX idx_pathfinder_project_overviews_scap_detail_id ON pathfinder_projects_overviews(scap_detail_id);

CREATE TABLE pathfinder_projects (
  id UUID PRIMARY KEY
, pathfinder_projects_overview_id UUID NOT NULL
, pathfinder_project_id INT NOT NULL
, pathfinder_project_name TEXT
, created_timestamp TIMESTAMP NOT NULL
, CONSTRAINT fk_pathfinder_projects_pathfinder_projects_overview_id
  FOREIGN KEY (pathfinder_projects_overview_id) REFERENCES pathfinder_projects_overviews(id)
);

CREATE INDEX idx_pathfinder_projects_pathfinder_projects_overview_id ON pathfinder_projects(pathfinder_projects_overview_id);
