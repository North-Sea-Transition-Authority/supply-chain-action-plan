-- Rename SCAP base table
ALTER TABLE scap_overviews RENAME TO scaps;

-- Rename planned_tenders table
ALTER TABLE scap_planned_tenders
RENAME TO planned_tenders;

ALTER INDEX idx_scap_planned_tenders_scap_details_id
RENAME TO idx_planned_tenders_scap_details_id;

ALTER TABLE planned_tenders
RENAME CONSTRAINT fk_scap_planned_tenders_scap_details_id
TO fk_planned_tenders_scap_details_id;

-- Rename planned_tender_activities table
ALTER TABLE scap_planned_tender_details
RENAME TO planned_tender_activities;

ALTER TABLE planned_tender_activities
RENAME COLUMN scap_planned_tender_id
TO planned_tender_id;

ALTER INDEX idx_scap_planned_tender_details_scap_planned_tender_id
RENAME TO idx_planned_tender_activities_planned_tender_id;

ALTER TABLE planned_tender_activities
RENAME CONSTRAINT fk_scap_planned_tender_details_scap_planned_tender_id
TO fk_planned_tender_details_planned_tender_id;

-- Rename project_details table
ALTER TABLE scap_project_details
RENAME TO project_details;

ALTER INDEX idx_scap_project_details_scap_detail_id
RENAME TO idx_project_details_scap_detail_id;

ALTER TABLE project_details
RENAME CONSTRAINT fk_scap_project_details_scap_detail_id
TO fk_project_details_scap_detail_id;

ALTER TABLE project_details
ADD PRIMARY KEY (id);

-- Rename project_detail_types table
ALTER TABLE scap_project_detail_types
RENAME TO project_detail_types;

ALTER INDEX idx_scap_project_detail_types_scap_project_detail_id
RENAME TO idx_project_detail_types_project_detail_id;

ALTER TABLE project_detail_types
RENAME COLUMN scap_project_detail_id
TO project_detail_id;

ALTER TABLE project_detail_types
ADD PRIMARY KEY (id);

ALTER TABLE project_detail_types
ADD CONSTRAINT fk_project_details_scap_detail_id
FOREIGN KEY (project_detail_id) REFERENCES project_details(id);
