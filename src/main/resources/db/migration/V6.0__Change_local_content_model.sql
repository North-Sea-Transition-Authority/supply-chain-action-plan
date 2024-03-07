ALTER TABLE project_details
  ADD COLUMN aware_of_local_content_commitment BOOLEAN DEFAULT TRUE,
  ADD COLUMN expects_to_meet_local_content_commitment BOOLEAN DEFAULT FALSE;

ALTER TABLE project_details
  ALTER COLUMN aware_of_local_content_commitment DROP DEFAULT,
  ALTER COLUMN expects_to_meet_local_content_commitment DROP DEFAULT;

UPDATE project_details
  SET expects_to_meet_local_content_commitment = TRUE
  WHERE estimated_value_local_content * 2 > project_details.project_cost_estimate;

ALTER TABLE project_details DROP COLUMN estimated_value_local_content;

ALTER TABLE project_details ADD COLUMN miss_local_content_commitment_rationale TEXT;
