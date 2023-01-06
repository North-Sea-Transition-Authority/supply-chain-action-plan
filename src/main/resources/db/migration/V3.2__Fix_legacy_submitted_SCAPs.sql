-- Legacy SCAPs from testing that didn't record when they were submitted should be returned to DRAFT status

UPDATE scap_details sd
SET status = 'DRAFT'
WHERE sd.submitted_timestamp IS NULL
      AND sd.status = 'SUBMITTED'
      AND sd.created_timestamp < TIMESTAMP '2023-01-06 15:32:00';
