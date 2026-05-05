ALTER TABLE actual_tender_activities
    ADD COLUMN no_of_jobs_supporting_contract INT,
    ADD COLUMN no_of_new_jobs_created_for_contract INT,
    ADD COLUMN no_of_jobs_based_in_uk_for_contract INT;