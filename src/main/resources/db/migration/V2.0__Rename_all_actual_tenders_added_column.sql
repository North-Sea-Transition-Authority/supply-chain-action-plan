ALTER TABLE actual_tenders RENAME COLUMN all_actual_tenders_added TO has_more_actual_tenders;
ALTER TABLE actual_tenders ALTER COLUMN has_more_actual_tenders TYPE TEXT;
