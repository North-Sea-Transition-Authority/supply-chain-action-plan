ALTER TABLE scaps ADD COLUMN reference TEXT;

UPDATE scaps s SET reference = 'MISSING_REFERENCE-' || s.scap_id WHERE s.reference IS NULL;

ALTER TABLE scaps ALTER COLUMN reference SET NOT NULL;

ALTER TABLE scaps ADD CONSTRAINT scap_reference_unique UNIQUE (reference);
