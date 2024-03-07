ALTER TABLE case_events ADD file_id UUID;

ALTER TABLE case_events ADD CONSTRAINT case_events_uploaded_files__fk FOREIGN KEY (file_id) REFERENCES uploaded_files;
