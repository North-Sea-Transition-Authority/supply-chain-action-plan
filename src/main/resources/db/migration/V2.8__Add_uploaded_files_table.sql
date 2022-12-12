CREATE TABLE uploaded_files (
  id                    UUID PRIMARY KEY
, s3_key                TEXT NOT NULL
, bucket_name           TEXT NOT NULL
, virtual_folder        TEXT
, filename              TEXT
, file_content_type     TEXT
, file_size_bytes       NUMERIC
, uploaded_time_stamp   TIMESTAMP NOT NULL
, description           TEXT
);

CREATE TABLE supporting_documents (
  id                        UUID PRIMARY KEY
, scap_detail_id            INT NOT NULL
, supporting_document_type  TEXT NOT NULL
, uploaded_file_id          UUID NOT NULL
, CONSTRAINT fk_supporting_documents_scap_detail_id
  FOREIGN KEY (scap_detail_id) REFERENCES scap_details (id)
, CONSTRAINT fk_supporting_documents_uploaded_file_id
  FOREIGN KEY (uploaded_file_id) REFERENCES  uploaded_files(id)
);

CREATE INDEX idx_supporting_documents_scap_detail_id ON supporting_documents(scap_detail_id);
CREATE INDEX idx_supporting_documents_uploaded_file_id ON supporting_documents(uploaded_file_id);
