CREATE TABLE scap_details (
  id INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY
, scap_id INT NOT NULL
, version_number INT NOT NULL
, tip_flag BOOLEAN NOT NULL
, status TEXT
, created_timestamp TIMESTAMP NOT NULL
, created_by_user_id INT NOT NULL
, CONSTRAINT fk_scap_details_scap_id FOREIGN KEY (scap_id) REFERENCES scap_overviews(scap_id)
);

CREATE INDEX idx_scap_details_scap_id ON scap_details(scap_id);
