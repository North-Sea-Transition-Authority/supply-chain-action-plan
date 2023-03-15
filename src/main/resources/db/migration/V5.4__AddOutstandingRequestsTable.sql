CREATE TABLE scap_update_requests (
    id UUID PRIMARY KEY
    , scap_detail_id INT NOT NULL
    , request_type VARCHAR(50)
    , created_timestamp TIMESTAMP
    , created_by_user_id INT NOT NULL
    , due_date TIMESTAMP NOT NULL
    , resolution_date TIMESTAMP
    , resolved_by_user_id INT
    , CONSTRAINT fk_scap_outstanding_scap_details_id FOREIGN KEY (scap_detail_id) REFERENCES scap_details(id)
);

ALTER TABLE case_events DROP COLUMN due_date;

CREATE INDEX idx_outstanding_scap_detail_id ON scap_update_requests(scap_detail_id);
