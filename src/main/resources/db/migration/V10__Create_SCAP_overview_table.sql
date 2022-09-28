CREATE TABLE scap_overviews (
  scap_id INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY
, operator_id INT NOT NULL
, created_timestamp TIMESTAMP NOT NULL
)
