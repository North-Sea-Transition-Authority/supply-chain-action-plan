CREATE TABLE contracting_performance_overviews (
  id                            INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY
, scap_detail_id                INT NOT NULL
, has_contracting_performance   BOOLEAN
, created_timestamp             TIMESTAMP
, CONSTRAINT fk_contracting_performance_overviews_scap_detail_id
  FOREIGN KEY (scap_detail_id) REFERENCES scap_details(id)
);

CREATE INDEX idx_contracting_performance_overviews_scap_detail_id ON contracting_performance_overviews(scap_detail_id);

CREATE TABLE activity_contracting_performance (
  id                                    INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY
, contracting_performance_overview_id   INT NOT NULL
, actual_tender_activity_id             INT
, outturn_cost                          DECIMAL
, outturn_rationale                     TEXT
, created_timestamp             TIMESTAMP
, CONSTRAINT fk_activity_contracting_performance_performance_overview_id
  FOREIGN KEY (contracting_performance_overview_id) REFERENCES contracting_performance_overviews(id)
, CONSTRAINT fk_activity_contracting_performance_actual_tender_activity_id
  FOREIGN KEY (actual_tender_activity_id) REFERENCES actual_tender_activities(id)
);

CREATE INDEX idx_activity_contracting_performance_performance_overview_id
  ON activity_contracting_performance(contracting_performance_overview_id);
CREATE INDEX idx_activity_contracting_performance_actual_tender_activity_id
  ON activity_contracting_performance(actual_tender_activity_id);
