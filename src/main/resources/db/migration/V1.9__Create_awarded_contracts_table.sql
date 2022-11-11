CREATE TABLE awarded_contracts (
  id                            INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY
, actual_tender_activity_id     INT NOT NULL
, preferred_bidder_ittp_id      INT
, award_value                   DECIMAL
, award_rationale               TEXT
, preferred_bidder_location     INT
, created_timestamp             TIMESTAMP NOT NULL
, CONSTRAINT fk_awarded_contracts_actual_tender_activity_id
  FOREIGN KEY (actual_tender_activity_id) REFERENCES actual_tender_activities(id)
, CONSTRAINT fk_awarded_contracts_preferred_bidder_ittp_id
  FOREIGN KEY (preferred_bidder_ittp_id) REFERENCES invitation_to_tender_participants(id)
);

CREATE INDEX idx_awarded_contracts_actual_tender_activity_id ON awarded_contracts(actual_tender_activity_id);
CREATE INDEX idx_awarded_contracts_preferred_bidder_ittp_id ON awarded_contracts(preferred_bidder_ittp_id);
