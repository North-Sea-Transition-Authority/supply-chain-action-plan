CREATE TABLE audit_revisions (
  rev                 SERIAL PRIMARY KEY
, created_timestamp   TIMESTAMP
, web_user_account_id INT
);

-- scap_details audit table
CREATE TABLE scap_details_aud (
  rev                   SERIAL
, revtype               NUMERIC
, id                    INT
, status                TEXT
, created_timestamp     TIMESTAMP
, submitted_timestamp   TIMESTAMP
, PRIMARY KEY (rev, id)
, FOREIGN KEY (rev) REFERENCES audit_revisions(rev)
);

CREATE INDEX idx_scap_details_aud_rev ON scap_details_aud(rev);

-- team_member_roles audit table

CREATE TABLE team_member_roles_aud (
  rev     SERIAL
, revtype NUMERIC
, uuid    UUID
, wua_id  INT
, team_id UUID
, role    TEXT
, PRIMARY KEY (rev, uuid)
, FOREIGN KEY (rev) REFERENCES audit_revisions(rev)
);

CREATE INDEX idx_team_member_roles_aud_rev ON team_member_roles_aud(rev);
