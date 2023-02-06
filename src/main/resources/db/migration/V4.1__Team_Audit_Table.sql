CREATE TABLE teams_aud (
   rev     SERIAL,
   revtype NUMERIC,
   uuid      UUID,
   display_name   TEXT,
   organisation_group_id INTEGER,
   PRIMARY KEY (rev, uuid),
   FOREIGN KEY (rev) REFERENCES audit_revisions(rev)
);

CREATE INDEX idx_team_aud_rev ON teams_aud(rev);
