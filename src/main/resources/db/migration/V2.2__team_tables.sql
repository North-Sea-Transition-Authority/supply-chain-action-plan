CREATE TABLE teams (
  uuid UUID PRIMARY KEY
, type VARCHAR NOT NULL
);

CREATE TABLE team_member_roles (
  uuid UUID PRIMARY KEY
, wua_id INT NOT NULL
, team_id UUID NOT NULL REFERENCES teams(uuid)
, role TEXT NOT NULL
);

CREATE INDEX idx_scap_t_team_member_roles ON team_member_roles(team_id);

INSERT INTO teams (uuid, type)
VALUES (gen_random_uuid(), 'REGULATOR');