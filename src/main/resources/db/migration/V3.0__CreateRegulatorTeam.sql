/* Generate NSTA Regulator Team for Team Management */
DELETE FROM scap.teams WHERE type = 'REGULATOR';
INSERT INTO scap.teams (uuid, type, display_name, organisation_group_id) VALUES ('00000000-0000-0000-0000-000000000000', 'REGULATOR', 'NSTA', NULL);
