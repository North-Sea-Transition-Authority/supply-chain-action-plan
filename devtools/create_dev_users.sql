-- Script used for testing, adds SCAP Test Accounts, with correct roles already setup:
-- https://confluence.fivium.co.uk/display/BESPOKE/SCAP+Test+Accounts

SET SEARCH_PATH = 'scap';

BEGIN TRANSACTION;

  -- Create regulator team

  INSERT INTO teams(uuid, type, display_name) VALUES ('00000000-0000-0000-0000-000000000000', 'REGULATOR', 'NSTA');

  INSERT INTO team_member_roles VALUES (gen_random_uuid(), 53235, '00000000-0000-0000-0000-000000000000', 'ACCESS_MANAGER'),
                                       (gen_random_uuid(), 53236, '00000000-0000-0000-0000-000000000000', 'SCAP_VIEWER'),
                                       (gen_random_uuid(), 53237, '00000000-0000-0000-0000-000000000000', 'SCAP_CASE_OFFICER'),
                                       (gen_random_uuid(), 53238, '00000000-0000-0000-0000-000000000000', 'ORGANISATION_ACCESS_MANAGER');

  -- Create industry team

  INSERT INTO teams VALUES ('48747735-1ff0-4b72-8649-d8a00c537012', 'INDUSTRY', 55, 'CENTRICA');

  INSERT INTO team_member_roles VALUES (gen_random_uuid(), 53232, '48747735-1ff0-4b72-8649-d8a00c537012', 'ACCESS_MANAGER'),
                                       (gen_random_uuid(), 53233, '48747735-1ff0-4b72-8649-d8a00c537012', 'SCAP_VIEWER'),
                                       (gen_random_uuid(), 53234, '48747735-1ff0-4b72-8649-d8a00c537012', 'SCAP_SUBMITTER');

COMMIT;
