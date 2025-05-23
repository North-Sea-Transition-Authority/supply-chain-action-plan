-- Script used for testing, adds SCAP Test Accounts, with correct roles already setup:
-- https://fivium.atlassian.net/wiki/spaces/BESPOKE/pages/1738766/SCAP+Test+Accounts

SET SEARCH_PATH = 'scap';

BEGIN TRANSACTION;

  -- Create regulator team

  INSERT INTO team_member_roles VALUES (gen_random_uuid(), 53235, '00000000-0000-0000-0000-000000000000', 'ACCESS_MANAGER'),
                                       (gen_random_uuid(), 53236, '00000000-0000-0000-0000-000000000000', 'SCAP_VIEWER'),
                                       (gen_random_uuid(), 53237, '00000000-0000-0000-0000-000000000000', 'SCAP_CASE_OFFICER'),
                                       (gen_random_uuid(), 53238, '00000000-0000-0000-0000-000000000000', 'ORGANISATION_ACCESS_MANAGER');

  -- Create industry team 1

  INSERT INTO teams VALUES ('48747735-1ff0-4b72-8649-d8a00c537012', 'INDUSTRY', 55, 'CENTRICA');

  INSERT INTO team_member_roles VALUES (gen_random_uuid(), 53232, '48747735-1ff0-4b72-8649-d8a00c537012', 'ACCESS_MANAGER'),
                                       (gen_random_uuid(), 53233, '48747735-1ff0-4b72-8649-d8a00c537012', 'SCAP_VIEWER'),
                                       (gen_random_uuid(), 53234, '48747735-1ff0-4b72-8649-d8a00c537012', 'SCAP_SUBMITTER');

  -- Create industry team 2 (user is 'pporter')

  INSERT INTO teams VALUES ('a038e5dc-e5ac-48c1-91e5-761f0f8bf706', 'INDUSTRY', 116, 'ROYAL DUTCH SHELL');

  INSERT INTO team_member_roles VALUES (gen_random_uuid(), 16178, 'a038e5dc-e5ac-48c1-91e5-761f0f8bf706', 'ACCESS_MANAGER');
  INSERT INTO team_member_roles VALUES (gen_random_uuid(), 16178, 'a038e5dc-e5ac-48c1-91e5-761f0f8bf706', 'SCAP_VIEWER');
  INSERT INTO team_member_roles VALUES (gen_random_uuid(), 16178, 'a038e5dc-e5ac-48c1-91e5-761f0f8bf706', 'SCAP_SUBMITTER');

COMMIT;
