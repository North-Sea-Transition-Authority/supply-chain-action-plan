CREATE USER datawarehouseuser WITH PASSWORD '${datawarehouseAuthentication}';
GRANT USAGE ON SCHEMA scap TO nstapowerbiuser;
GRANT SELECT ON ALL TABLES IN SCHEMA scap TO nstaPowerbiUser;

REVOKE USAGE ON SCHEMA scap FROM nstapowerbiuser;
REVOKE SELECT ON ALL TABLES IN SCHEMA scap FROM nstaPowerbiUser;
DROP USER nstaPowerbiUser;