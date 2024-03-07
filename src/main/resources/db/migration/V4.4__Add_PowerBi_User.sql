CREATE USER nstaPowerbiUser WITH PASSWORD '${powerbiAuthentication}';
GRANT USAGE ON SCHEMA scap TO nstapowerbiuser;
GRANT SELECT ON ALL TABLES IN SCHEMA scap TO nstaPowerbiUser;
