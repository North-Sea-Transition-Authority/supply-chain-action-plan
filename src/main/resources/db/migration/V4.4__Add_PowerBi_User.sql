CREATE USER nstaPowerbiUser WITH PASSWORD '${powerbiAuthentication}';
GRANT SELECT ON ALL TABLES IN SCHEMA scap TO nstaPowerbiUser;