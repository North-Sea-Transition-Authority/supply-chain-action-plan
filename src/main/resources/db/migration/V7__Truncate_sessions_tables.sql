-- The Spring Boot 2.x to 3.x upgrade requires persisted sessions to be cleared as the objects are not compatible between versions.
-- This will cause users to get logged out, however their Fox session will still be active so they won't need to re-enter
-- their user/password, they should immediately be redirected back to the service.
TRUNCATE TABLE spring_session_attributes, spring_session;
