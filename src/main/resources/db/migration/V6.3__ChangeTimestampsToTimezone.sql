ALTER TABLE scaps ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE scaps SET created_timestamp_timezone = created_timestamp;
ALTER TABLE scaps DROP COLUMN created_timestamp;
ALTER TABLE scaps RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE scap_details ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE scap_details SET created_timestamp_timezone = created_timestamp;
ALTER TABLE scap_details DROP COLUMN created_timestamp;
ALTER TABLE scap_details RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE planned_tenders ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE planned_tenders SET created_timestamp_timezone = created_timestamp;
ALTER TABLE planned_tenders DROP COLUMN created_timestamp;
ALTER TABLE planned_tenders RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE planned_tender_activities ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE planned_tender_activities SET created_timestamp_timezone = created_timestamp;
ALTER TABLE planned_tender_activities DROP COLUMN created_timestamp;
ALTER TABLE planned_tender_activities RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE project_details ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE project_details SET created_timestamp_timezone = created_timestamp;
ALTER TABLE project_details DROP COLUMN created_timestamp;
ALTER TABLE project_details RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE project_detail_types ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE project_detail_types SET created_timestamp_timezone = created_timestamp;
ALTER TABLE project_detail_types DROP COLUMN created_timestamp;
ALTER TABLE project_detail_types RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE actual_tenders ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE actual_tenders SET created_timestamp_timezone = created_timestamp;
ALTER TABLE actual_tenders DROP COLUMN created_timestamp;
ALTER TABLE actual_tenders RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE actual_tender_activities ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE actual_tender_activities SET created_timestamp_timezone = created_timestamp;
ALTER TABLE actual_tender_activities DROP COLUMN created_timestamp;
ALTER TABLE actual_tender_activities RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE invitation_to_tender_participants ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE invitation_to_tender_participants SET created_timestamp_timezone = created_timestamp;
ALTER TABLE invitation_to_tender_participants DROP COLUMN created_timestamp;
ALTER TABLE invitation_to_tender_participants RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE awarded_contracts ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE awarded_contracts SET created_timestamp_timezone = created_timestamp;
ALTER TABLE awarded_contracts DROP COLUMN created_timestamp;
ALTER TABLE awarded_contracts RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE contracting_performance_overviews ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE contracting_performance_overviews SET created_timestamp_timezone = created_timestamp;
ALTER TABLE contracting_performance_overviews DROP COLUMN created_timestamp;
ALTER TABLE contracting_performance_overviews RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE activity_contracting_performance ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE activity_contracting_performance SET created_timestamp_timezone = created_timestamp;
ALTER TABLE activity_contracting_performance DROP COLUMN created_timestamp;
ALTER TABLE activity_contracting_performance RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE project_performances ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE project_performances SET created_timestamp_timezone = created_timestamp;
ALTER TABLE project_performances DROP COLUMN created_timestamp;
ALTER TABLE project_performances RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE project_facilities ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE project_facilities SET created_timestamp_timezone = created_timestamp;
ALTER TABLE project_facilities DROP COLUMN created_timestamp;
ALTER TABLE project_facilities RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE project_fields ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE project_fields SET created_timestamp_timezone = created_timestamp;
ALTER TABLE project_fields DROP COLUMN created_timestamp;
ALTER TABLE project_fields RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE audit_revisions ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE audit_revisions SET created_timestamp_timezone = created_timestamp;
ALTER TABLE audit_revisions DROP COLUMN created_timestamp;
ALTER TABLE audit_revisions RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE scap_details_aud ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE scap_details_aud SET created_timestamp_timezone = created_timestamp;
ALTER TABLE scap_details_aud DROP COLUMN created_timestamp;
ALTER TABLE scap_details_aud RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE scap_update_requests ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE scap_update_requests SET created_timestamp_timezone = created_timestamp;
ALTER TABLE scap_update_requests DROP COLUMN created_timestamp;
ALTER TABLE scap_update_requests RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE pathfinder_projects_overviews ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE pathfinder_projects_overviews SET created_timestamp_timezone = created_timestamp;
ALTER TABLE pathfinder_projects_overviews DROP COLUMN created_timestamp;
ALTER TABLE pathfinder_projects_overviews RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE pathfinder_projects ADD COLUMN created_timestamp_timezone TIMESTAMPTZ;
UPDATE pathfinder_projects SET created_timestamp_timezone = created_timestamp;
ALTER TABLE pathfinder_projects DROP COLUMN created_timestamp;
ALTER TABLE pathfinder_projects RENAME COLUMN created_timestamp_timezone TO created_timestamp;

ALTER TABLE scap_details ADD COLUMN approved_timestamp_timezone TIMESTAMPTZ;
UPDATE scap_details SET approved_timestamp_timezone = approved_timestamp;
ALTER TABLE scap_details DROP COLUMN approved_timestamp;
ALTER TABLE scap_details RENAME COLUMN approved_timestamp_timezone TO approved_timestamp;

ALTER TABLE outbound_email ADD COLUMN created_date_time_timezone TIMESTAMPTZ;
UPDATE outbound_email SET created_date_time_timezone = created_date_time;
ALTER TABLE outbound_email DROP COLUMN created_date_time;
ALTER TABLE outbound_email RENAME COLUMN created_date_time_timezone TO created_date_time;

ALTER TABLE outbound_email ADD COLUMN created_date_time_timezone TIMESTAMPTZ;
UPDATE outbound_email SET created_date_time_timezone = created_date_time;
ALTER TABLE outbound_email DROP COLUMN created_date_time;
ALTER TABLE outbound_email RENAME COLUMN created_date_time_timezone TO created_date_time;

ALTER TABLE scap_update_requests ADD COLUMN due_date_timezone TIMESTAMPTZ;
UPDATE scap_update_requests SET due_date_timezone = due_date;
ALTER TABLE scap_update_requests DROP COLUMN due_date;
ALTER TABLE scap_update_requests RENAME COLUMN due_date_timezone TO due_date;

ALTER TABLE case_events ADD COLUMN event_time_timezone TIMESTAMPTZ;
UPDATE case_events SET event_time_timezone = event_time;
ALTER TABLE case_events DROP COLUMN event_time;
ALTER TABLE case_events RENAME COLUMN event_time_timezone TO event_time;

ALTER TABLE scap_update_requests ADD COLUMN resolution_date_timezone TIMESTAMPTZ;
UPDATE scap_update_requests SET resolution_date_timezone = resolution_date;
ALTER TABLE scap_update_requests DROP COLUMN resolution_date;
ALTER TABLE scap_update_requests RENAME COLUMN resolution_date_timezone TO resolution_date;

ALTER TABLE scap_details ADD COLUMN submitted_timestamp_timezone TIMESTAMPTZ;
UPDATE scap_details SET submitted_timestamp_timezone = submitted_timestamp;
ALTER TABLE scap_details DROP COLUMN submitted_timestamp;
ALTER TABLE scap_details RENAME COLUMN submitted_timestamp_timezone TO submitted_timestamp;

ALTER TABLE scap_details_aud ADD COLUMN submitted_timestamp_timezone TIMESTAMPTZ;
UPDATE scap_details_aud SET submitted_timestamp_timezone = submitted_timestamp;
ALTER TABLE scap_details_aud DROP COLUMN submitted_timestamp;
ALTER TABLE scap_details_aud RENAME COLUMN submitted_timestamp_timezone TO submitted_timestamp;

ALTER TABLE outbound_email ADD COLUMN updated_date_time_timezone TIMESTAMPTZ;
UPDATE outbound_email SET updated_date_time_timezone = updated_date_time;
ALTER TABLE outbound_email DROP COLUMN updated_date_time;
ALTER TABLE outbound_email RENAME COLUMN updated_date_time_timezone TO updated_date_time;

ALTER TABLE outbound_sms ADD COLUMN updated_date_time_timezone TIMESTAMPTZ;
UPDATE outbound_sms SET updated_date_time_timezone = updated_date_time;
ALTER TABLE outbound_sms DROP COLUMN updated_date_time;
ALTER TABLE outbound_sms RENAME COLUMN updated_date_time_timezone TO updated_date_time;

ALTER TABLE outbound_sms ADD COLUMN created_date_time_timezone TIMESTAMPTZ;
UPDATE outbound_sms SET created_date_time_timezone = created_date_time;
ALTER TABLE outbound_sms DROP COLUMN created_date_time;
ALTER TABLE outbound_sms RENAME COLUMN created_date_time_timezone TO created_date_time;

ALTER TABLE uploaded_files ADD COLUMN uploaded_time_stamp_timezone TIMESTAMPTZ;
UPDATE uploaded_files SET uploaded_time_stamp_timezone = uploaded_time_stamp;
ALTER TABLE uploaded_files DROP COLUMN uploaded_time_stamp;
ALTER TABLE uploaded_files RENAME COLUMN uploaded_time_stamp_timezone TO uploaded_time_stamp;
