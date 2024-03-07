ALTER TABLE project_facilities DROP CONSTRAINT project_facilities_pkey;
ALTER TABLE project_facilities DROP COLUMN id;
ALTER TABLE project_facilities ADD COLUMN id UUID DEFAULT gen_random_uuid();
ALTER TABLE project_facilities ADD PRIMARY KEY (id);

ALTER TABLE project_detail_types DROP CONSTRAINT project_detail_types_pkey;
ALTER TABLE project_detail_types DROP COLUMN id;
ALTER TABLE project_detail_types ADD COLUMN id UUID DEFAULT gen_random_uuid();
ALTER TABLE project_detail_types ADD PRIMARY KEY (id);