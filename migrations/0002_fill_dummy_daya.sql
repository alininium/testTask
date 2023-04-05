INSERT INTO author (name, password) VALUES ('admin', 'admin');
INSERT INTO author (name, password) VALUES ('test', 'test');

INSERT INTO phrase (text, author_id) VALUES ('message', 1);
INSERT INTO phrase (text, author_id, approved, parent_id) VALUES ('message2', 1, true, 1);
INSERT INTO phrase (text, author_id, parent_id) VALUES ('message3', 2, 1);

