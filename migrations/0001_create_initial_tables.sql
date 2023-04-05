CREATE TABLE "author"
(
	"id" SERIAL PRIMARY KEY,
	"name" VARCHAR(256) UNIQUE NOT NULL,
	"password" TEXT NOT NULL
);

CREATE TABLE "phrase"
(
	"id" SERIAL PRIMARY KEY,
	"text" TEXT NOT NULL,
	"author_id" INT NOT NULL,
	"approved" BOOL DEFAULT False,
	"parent_id" INT,
	CONSTRAINT fk_author FOREIGN KEY(author_id) REFERENCES author(id),
	CONSTRAINT fk_parent FOREIGN KEY(parent_id) REFERENCES phrase(id)
);
