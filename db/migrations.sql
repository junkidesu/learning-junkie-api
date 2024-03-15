CREATE TABLE IF NOT EXISTS users (
	id SERIAL PRIMARY KEY,
	joined TIMESTAMPTZ DEFAULT (now() at time zone('utc')),
	name TEXT,
	birthday DATE,
	education TEXT,
	role TEXT NOT NULL,
	email TEXT NOT NULL UNIQUE,
	passwordHash TEXT NOT NULL
);

DROP TABLE IF EXISTS universities;

CREATE TABLE IF NOT EXISTS universities (
	id SERIAL PRIMARY KEY,
	name TEXT NOT NULL UNIQUE,
	abbreviation TEXT,
	year INT NOT NULL,
	joined TIMESTAMPTZ DEFAULT (now() at time zone('utc'))
);
