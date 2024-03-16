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

INSERT INTO users (name, birthday, education, role, email, passwordHash)
VALUES ('Anwar', '2003-08-24', 'bachelor', 'admin', 'anwar-admin@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu')

CREATE TABLE IF NOT EXISTS universities (
	id SERIAL PRIMARY KEY,
	name TEXT NOT NULL UNIQUE,
	abbreviation TEXT,
	year INT NOT NULL,
	joined TIMESTAMPTZ DEFAULT (now() at time zone('utc'))
);

DROP TABLE IF EXISTS courses;

CREATE TABLE IF NOT EXISTS courses (
	id SERIAL PRIMARY KEY,
	title TEXT NOT NULL,
	description TEXT NOT NULL,
	difficulty TEXT NOT NULL,
	university INT NOT NULL,
	FOREIGN KEY (university) REFERENCES universities(id)
);

CREATE TABLE IF NOT EXISTS enrollments (
	userId INT NOT NULL,
	courseId INT NOT NULL,
	enrolled TIMESTAMPTZ DEFAULT (now() at time zone('utc')),
	FOREIGN KEY (userId) REFERENCES users(id),
	FOREIGN KEY (courseId) REFERENCES courses(id)
);
