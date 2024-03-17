DROP TABLE IF EXISTS lessons;
DROP TABLE IF EXISTS enrollments;
DROP TABLE IF EXISTS courses;
DROP TABLE IF EXISTS instructors;
DROP TABLE IF EXISTS users;
DROP TABLE IF EXISTS universities;

CREATE TABLE IF NOT EXISTS universities (
	id SERIAL PRIMARY KEY,
	name TEXT NOT NULL UNIQUE,
	abbreviation TEXT,
	year INT NOT NULL,
	url TEXT NOT NULL UNIQUE,
	joined TIMESTAMPTZ DEFAULT (now() at time zone('utc'))
);

INSERT INTO universities (name, abbreviation, year, url)
VALUES ('Fictional University', 'FU', 2003, 'https://fictional-university.edu');

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

CREATE TABLE IF NOT EXISTS instructors (
	id INT NOT NULL REFERENCES users
		ON DELETE CASCADE,
	university INT NOT NULL,
	FOREIGN KEY (id) REFERENCES users(id),
	FOREIGN KEY (university) REFERENCES universities(id)
);

INSERT INTO users (name, birthday, education, role, email, passwordHash)
VALUES ('Anwar', '2003-08-24', 'bachelor', 'admin', 'anwar-admin@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu');

INSERT INTO users (name, birthday, education, role, email, passwordHash)
VALUES ('John Doe', '1980-01-12', 'phd', 'instructor', 'johndoe@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu');

INSERT INTO users (name, birthday, education, role, email, passwordHash)
VALUES ('Junki', '2003-08-24', 'bachelor', 'student', 'junki@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu');

CREATE TABLE IF NOT EXISTS courses (
	id SERIAL PRIMARY KEY,
	title TEXT NOT NULL,
	description TEXT NOT NULL,
	difficulty TEXT NOT NULL,
	university INT NOT NULL,
	instructor INT NOT NULL,
	FOREIGN KEY (university) REFERENCES universities(id),
	FOREIGN KEY (instructor) REFERENCES users(id)
);

CREATE TABLE IF NOT EXISTS enrollments (
	userId INT NOT NULL,
	courseId INT NOT NULL,
	enrolled TIMESTAMPTZ DEFAULT (now() at time zone('utc')),
	FOREIGN KEY (userId) REFERENCES users(id),
	FOREIGN KEY (courseId) REFERENCES courses(id)
);

CREATE TABLE IF NOT EXISTS lessons (
	number INT NOT NULL,
	title TEXT NOT NULL,
	description TEXT NOT NULL,
	content TEXT NOT NULL,
	course INT NOT NULL,
	PRIMARY KEY(number, course)
);
