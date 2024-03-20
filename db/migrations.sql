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
VALUES ('Fictional University in Tashkent', 'FUT', 2003, 'https://fictional-university.edu'),
('Imaginary University in Tashkent', 'IUT', 2006, 'https://imaginary-university.edu');

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

-- admins
INSERT INTO users (name, birthday, education, role, email, passwordHash)
VALUES ('Anwar', '2003-08-24', 'bachelor', 'admin', 'anwar-admin@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu');

-- students
INSERT INTO users (name, birthday, education, role, email, passwordHash)
VALUES
('Junki', '2003-08-24', 'bachelor', 'student', 'junki@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu'),
('Alvaro', '2003-08-24', NULL, 'student', 'alvaro@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu');

-- instructors
INSERT INTO users (name, birthday, education, role, email, passwordHash)
VALUES
('Jake', '1990-08-24', 'master', 'instructor', 'jake@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu'),
('John', '1985-08-24', 'phd', 'instructor', 'john@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu');

INSERT INTO instructors (id, university)
VALUES
(4, 1),
(5, 2);

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

INSERT INTO courses (title, description, difficulty, university, instructor)
VALUES
('Basic Python', 'Learn the basics of the Python programming language', 'beginner', 1, 4),
('OOP in Python', 'Learn object-oriented programming in Python', 'intermediate', 1, 4),
('Calculus 1', 'Learn fundamental single-variable differential and integral calculus', 'intermediate', 2, 5),
('Linear Algebra', 'Learn vectors, matrices, vector spaces, and linear equations', 'advanced', 2, 5);

CREATE TABLE IF NOT EXISTS enrollments (
	userId INT NOT NULL,
	courseId INT NOT NULL,
	enrolled TIMESTAMPTZ DEFAULT (now() at time zone('utc')),
	FOREIGN KEY (userId) REFERENCES users(id),
	FOREIGN KEY (courseId) REFERENCES courses(id)
);

INSERT INTO enrollments (userId, courseId)
VALUES
(2, 1),
(2, 2),
(2, 3),
(2, 4),
(3, 1),
(3, 2),
(3, 3),
(3, 4);

CREATE TABLE IF NOT EXISTS lessons (
	number INT NOT NULL,
	title TEXT NOT NULL,
	description TEXT NOT NULL,
	content TEXT NOT NULL,
	course INT NOT NULL,
	PRIMARY KEY(number, course)
);

INSERT INTO lessons (number, title, description, content, course)
VALUES
(1, 'Variables and IO', 'Learn about variables, datatypes, and basic input and output.', '**Markdown content**', 1),
(2, 'Conditionals and loops', 'Learn about how to use conditionals and how to iterate.', '**Markdown content**', 1),
(3, 'Functions', 'Learn about how to define your own functions.', '**Markdown content**', 1),
(1, 'Classes and Objects', 'Learn about classes and objects.', '**Markdown content**', 2),
(2, 'Constructors and Methods', 'Learn about how to define and instantiate your own classes.', '**Markdown content**', 2),
(3, 'Inheritance and Polymorphism', 'Learn about child classes, abstract classes, and interfaces.', '**Markdown content**', 2),
(1, 'Functions and Limits', 'Learn about functions and limits.', '**Markdown content**', 3),
(2, 'Differentiation', 'Learn about derivatives and differentiation rules.', '**Markdown content**', 3),
(3, 'Definite and indefinite integrals', 'Learn integrals and their applications.', '**Markdown content**', 3),
(1, 'Vectors and Vector Spaces', 'Learn about different vector spaces and subspaces.', '**Markdown content**', 4),
(2, 'Linear Transformations', 'Learn about linear transformations and matrix multiplication.', '**Markdown content**', 4),
(3, 'Systems of Linear Equations', 'Learn to solve systems of linear equations.', '**Markdown content**', 4);
