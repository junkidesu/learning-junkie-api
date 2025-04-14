DROP TABLE IF EXISTS completions;
DROP TABLE IF EXISTS solutions;
DROP TABLE IF EXISTS quizzes;
DROP TABLE IF EXISTS essays;
DROP TABLE IF EXISTS questions;
DROP TABLE IF EXISTS exercises;
DROP TABLE IF EXISTS lessons;
DROP TABLE IF EXISTS enrollments;
DROP TABLE IF EXISTS courses;
DROP TABLE IF EXISTS users;
DROP TABLE IF EXISTS universities;

CREATE TABLE IF NOT EXISTS universities (
	id SERIAL PRIMARY KEY,
	name TEXT NOT NULL UNIQUE,
	abbreviation TEXT,
	year INT NOT NULL,
	url TEXT NOT NULL UNIQUE,
	logo TEXT,
	joined TIMESTAMPTZ DEFAULT (now() at time zone('utc'))
);

INSERT INTO universities (name, abbreviation, year, url)
VALUES ('Fictional University in Tashkent', 'FUT', 2003, 'https://fictional-university.edu'),
('Imaginary University in Tashkent', NULL, 2006, 'https://imaginary-university.edu');

CREATE TABLE IF NOT EXISTS users (
	id SERIAL PRIMARY KEY,
	joined TIMESTAMPTZ DEFAULT (now() at time zone('utc')),
	name TEXT NOT NULL,
	birthday DATE,
	education TEXT,
	role TEXT NOT NULL,
	email TEXT NOT NULL UNIQUE,
	passwordHash TEXT NOT NULL,
	avatar TEXT,
	university__id INT,
	FOREIGN KEY (university) REFERENCES universities(id) ON DELETE SET NULL
);

-- admins
INSERT INTO users (name, birthday, education, role, email, passwordHash)
VALUES ('Admin', '2003-08-24', 'bachelor', 'admin', 'admin@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu');

-- students
INSERT INTO users (name, birthday, education, role, email, passwordHash, university)
VALUES
('Junki', '2003-08-24', 'bachelor', 'student', 'junki@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu', NULL),
('Alvaro', '2003-08-24', NULL, 'student', 'alvaro@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu', NULL);

-- instructors
INSERT INTO users (name, birthday, education, role, email, passwordHash, university)
VALUES
('Jake Chen', '1990-08-24', 'master', 'instructor', 'jake@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu', 1),
('John Doe', '1985-08-24', 'phd', 'instructor', 'john@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu', 2);

CREATE TABLE IF NOT EXISTS courses (
	id SERIAL PRIMARY KEY,
	title TEXT NOT NULL,
	description TEXT NOT NULL,
	difficulty TEXT NOT NULL,
	banner TEXT,
	university__id INT NOT NULL,
	instructor__id INT NOT NULL, 
	FOREIGN KEY (university__id) REFERENCES universities(id) ON DELETE CASCADE,
	FOREIGN KEY (instructor__id) REFERENCES users(id) ON DELETE CASCADE
);

INSERT INTO courses (title, description, difficulty, university, instructor)
VALUES
('Basic Python',
'Python is one of the most popular programming languages today. It is used in a multitude of areas, from 
web development to artificial intelligence. This course intends to teach you the key things you need to know 
to start writing real Python programs. By the end of this course, you will be able to write simple programs 
with text user interfaces.',
'beginner',
1,
4),
('OOP in Python', 'Learn object-oriented programming in Python', 'intermediate', 1, 4),
('Calculus 1', 'Learn fundamental single-variable differential and integral calculus', 'intermediate', 2, 5),
('Linear Algebra', 'Learn vectors, matrices, vector spaces, and linear equations', 'advanced', 2, 5);

CREATE TABLE IF NOT EXISTS enrollments (
	userId INT NOT NULL,
	courseId INT NOT NULL,
	enrolled TIMESTAMPTZ DEFAULT (now() at time zone('utc')),
	FOREIGN KEY (userId) REFERENCES users(id) ON DELETE CASCADE,
	FOREIGN KEY (courseId) REFERENCES courses(id) ON DELETE CASCADE,
	PRIMARY KEY (userId, courseId)
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
	PRIMARY KEY(number, course),
	FOREIGN KEY (course) REFERENCES courses(id) ON DELETE CASCADE
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

CREATE TABLE IF NOT EXISTS exercises (
	id SERIAL PRIMARY KEY,
	title TEXT,
	grade INT NOT NULL DEFAULT 1,
	required BOOLEAN NOT NULL DEFAULT TRUE,
	lesson INT NOT NULL,
	course INT NOT NULL,
	FOREIGN KEY (lesson, course) REFERENCES lessons(number, course) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS questions (
	id INT NOT NULL PRIMARY KEY,
	question TEXT NOT NULL,
	answer TEXT NOT NULL,
	FOREIGN KEY (id) REFERENCES exercises(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS essays (
	id INT NOT NULL PRIMARY KEY,
	task TEXT NOT NULL,
	model TEXT NOT NULL,
	FOREIGN KEY (id) REFERENCES exercises(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS quizzes (
	id INT NOT NULL PRIMARY KEY,
	question TEXT NOT NULL,
	optionA TEXT NOT NULL,
	optionB TEXT NOT NULL,
	optionC TEXT NOT NULL,
	optionD TEXT NOT NULL,
	correct TEXT NOT NULL,
	FOREIGN KEY (id) REFERENCES exercises(id) ON DELETE CASCADE
);

-- question
INSERT INTO exercises (grade, title, lesson, course)
VALUES (1, 'Your first programming exercise', 1, 1);

INSERT INTO questions (id, question, answer)
VALUES (1, 'What data type is the value "Hello world?"', 'string');

-- essays
INSERT INTO exercises (grade, title, lesson, course)
VALUES (3, 'A bit of history', 1, 1);

INSERT INTO essays (id, task, model)
VALUES (2, 'Write an essay on the history of the Python programming language.', 'This is a model answer to the essay.');

-- quiz
INSERT INTO exercises (grade, title, lesson, course)
VALUES (1, 'Asking users', 1, 1);

INSERT INTO quizzes (id, question, optionA, optionB, optionC, optionD, correct)
VALUES (3, 'We can get user input with ...', 'print', 'def', 'input', 'while', 'c');

CREATE TABLE IF NOT EXISTS solutions (
	userId INT NOT NULL,
	exerciseId INT NOT NULL,
	grade INT, 
	time TIMESTAMPTZ DEFAULT (now() at time zone('utc')),
	FOREIGN KEY(userId) REFERENCES users(id) ON DELETE CASCADE,
	FOREIGN KEY(exerciseId) REFERENCES exercises(id) ON DELETE CASCADE,
	PRIMARY KEY (userId, exerciseId)
);

CREATE TABLE IF NOT EXISTS completions (
	courseId INT NOT NULL,
	userId INT NOT NULL,
	time TIMESTAMPTZ DEFAULT (now() at time zone('utc')),
	FOREIGN KEY(courseId) REFERENCES courses(id) ON DELETE CASCADE,
	FOREIGN KEY(userId) REFERENCES users(id) ON DELETE CASCADE,
	PRIMARY KEY (userId, courseId)
);
