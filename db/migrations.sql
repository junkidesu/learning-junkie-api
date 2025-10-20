DROP TABLE IF EXISTS submissions;
DROP TABLE IF EXISTS exercises;
DROP TABLE IF EXISTS lessons;
DROP TABLE IF EXISTS chapters;
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
VALUES ('Fictional University in Tashkent', 'FUT', 2003, 'https://fut.ac.uz'),
('Imaginary University in Tashkent', NULL, 2006, 'https://iu.ac.uz');

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
	FOREIGN KEY (university__id) REFERENCES universities(id) ON DELETE SET NULL
);

-- admins
INSERT INTO users (name, birthday, education, role, email, passwordHash)
VALUES ('Admin', '2003-08-24', 'Bachelor', 'Admin', 'admin@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu');

-- university representative 
INSERT INTO users (name, education, role, email, passwordHash, university__id)
VALUES
('FUT Representative', 'Master', 'UniversityRep', 'fut_rep1@fut.ac.uz', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu', 1),
('IU Representative', 'Master', 'UniversityRep', 'iu_rep1@iu.ac.uz', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu', 2);

-- instructors
INSERT INTO users (name, birthday, education, role, email, passwordHash, university__id)
VALUES
('Jake Chen', '1990-08-24', 'Master', 'Instructor', 'jake@iu.ac.uz', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu', 1),
('John Doe', '1985-08-24', 'PhD', 'Instructor', 'john@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu', 2);

-- students
INSERT INTO users (name, birthday, education, role, email, passwordHash, university__id)
VALUES
('Junki', '2003-08-24', 'Bachelor', 'Student', 'junki@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu', NULL),
('Alvaro', '2003-08-24', NULL, 'Student', 'alvaro@example.com', '$2b$10$2dsWB4pJedMef6Iuv4J64OyKYn85z/CHYzrWJ0iGouv2e3NMKWADu', NULL);

CREATE TABLE IF NOT EXISTS courses (
	id SERIAL PRIMARY KEY,
	title TEXT NOT NULL,
	description TEXT NOT NULL,
	difficulty TEXT NOT NULL,
	banner TEXT,
	university__id INT NOT NULL,
	instructor__id INT NOT NULL, 
	completion_requirements JSONB NOT NULL,
	FOREIGN KEY (university__id) REFERENCES universities(id) ON DELETE CASCADE,
	FOREIGN KEY (instructor__id) REFERENCES users(id) ON DELETE CASCADE
);

INSERT INTO courses (title, description, difficulty, university__id, instructor__id, completion_requirements)
VALUES
('Basic Python',
'Python is one of the most popular programming languages today. It is used in a multitude of areas, from 
web development to artificial intelligence. This course intends to teach you the key things you need to know 
to start writing real Python programs. By the end of this course, you will be able to write simple programs 
with text user interfaces.',
'Beginner',
1,
4,
'{ "exercisePercentage": 85, "finalProject": true }'),
('OOP in Python', 'Learn object-oriented programming in Python', 'Intermediate', 1, 4, '{ "exercisePercentage": 85, "finalProject": true }'
),
('Calculus 1', 'Learn fundamental single-variable differential and integral calculus', 'Intermediate', 2, 5, '{ "exercisePercentage": 85, "finalProject": true }'),
('Linear Algebra', 'Learn vectors, matrices, vector spaces, and linear equations', 'Advanced', 2, 5, '{ "exercisePercentage": 85, "finalProject": true }');

CREATE TABLE IF NOT EXISTS enrollments (
	user__id INT NOT NULL,
	course__id INT NOT NULL,
	time TIMESTAMPTZ DEFAULT (now() at time zone('utc')),
	FOREIGN KEY (user__id) REFERENCES users(id) ON DELETE CASCADE,
	FOREIGN KEY (course__id) REFERENCES courses(id) ON DELETE CASCADE,
	PRIMARY KEY (user__id, course__id)
);

INSERT INTO enrollments (user__id, course__id)
VALUES
(6, 1),
(6, 2),
(6, 3),
(6, 4),
(7, 1),
(7, 2),
(7, 3),
(7, 4);

CREATE TABLE IF NOT EXISTS chapters (
	chapter_number INT NOT NULL,
	course__id INT NOT NULL,
	title TEXT NOT NULL,
	description TEXT NOT NULL,
	banner TEXT,
	FOREIGN KEY (course__id) REFERENCES courses (id) ON DELETE CASCADE,
	PRIMARY KEY (course__id, chapter_number)
);

INSERT INTO chapters (course__id, chapter_number, title, description)
VALUES  
(1, 1, 'Basics of I/O', 'Learn the very basics of Python: printing and getting input'),
(1, 2, 'Control Structures', 'Conditionals and iterations, learn how to control your program'),
(1, 3, 'OOP', 'Learn about classes, objects, and the four pillars of OOP');

CREATE TABLE IF NOT EXISTS lessons (
	id SERIAL PRIMARY KEY,
	lesson_number INT NOT NULL,
	chapter__course__id INT NOT NULL,
	chapter__chapter_number INT NOT NULL,
	title TEXT NOT NULL,
	description TEXT NOT NULL,
	components JSONB NOT NULL,
	FOREIGN KEY(chapter__course__id, chapter__chapter_number) 
	REFERENCES chapters(chapter_number, course__id)
);

INSERT INTO lessons (lesson_number, chapter__course__id, chapter__chapter_number, title, description, components)
VALUES 
(1, 1, 1, 'Hello World: the very basics', 'Hello world is the first program that a programmer ever writes', '[{ "tag": "Markdown", "content": "This is some test content" }]');

CREATE TABLE IF NOT EXISTS exercises (
	id SERIAL PRIMARY KEY, 
	title TEXT NOT NULL, 
	max_grade INT NOT NULL,
	description TEXT NOT NULL,
	content JSONB NOT NULL,
	lesson__id INT NOT NULL,
	FOREIGN KEY (lesson__id) REFERENCES lessons(id)
);

INSERT INTO exercises (lesson__id, title, max_grade, description, content)
VALUES
(1, 'Example type answer', 2, 'This is example type answer exercise',
'{ "tag": "TypeAnswer", "question": "Example question", "answer": "Example answer" }'),
(1, 'Example true false', 2, 'This is example true false exercise',
'{ "tag": "TrueFalse", "question": "Example question", "correctBool": true }'),
(1, 'Example quiz', 1, 'This is example quiz exercise',
'{ "tag": "Quiz", "question": "Example question", "options": { "A": "Option A", "B": "Option B", "C": "Option C", "D": "Option D" }, "correctOption": "A" }'),
(1, 'Example essay', 3, 'This is example essay exercise',
'{ "tag": "Essay", "task": "Example essay", "model": "Model answer" }');

CREATE TABLE IF NOT EXISTS submissions (
	id SERIAL PRIMARY KEY,
	user__id INT NOT NULL,
	exercise__id INT NOT NULL,
	content JSONB NOT NULL,
	state TEXT NOT NULL,
	grade INT,
	comment TEXT,
	submitted TIMESTAMPTZ DEFAULT (now() at time zone('utc')),
	FOREIGN KEY (user__id) REFERENCES users(id),
	FOREIGN KEY (exercise__id) REFERENCES exercises(id)
);
