CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  username VARCHAR(40) NOT NULL,
  password VARCHAR NOT NULL,
  email VARCHAR(40) NOT NULL
);
       
