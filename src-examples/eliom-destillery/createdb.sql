-- copyright Vincent Balat

CREATE TABLE users (
       userid bigserial primary key,
       email text NOT NULL UNIQUE,
       pwd text NOT NULL,
       firstname text NOT NULL,
       lastname text NOT NULL
);

