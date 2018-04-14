/*
DROP TABLE public.users;

-- The following must be run once on a database in order to support
-- `gen_random_uuid()`:
CREATE EXTENSION pgcrypto;
*/

CREATE TABLE public.users (
  id SERIAL NOT NULL,
  name text NOT NULL,
  email text NOT NULL DEFAULT '',
  is_admin boolean NOT NULL DEFAULT false,
  password text NOT NULL DEFAULT '',
  salt text NOT NULL DEFAULT '',
  auto_auth text NOT NULL DEFAULT gen_random_uuid(),
  leagues jsonb NOT NULL DEFAULT '{}',
  position text NOT NULL DEFAULT '',
  email_opts jsonb NOT NULL DEFAULT '{"game-chats":true}',
  created_on timestamp NOT NULL DEFAULT (now() at time zone 'utc'),
  created_by text NOT NULL DEFAULT '',
  modified_on timestamp NOT NULL DEFAULT (now() at time zone 'utc'),
  modified_by text NOT NULL DEFAULT '',
  CONSTRAINT users_pk PRIMARY KEY (id),
  CONSTRAINT users_un UNIQUE (email)
)
WITH (
  OIDS=FALSE
) ;
