/*
DROP TABLE public.users;
DROP SEQUENCE public.users_id_seq;

-- The following must be run once on a database in order to support
-- `gen_random_uuid()`:
CREATE EXTENSION pgcrypto;
*/

CREATE SEQUENCE public.users_id_seq
INCREMENT BY 1
MINVALUE 1
MAXVALUE 9223372036854775807
START 1;

CREATE TABLE public.users (
  id int4 NOT NULL DEFAULT nextval('users_id_seq'::regclass),
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
