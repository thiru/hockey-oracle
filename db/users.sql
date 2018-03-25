-- DROP SEQUENCE public.users_id_seq;

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
  is_active boolean NOT NULL DEFAULT true,
  CONSTRAINT users_pk PRIMARY KEY (id)
)
WITH (
  OIDS=FALSE
) ;
