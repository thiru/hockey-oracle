/*
DROP TABLE public.leagues;
DROP SEQUENCE public.leagues_id_seq;
*/

CREATE SEQUENCE public.leagues_id_seq
INCREMENT BY 1
MINVALUE 1
MAXVALUE 9223372036854775807
START 1;

CREATE TABLE public.leagues (
  id int4 NOT NULL DEFAULT nextval('leagues_id_seq'::regclass),
  name text NOT NULL,
  tricode text NOT NULL DEFAULT '',
  is_active boolean NOT NULL DEFAULT true,
  created_on timestamp NOT NULL DEFAULT (now() at time zone 'utc'),
  created_by text NOT NULL DEFAULT '',
  modified_on timestamp NOT NULL DEFAULT (now() at time zone 'utc'),
  modified_by text NOT NULL DEFAULT '',
  CONSTRAINT leagues_pk PRIMARY KEY (id),
	CONSTRAINT leagues_un UNIQUE (tricode)
)
WITH (
  OIDS=FALSE
) ;
