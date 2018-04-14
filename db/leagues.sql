/*
DROP TABLE public.leagues;
*/

CREATE TABLE public.leagues (
  id SERIAL NOT NULL,
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
