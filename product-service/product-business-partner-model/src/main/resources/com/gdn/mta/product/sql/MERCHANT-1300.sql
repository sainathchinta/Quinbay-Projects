CREATE INDEX idx_sequence_1
	ON prd_sequence
	USING btree(key);
	
CREATE OR REPLACE FUNCTION get_sequence(input_key character varying)
	RETURNS bigint AS
		$BODY$
			DECLARE
				ctr bigint;
			BEGIN
				LOOP
					UPDATE prd_sequence
						SET counter = counter + increment
						WHERE key = input_key;
					IF found THEN
						EXIT;
					END IF;
					BEGIN
						INSERT INTO prd_sequence(key) VALUES(input_key);
						UPDATE prd_sequence
							SET counter = counter + increment
							WHERE key = input_key;
						EXIT;
						EXCEPTION WHEN unique_violation THEN
							-- Do nothing
					END;
				END LOOP;
				SELECT counter INTO ctr
					FROM prd_sequence
					WHERE key = input_key;
				RETURN ctr;
			END;
		$BODY$
	LANGUAGE plpgsql VOLATILE COST 100;