-- import Notes from note table using a cohort and start and end days.

SELECT c.@row_id_field, notes.note_id, notes.note_date, notes.note_text
FROM @cohort_table AS c
LEFT JOIN @note_database_schema.@note_table as notes
ON (c.subject_id=notes.person_id
AND (c.cohort_start_date + @endDay*interval '1 day') >= notes.note_date
AND (c.cohort_start_date + @startDay*interval '1 day') <= notes.note_date)
WHERE 1=1 {@customWhere != ''} ? {AND @customWhere} {@cohort_id != -1} ? {AND c.cohort_definition_id = @cohort_id}
