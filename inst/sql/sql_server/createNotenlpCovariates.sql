DROP TABLE IF EXISTS #covariates;
SELECT DISTINCT
c.@row_id_field as row_id,
{@include_nonexist == TRUE | @include_temporal == TRUE} ? {
CAST(nn.note_nlp_concept_id AS BIGINT) * 10000 + (CASE WHEN (nn.term_exists AND nn.term_temporal) THEN 3 WHEN (nn.term_exists) THEN 1 WHEN (nn.term_temporal) THEN 2 ELSE 0 END)*1000 + @analysis_id AS covariate_id,}:{
CAST(nn.note_nlp_concept_id AS BIGINT) * 10000 + @analysis_id AS covariate_id,
}
1 as covariate_value
INTO #covariates
FROM @cohort_table as c
INNER JOIN @note_database.@note_table as n on c.subject_id=n.person_id
LEFT JOIN @notenlp_database.@notenlp_table as nn on n.note_id=nn.note_id
{@domains != '' | @standard == TRUE} ? {LEFT JOIN @cdm_database_schema.concept as con on nn.note_nlp_concept_id=con.concept_id }
WHERE n.note_date <= DATEADD(DAY, @end_day, c.cohort_start_date)
{@start_day != 'anyTimePrior'} ? { AND n.note_date >= DATEADD(DAY, @start_day, c.cohort_start_date) }
{@domains != ''} ? { AND con.domain_id in (@domains) }
{@standard == TRUE} ? { AND con.standard_concept='S'}
{@include_nonexist == FALSE} ? { AND nn.term_exists = FALSE}
{@include_temporal == FALSE} ? { AND nn.term_temporal = FALSE}
AND nn.note_nlp_concept_id is not null AND nn.note_nlp_concept_id != 0
{@cohort_id != -1} ? {AND c.cohort_definition_id = @cohort_id};


DROP TABLE IF EXISTS #cov_ref;
SELECT
covariate_id,
{@start_day == 'anyTimePrior'} ? {
	CAST(CONCAT('Note_nlp ',domain_id,' any time prior through @end_day days relative to index: ', CASE WHEN concept_name IS NULL THEN 'Unknown concept' ELSE concept_name END,' ', CASE WHEN ((covariate_id % 10000) > 2999) THEN 'NEX-TEMP' WHEN ((covariate_id % 10000) > 1999) THEN 'TEMP' WHEN ((covariate_id % 10000) > 999) THEN 'NEX' ELSE '' END) AS VARCHAR(512)) AS covariate_name,
} : {
	CAST(CONCAT('Note_nlp ',domain_id,' during day @start_day through @end_day days relative to index: ', CASE WHEN concept_name IS NULL THEN 'Unknown concept' ELSE concept_name END,' ', CASE WHEN ((covariate_id % 10000) > 2999) THEN 'NEX-TEMP' WHEN ((covariate_id % 10000) > 1999) THEN 'TEMP' WHEN ((covariate_id % 10000) > 999) THEN 'NEX' ELSE '' END) AS VARCHAR(512)) AS covariate_name,
}
@analysis_id AS analysis_id,
CAST(ROUND((covariate_id - @analysis_id) / 10000 ,0) AS INT) AS concept_id
INTO #cov_ref
FROM (
	SELECT DISTINCT covariate_id
	FROM #covariates
	) t1
LEFT JOIN @cdm_database_schema.concept
	ON concept_id = CAST(ROUND((covariate_id - @analysis_id) / 10000 ,0) AS INT);


DROP TABLE IF EXISTS #analysis_ref;
SELECT *
INTO #analysis_ref
FROM (
SELECT @analysis_id AS analysis_id,
	CAST('@analysis_name' AS VARCHAR(512)) AS analysis_name,
	CAST('note_nlp' AS VARCHAR(20)) AS domain_id,
{@start_day == 'anyTimePrior'} ? {
	CAST(NULL AS INT) AS start_day,
} : {
	@start_day AS start_day,
}
	@end_day AS end_day,
	CAST('Y' AS VARCHAR(1)) AS is_binary,
	CAST(NULL AS VARCHAR(1)) AS missing_means_zero
) ar;
