{DEFAULT @CDM_schema = 'CDM4_SIM'}

use @CDM_schema;

create table #cohort_params
(
	parameter_name varchar(255),
	parameter_value varchar(255)
);

/*****

parameters to use when playing with CDM4_SIM when you want a decent sample size in each cohort and some events in both groups

******/


insert into #cohort_params (parameter_name, parameter_value) values ('target_drug_concept_id','755695');  /*fluoxetine*/
insert into #cohort_params (parameter_name, parameter_value) values ('comparator_drug_concept_id','739138'); /*sertraline*/
insert into #cohort_params (parameter_name, parameter_value) values ('indication_concept_id','439926'); /*malaise and fatigue*/

insert into #cohort_params (parameter_name, parameter_value) values ('washout_window','183');
insert into #cohort_params (parameter_name, parameter_value) values ('indication_lookback_window','183');
insert into #cohort_params (parameter_name, parameter_value) values ('exposure_extension_window','7');
insert into #cohort_params (parameter_name, parameter_value) values ('study_start_date','11/1/2000');
insert into #cohort_params (parameter_name, parameter_value) values ('study_end_date','1/1/2014');

insert into #cohort_params (parameter_name, parameter_value) values ('exclusion_concept_id','4027133'); /*dialysis*/
insert into #cohort_params (parameter_name, parameter_value) values ('exclusion_concept_id','4032243'); /*dialysis procedure*/
insert into #cohort_params (parameter_name, parameter_value) values ('exclusion_concept_id','4146536'); /*dialysis procedure*/
insert into #cohort_params (parameter_name, parameter_value) values ('exclusion_concept_id','2002282'); /*dialysis procedure*/
insert into #cohort_params (parameter_name, parameter_value) values ('exclusion_concept_id','2213572'); /*dialysis procedure*/
insert into #cohort_params (parameter_name, parameter_value) values ('exclusion_concept_id','2005890'); /*joint replacement*/
insert into #cohort_params (parameter_name, parameter_value) values ('exclusion_concept_id','43534760'); /*factor Xa*/
insert into #cohort_params (parameter_name, parameter_value) values ('exclusion_concept_id','21601019'); /*direct thrombin inhibitor*/

insert into #cohort_params (parameter_name, parameter_value) values ('outcome_concept_id','194133');  /*low back pain*/
insert into #cohort_params (parameter_name, parameter_value) values ('outcome_condition_type_concept_id','38000215'); 
insert into #cohort_params (parameter_name, parameter_value) values ('outcome_condition_type_concept_id','38000216');  
insert into #cohort_params (parameter_name, parameter_value) values ('outcome_condition_type_concept_id','38000217');  
insert into #cohort_params (parameter_name, parameter_value) values ('outcome_condition_type_concept_id','38000218');  
insert into #cohort_params (parameter_name, parameter_value) values ('outcome_condition_type_concept_id','38000183');  
insert into #cohort_params (parameter_name, parameter_value) values ('outcome_condition_type_concept_id','38000232');  
insert into #cohort_params (parameter_name, parameter_value) values ('max_outcome_count','1');  /*number of conditoins, 1 is first occurrence*/





/*made data table that contains cohorts and end of observation period*/


	select distinct raw_cohorts.cohort_id,
		raw_cohorts.person_id,
		raw_cohorts.cohort_start_date,
		case when raw_cohorts.cohort_end_date <= op1.observation_period_end_date and raw_cohorts.cohort_end_date <= study_end_date then raw_cohorts.cohort_end_date
			when raw_cohorts.cohort_end_date > op1.observation_period_end_date and op1.observation_period_end_date <= study_end_date then op1.observation_period_end_date
			else study_end_date end as cohort_censor_date

			into #cohorts
	from
	(select cast(parameter_value as int) as washout_window from #cohort_params where parameter_name = 'washout_window') washout_window,
	(select cast(parameter_value as int) as indication_lookback_window from #cohort_params where parameter_name = 'indication_lookback_window') indication_lookback_window,
	(select cast(parameter_value as date) as study_start_date from #cohort_params where parameter_name = 'study_start_date') study_start_date,
	(select cast(parameter_value as date) as study_end_date from #cohort_params where parameter_name = 'study_end_date') study_end_date,
	(
	select drug_ids.cohort_id,
		de1.person_id, 
		min(de1.drug_era_start_date) as cohort_start_date,
		min(dateadd(dd,exposure_extension_window,de1.drug_era_end_date)) as cohort_end_date
	from 
	(select cast(parameter_value as int) as exposure_extension_window from #cohort_params where parameter_name = 'exposure_extension_window') exposure_extension_window,
	drug_era de1
	inner join
	concept_ancestor ca1
	on de1.drug_concept_id = ca1.descendant_concept_id
	inner join 
	(select case when parameter_name = 'target_drug_concept_id' then 1 when parameter_name = 'comparator_drug_concept_id' then 0 else -1 end as cohort_id, 
		cast(parameter_value as int) as cohort_concept_id
		from #cohort_params 
		where parameter_name in ('target_drug_concept_id','comparator_drug_concept_id') 
	) drug_ids
	on ca1.ancestor_concept_id = drug_ids.cohort_concept_id
	group by drug_ids.cohort_id, de1.person_id
	) raw_cohorts
	inner join
	observation_period op1
	on raw_cohorts.person_id = op1.person_id
	inner join
	(
	select person_id, condition_start_date as indication_date
	from condition_occurrence
	where condition_concept_id in (
		select descendant_concept_id
		from concept_ancestor 
		where ancestor_concept_id in (select cast(parameter_value as int) as indication_concept_id from #cohort_params where parameter_name in ('indication_concept_id') ) 
		)
	) indication
	on raw_cohorts.person_id = indication.person_id
	where raw_cohorts.cohort_start_date >= dateadd(dd,washout_window,op1.observation_period_start_date)
	and raw_cohorts.cohort_start_date <= op1.observation_period_end_date
	and raw_cohorts.cohort_start_date <= dateadd(dd,indication_lookback_window,indication.indication_date)
	and raw_cohorts.cohort_start_date >= indication.indication_date
	and raw_cohorts.cohort_start_date >= study_start_date
	and raw_cohorts.cohort_start_date <= study_end_date
	;


/* delete persons in both cohorts (may need to revise if want to deal with persons in multiple cohorts but without overlapping time-at-risk )   */
delete from #cohorts
where person_id in (

select person_id
from
(
select person_id, count(cohort_id) as num_cohorts
from #cohorts
group by person_id
) t1
where num_cohorts = 2

);


/*delete persons with exclusion criteria*/

/*******
MARTIJN:   these would not pass on SQLRender because DELETE JOIN syntax is different

delete c1
from #cohorts c1
inner join 
(select person_id, condition_start_date
from condition_occurrence
where condition_concept_id in (select descendant_concept_id from concept_ancestor where ancestor_concept_id in (select cast(parameter_value as int) from #cohort_params where parameter_name in ('exclusion_concept_id')  ) )
) co1
on c1.person_id = co1.person_id
where c1.cohort_start_date > co1.condition_start_date
;


delete c1
from #cohorts c1
inner join
(select person_id, procedure_date
from procedure_occurrence
where procedure_concept_id in (select descendant_concept_id from concept_ancestor where ancestor_concept_id in (select cast(parameter_value as int) from #cohort_params where parameter_name in ('exclusion_concept_id')  ) )
) po1
on c1.person_id = po1.person_id
where c1.cohort_start_date > po1.procedure_date
;


delete c1
from #cohorts c1
inner join
(select person_id, drug_exposure_start_date
from drug_exposure
where drug_concept_id in (select descendant_concept_id from concept_ancestor where ancestor_concept_id in (select cast(parameter_value as int) from #cohort_params where parameter_name in ('exclusion_concept_id')  ) )
) de1
on c1.person_id = de1.person_id
where c1.cohort_start_date > de1.drug_exposure_start_date
;

********/


/* find covariates  */

create table #covariates
(
	cohort_id int,
	person_id numeric,
	covariate_id numeric,
	covariate_value decimal
);



create table #concept_counts
(
	concept_id int,
	num_records int
);


select distinct c1.concept_id as snomed_concept_id,
		c2.concept_id as meddra_concept_id

			into #snomed_to_all_meddra 
	from 	concept c1
		inner join concept_ancestor ca1
		on c1.concept_id = ca1.descendant_concept_id
		and c1.vocabulary_id = 1
		inner join concept c2
		on ca1.ancestor_concept_id = c2.concept_id
		and c2.vocabulary_id = 15
;



select distinct c1.concept_id as rxnorm_concept_id,
		c2.concept_id as atc_concept_id
			into #rxnorm_to_atc
	from concept c1
	inner join concept_ancestor ca1
	on c1.concept_id = ca1.descendant_concept_id
	and c1.vocabulary_id = 8
	and c1.concept_class = 'Ingredient'
	inner join concept c2
	on ca1.ancestor_concept_id = c2.concept_id
	and c2.vocabulary_id = 21
	and len(c2.concept_code) in (1, 3, 5)
	;



	--gender
		insert into #covariates (cohort_id, person_id, covariate_id, covariate_value)
		select ca1.cohort_id, ca1.person_id, gender_concept_id as covariate_id, 1 as covariate_value
		from #cohorts ca1
			inner join
			person p1
			on ca1.person_id = p1.person_id
			where gender_concept_id = 8507
		;


		--age decile
		insert into #covariates (cohort_id, person_id, covariate_id, covariate_value)
		select ca1.cohort_id, ca1.person_id, floor((year(ca1.cohort_start_date) - p1.YEAR_OF_BIRTH)/10)+1 as covariate_id, 1 as covariate_value
		from #cohorts ca1
			inner join
			person p1
			on ca1.person_id = p1.person_id
		;


		--Number of distinct conditions in last 6mo
		insert into #covariates (cohort_id, person_id, covariate_id, covariate_value)
		select ca1.cohort_id, ca1.person_id, 20 as covariate_id, count(distinct ce1.condition_concept_id) as covariate_value
		from #cohorts ca1
			inner join
			condition_era ce1
			on ca1.person_id = ce1.person_id
		where ce1.condition_era_start_date <= ca1.cohort_start_date
			and ce1.condition_era_start_date >= dateadd(dd,-365,ca1.cohort_start_date)
		group by ca1.cohort_id, ca1.person_id
		;


		--Number of distinct drugs in last 6mo
		insert into #covariates (cohort_id, person_id, covariate_id, covariate_value)
		select ca1.cohort_id, ca1.person_id, 21 as covariate_id, count(distinct de1.drug_concept_id) as covariate_value
		from #cohorts ca1
			inner join
			drug_era de1
			on ca1.person_id = de1.person_id
		where de1.drug_era_start_date <= ca1.cohort_start_date
			and de1.drug_era_start_date >= dateadd(dd,-365,ca1.cohort_start_date)
		group by ca1.cohort_id, ca1.person_id
		;


		--Number of distinct procedures in last 6mo
		insert into #covariates (cohort_id, person_id, covariate_id, covariate_value)
		select ca1.cohort_id, ca1.person_id, 22 as covariate_id, count(distinct po1.procedure_concept_id) as covariate_value
		from #cohorts ca1
			inner join
			procedure_occurrence po1
			on ca1.person_id = po1.person_id
		where po1.procedure_date <= ca1.cohort_start_date
			and po1.procedure_date >= dateadd(dd,-365,ca1.cohort_start_date)
		group by ca1.cohort_id, ca1.person_id
		;


		--Number of distinct outpatient visits in last 6mo
		insert into #covariates (cohort_id, person_id, covariate_id, covariate_value)
		select ca1.cohort_id, ca1.person_id, 23 as covariate_id, count(vo1.VISIT_OCCURRENCE_ID) as covariate_value
		from #cohorts ca1
			inner join
			visit_occurrence vo1
			on ca1.person_id = vo1.person_id
		where vo1.visit_start_date <= ca1.cohort_start_date
			and vo1.visit_start_date >= dateadd(dd,-365,ca1.cohort_start_date)
			and vo1.place_of_service_concept_id = 9202
		group by ca1.cohort_id, ca1.person_id
		;


		--Number of distinct inpatient visits in last 6mo
		insert into #covariates (cohort_id, person_id, covariate_id, covariate_value)
		select ca1.cohort_id, ca1.person_id, 24 as covariate_id, count(vo1.VISIT_OCCURRENCE_ID) as covariate_value
		from #cohorts ca1
			inner join
			visit_occurrence vo1
			on ca1.person_id = vo1.person_id
		where vo1.visit_start_date <= ca1.cohort_start_date
			and vo1.visit_start_date >= dateadd(dd,-365,ca1.cohort_start_date)
			and vo1.place_of_service_concept_id = 9201
		group by ca1.cohort_id, ca1.person_id
		;


		--Number of distinct ER visits in last 6mo
		insert into #covariates (cohort_id, person_id, covariate_id, covariate_value)
		select ca1.cohort_id, ca1.person_id, 25 as covariate_id, count(vo1.VISIT_OCCURRENCE_ID) as covariate_value
		from #cohorts ca1
			inner join
			visit_occurrence vo1
			on ca1.person_id = vo1.person_id
		where vo1.visit_start_date <= ca1.cohort_start_date
			and vo1.visit_start_date >= dateadd(dd,-365,ca1.cohort_start_date)
			and vo1.place_of_service_concept_id = 9203
		group by ca1.cohort_id, ca1.person_id
		;



		
		--add covariate per SNOMED condition
		 delete from #concept_counts;
		 insert into #concept_counts (concept_id, num_records)
		 select ce1.condition_concept_id, count(ce1.person_id) as num_records
		from #cohorts ca1
			inner join
			condition_era ce1
			on ca1.person_id = ce1.person_id
		where ce1.condition_era_start_date <= ca1.cohort_start_date
			and ce1.condition_era_start_date >= dateadd(dd,-365,ca1.cohort_start_date)
			and ce1.condition_concept_id > 0
		group by ce1.condition_concept_id
		;

		
		insert into #covariates (cohort_id, person_id, covariate_id, covariate_value)
		select ca1.cohort_id, ca1.person_id, ce1.condition_concept_id as covariate_id, 1 as covariate_value
		from #cohorts ca1
			inner join
			condition_era ce1
			on ca1.person_id = ce1.person_id
		where ce1.condition_era_start_date <= ca1.cohort_start_date
			and ce1.condition_era_start_date >= dateadd(dd,-365,ca1.cohort_start_date)
			and ce1.condition_concept_id > 0
			and ce1.condition_concept_id in (select concept_id from #concept_counts where num_records > 100)
		group by ca1.cohort_id, ca1.person_id, ce1.condition_concept_id
		;
	

		--add covariate per MEDDRA concept SNOMED condition
		insert into #covariates (cohort_id, person_id, covariate_id, covariate_value)
		select ca1.cohort_id, ca1.person_id, stam1.meddra_concept_id as covariate_id, 1 as covariate_value
		from #covariates ca1
			inner join
			#snomed_to_all_meddra stam1 
			on ca1.covariate_id = stam1.snomed_concept_id
		group by ca1.cohort_id, ca1.person_id, stam1.meddra_concept_id
		;

		--add covariate per RxNorm ingredient
		delete from #concept_counts ;
		insert into #concept_counts (concept_id, num_records)
		 select de1.drug_concept_id, count(de1.person_id) as num_records
		from #cohorts ca1
			inner join
			drug_era de1
			on ca1.person_id = de1.person_id
		where de1.drug_era_start_date <= ca1.cohort_start_date
			and de1.drug_era_start_date >= dateadd(dd,-365,ca1.cohort_start_date)
			and de1.drug_concept_id > 0
		group by de1.drug_concept_id
		;
		
		
		insert into #covariates (cohort_id, person_id, covariate_id, covariate_value)
		select ca1.cohort_id, ca1.person_id, de1.drug_concept_id as covariate_id, 1 as covariate_value
		from #cohorts ca1
			inner join
			drug_era de1
			on ca1.person_id = de1.person_id
		where de1.drug_era_start_date < ca1.cohort_start_date
			and de1.drug_era_start_date >= dateadd(dd,-365,ca1.cohort_start_date)
			and de1.drug_concept_id > 0
			and de1.drug_concept_id in (select concept_id from #concept_counts where num_records > 100)
			
		group by ca1.cohort_id, ca1.person_id, de1.drug_concept_id
		;

		--add covariate per ATC class concept within RxNorm drug
		insert into #covariates (cohort_id, person_id, covariate_id, covariate_value)
		select ca1.cohort_id, ca1.person_id, rta1.atc_concept_id as covariate_id, 1 as covariate_value
		from #covariates ca1
			inner join
			#rxnorm_to_atc rta1 
			on ca1.covariate_id = rta1.rxnorm_concept_id
		group by ca1.cohort_id, ca1.person_id, rta1.atc_concept_id
		;



		--number of drugs within each ATC3 groupings
		insert into #covariates (cohort_id, person_id, covariate_id, covariate_value)
		select ca1.cohort_id, ca1.person_id, 2000000000 + t1.atc_concept_id, count(distinct ca1.covariate_id)
		from #covariates ca1
			inner join
			(
				select c1.concept_id as rxnorm_concept_id,
					c2.concept_id as atc_concept_id
				from concept c1
					inner join concept_ancestor ca1
					on c1.concept_id = ca1.descendant_concept_id
					and c1.vocabulary_id = 8
					and c1.concept_level = 2
					inner join concept c2
					on c2.concept_id = ca1.ancestor_concept_id
					and c2.vocabulary_id = 21
					and len(c2.concept_code) in (3)
			) t1
			on ca1.covariate_id = t1.rxnorm_concept_id
		group by ca1.cohort_id, ca1.person_id, 2000000000 + t1.atc_concept_id
		;



		--add covariate per procedure
		delete from #concept_counts ;
		insert into #concept_counts (concept_id, num_records)
		 select po1.procedure_concept_id, count(po1.person_id) as num_records
		from #cohorts ca1
			inner join
			procedure_occurrence po1
			on ca1.person_id = po1.person_id
			inner join
			concept c1
			on po1.procedure_concept_id = c1.concept_id
			and c1.vocabulary_id in (3, 4)
		where po1.procedure_date <= ca1.cohort_start_date
			and po1.procedure_date >= dateadd(dd,-365,ca1.cohort_start_date)
			and po1.procedure_concept_id > 0
		group by po1.procedure_concept_id
		;

		insert into #covariates (cohort_id, person_id, covariate_id, covariate_value)
		select ca1.cohort_id, ca1.person_id, po1.procedure_concept_id as covariate_id, 1 as covariate_value
		from #cohorts ca1
			inner join
			procedure_occurrence po1
			on ca1.person_id = po1.person_id
		where po1.procedure_date < ca1.cohort_start_date
			and po1.procedure_date >= dateadd(dd,-365,ca1.cohort_start_date)
			and po1.procedure_concept_id > 0
			and po1.procedure_concept_id in (select concept_id from #concept_counts where num_records > 100)
			
		group by ca1.cohort_id, ca1.person_id, po1.procedure_concept_id
		;

















/*   find outcomes  */


	select cohort_id, person_id, max(outcomes.outcome_count) as num_outcomes, min(datediff(dd,cohort_start_date,outcome_date)) as time_to_censor
		into #outcomes
	from
	(
	select c1.cohort_id, c1.person_id, c1.cohort_start_date, co1.condition_start_date as outcome_date, row_number() over (partition by c1.person_id order by co1.condition_start_date asc) as outcome_count
	from #cohorts c1
	inner join condition_occurrence co1
	on c1.person_id = co1.person_id
	where co1.condition_concept_id in (
		select descendant_concept_id
		from concept_ancestor 
		where ancestor_concept_id in (select cast(parameter_value as int) as outcome_concept_id from #cohort_params where parameter_name in ('outcome_concept_id') ) 
		)
	and co1.condition_type_concept_id in (select cast(parameter_value as int) from #cohort_params where parameter_name in ('outcome_condition_type_concept_id') )
	and co1.condition_start_date > c1.cohort_start_date
	and co1.condition_start_Date <= c1.cohort_censor_date
	) outcomes,
	(select cast(parameter_value as int) as max_outcome_count from #cohort_params where parameter_name in ('max_outcome_count') ) max_outcome_count
	where outcomes.outcome_count <= max_outcome_count
	group by cohort_id, person_id
;





--tables for propensity score fitting

select 1 as stratum_id, c1.person_id as row_id, cohort_id as y_value, 0 as time_to_censor
	into #ccd_outcome_input_for_ps
from #cohorts c1
order by c1.person_id
;



select 1 as stratum_id, person_id as row_id, covariate_id, covariate_value
	into #ccd_covariate_input_for_ps
from #covariates
order by person_id
;




--tables for final outcome model   *****need to get matching variables from strata
select 1 as stratum_id, c1.person_id as row_id, case when o1.person_id is null then 0 else o1.num_outcomes end as y_value, case when o1.person_id is null then datediff(dd,c1.cohort_start_date,c1.cohort_censor_date) else o1.time_to_censor end as time_to_censor
	into #ccd_outcome_input_for_outcome
from #cohorts c1
left join
#outcomes o1
on c1.cohort_id = o1.cohort_id
and c1.person_id = o1.person_id
order by c1.person_id
;

select stratum_id, row_id, covariate_id, covariate_value
	into #ccd_covariate_input_for_outcome
from
(
select 1 as stratum_id, person_id as row_id, covariate_id, covariate_value
	
from #covariates

union

select 1 as stratum_id, person_id as row_id, 0 as covariate_id, cohort_id as covariate_value
from #cohorts
) t1
;


/**summary of number of persona nd number of events*/
/*
select c1.cohort_id, count(c1.person_id) as num_persons, sum(case when o1.person_id is null then 0 else num_outcomes end) as num_outcomes
from #cohorts c1
left join #outcomes o1
on c1.cohort_id = o1.cohort_id
and c1.person_id = o1.person_id
group by c1.cohort_id
*/

truncate table #cohort_params;
drop table #cohort_params;
truncate table #cohorts;
drop table #cohorts;
truncate table #covariates;
drop table #covariates;
truncate table #concept_counts;
drop table #concept_counts;
truncate table #snomed_to_all_meddra;
drop table #snomed_to_all_meddra;
truncate table #rxnorm_to_atc;
drop table #rxnorm_to_atc;
truncate table #outcomes;
drop table #outcomes;