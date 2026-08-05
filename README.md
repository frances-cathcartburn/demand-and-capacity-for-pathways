# demand-and-capacity-for-pathways
A stocks and flows-style model to facilitate demand and capacity modelling of patient pathways.
If a pathway can be mapped as a flow diagram with known proportions for the outcome of each event, then it can be modelled with this approach.

## Example 1

This example is an introduction of the ideas behind the model, and the six essential configuration files. It's recommended that new users look at the folder example_1 to see what these config files should look like. For detailed technical documentation including optional extra features, please see Technical Documentation.docx.

### Example 1 - The Model
The diagram below shows a fictional pathway that can be modelled.
\
\
![Diagram showing a patient pathway, including Fictional New appointment, Fictional Consultant Follow-Up Intervention, Fictional Nurse Follow-Up, Fictional Consultant Follow-Up Non-Intervention and Surgery.](/example_1/example_1_diagram.png)
\
\
Each blue rectangle represents an **event**. The arrows from each rectangle point to each **status** that the event can result in. Events and statuses are the key concepts of the model - statuses are stocks and events are flows.
\
\
For example, 50% of patients having a 'Fictional New' appointment will have an outcome status of 'Waiting for FUP Non-Intervention'. Every status corresponds to waiting for a specific future event, with the exception of exit statuses (e.g. Discharge). If a patient reaches a Discharge status, no further events will happen to them.
\
\
Model Inputs:
- Every event needs to be listed in a configuration file called model_events.csv.
- Every status needs to be listed in a configuration file called model_statuses.csv. Any discharge statuses should be marked as exit statuses.
- Each *arrow* from a status to its subsequent event needs to be added as a row in a configuration file called model_event_recipient.csv.
- Each *arrow* from an event to one of its outcome statuses needs to be added a row in a configuration file called model_event_outcome.csv.

There is one hard-coded special event, 'New Referral Received', which needs to be included in model_event_outcome.csv to say what happens to new referrals to the pathway.

### Example 1 - The Scenario
For a given model, multiple scenarios can be specified and the results compared. This allows 'what-if' exploration, such as "What if the number of referrals increased?" or "What if we offered more Nurse Follow-Ups?".
\
\
All scenarios should have consistent time periods, e.g. months, weeks or days.
\
\
Scenario Inputs:
- Every status needs to be listed in a configuration file called scenario_demand_initial.csv, with a column for the initial number of patients who are already waiting at that status at the start of the modelling timeframe.
- A configuration file called scenario_capacity_and_demand_referrals.csv should have a row for every month or week or day, going as far into the future as you want to run the model. There needs to be one column for New Referrals, showing how many new referrals the pathway expects to see each month/week/day. There also needs to be a column for each event, giving the capacity for that event each month/week/day.