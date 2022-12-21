<#include '../../layout/layout.ftl'>
<#import 'projectDetailsSummary.ftl' as projectDetailsSummary>
<#import 'plannedTenderSummary.ftl' as plannedTenderSummary>


<#macro summary projectDetailsSummaryView plannedTenderSummaryView>
  <#-- @ftlvariable name="projectDetailsSummaryView" type="uk.co.nstauthority.scap.scap.summary.ProjectDetailsSummaryView" -->
  <#-- @ftlvariable name="plannedTenderSummaryView" type="uk.co.nstauthority.scap.scap.summary.PlannedTenderSummaryView" -->

    <@fdsAccordion.accordion accordionId="scap-summary">
    <@fdsAccordion.accordionSection sectionHeading="Project details">
      <@projectDetailsSummary.projectDetailsSummary projectDetailsView=projectDetailsSummaryView />
    </@fdsAccordion.accordionSection>
    <@fdsAccordion.accordionSection sectionHeading="Planned tender activities">
      <@plannedTenderSummary.plannedTenderSummary plannedTenderSummaryView=plannedTenderSummaryView />
    </@fdsAccordion.accordionSection>
  </@fdsAccordion.accordion>
</#macro>
