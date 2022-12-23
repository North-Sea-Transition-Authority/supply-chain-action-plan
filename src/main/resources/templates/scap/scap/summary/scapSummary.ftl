<#include '../../layout/layout.ftl'>
<#import 'projectDetailsSummary.ftl' as projectDetailsSummary>
<#import 'plannedTenderSummary.ftl' as plannedTenderSummary>
<#import 'actualTenderSummary.ftl' as actualTenderSummary>


<#macro summary projectDetailsSummaryView plannedTenderSummaryView>
<#-- @ftlvariable name="projectDetailsSummaryView" type="uk.co.nstauthority.scap.scap.summary.ProjectDetailsSummaryView" -->
<#-- @ftlvariable name="plannedTenderSummaryView" type="uk.co.nstauthority.scap.scap.summary.plannedtender.PlannedTenderSummaryView" -->
<#-- @ftlvariable name="actualTenderSummaryView" type="uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryView" -->

    <@fdsAccordion.accordion accordionId="scap-summary">
    <@fdsAccordion.accordionSection sectionHeading="Project details">
      <@projectDetailsSummary.projectDetailsSummary projectDetailsView=projectDetailsSummaryView />
    </@fdsAccordion.accordionSection>
    <@fdsAccordion.accordionSection sectionHeading="Planned tender activities">
      <@plannedTenderSummary.plannedTenderSummary plannedTenderSummaryView=plannedTenderSummaryView />
    </@fdsAccordion.accordionSection>
    <@fdsAccordion.accordionSection sectionHeading="Actual tender activities">
      <@actualTenderSummary.actualTenderSummary actualTenderSummaryView=actualTenderSummaryView />
    </@fdsAccordion.accordionSection>
  </@fdsAccordion.accordion>
</#macro>
