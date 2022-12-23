<#include '../../layout/layout.ftl'>
<#import 'projectDetailsSummary.ftl' as projectDetailsSummary>
<#import 'plannedTenderSummary.ftl' as plannedTenderSummary>
<#import 'actualTenderSummary.ftl' as actualTenderSummary>


<#macro summary scapSummaryView>
<#-- @ftlvariable name="scapSummaryView" type="uk.co.nstauthority.scap.scap.summary.ScapSummaryView" -->

    <@fdsAccordion.accordion accordionId="scap-summary">
    <@fdsAccordion.accordionSection sectionHeading="Project details">
      <@projectDetailsSummary.projectDetailsSummary projectDetailsView=scapSummaryView.projectDetailsSummaryView() />
    </@fdsAccordion.accordionSection>
    <@fdsAccordion.accordionSection sectionHeading="Planned tender activities">
      <@plannedTenderSummary.plannedTenderSummary plannedTenderSummaryView=scapSummaryView.plannedTenderSummaryView() />
    </@fdsAccordion.accordionSection>
    <@fdsAccordion.accordionSection sectionHeading="Actual tender activities">
      <@actualTenderSummary.actualTenderSummary actualTenderSummaryView=scapSummaryView.actualTenderSummaryView() />
    </@fdsAccordion.accordionSection>
  </@fdsAccordion.accordion>
</#macro>
