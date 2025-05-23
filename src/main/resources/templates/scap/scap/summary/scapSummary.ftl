<#include '../../layout/layout.ftl'>
<#import 'projectDetailsSummary.ftl' as projectDetailsSummary>
<#import 'plannedTenderSummary.ftl' as plannedTenderSummary>
<#import 'actualTenderSummary.ftl' as actualTenderSummary>
<#import 'contractingPerformance.ftl' as contractingPerformance>
<#import 'projectPerformanceSummary.ftl' as projectPerformanceSummary>
<#import 'relatedPathfinderProjectsSummary.ftl' as relatedPathfinderProjectsSummary>


<#macro summary scapSummaryView>
<#-- @ftlvariable name="scapSummaryView" type="uk.co.nstauthority.scap.scap.summary.ScapSummaryView" -->
    <@fdsAccordion.accordion accordionId="scap-summary">
    <@fdsAccordion.accordionSection sectionHeading="Project details">
      <@projectDetailsSummary.projectDetailsSummary projectDetailsView=scapSummaryView.projectDetailsSummaryView() />
    </@fdsAccordion.accordionSection>
    <@fdsAccordion.accordionSection sectionHeading="Planned tender activities">
      <@plannedTenderSummary.plannedTenderSummary plannedTenderSummaryView=scapSummaryView.plannedTenderSummaryView() />
    </@fdsAccordion.accordionSection>
    <@fdsAccordion.accordionSection sectionHeading="Related Pathfinder projects">
      <@relatedPathfinderProjectsSummary.relatedPathfinderProjectsSummary
        relatedPathfinderProjectsSummaryView=scapSummaryView.pathfinderProjectsSummaryView()
      />
    </@fdsAccordion.accordionSection>
    <@fdsAccordion.accordionSection sectionHeading="Actual tender activities">
      <@actualTenderSummary.actualTenderSummary actualTenderSummaryView=scapSummaryView.actualTenderSummaryView() />
    </@fdsAccordion.accordionSection>
    <@fdsAccordion.accordionSection sectionHeading="Contracting performance">
      <@contractingPerformance.contractingPerformanceOverviewSummary
        contractingPerformanceOverviewSummaryView=scapSummaryView.contractingPerformanceOverviewSummaryView() />
    </@fdsAccordion.accordionSection>
    <@fdsAccordion.accordionSection sectionHeading="Project performance and close-out">
      <@projectPerformanceSummary.projectPerformanceSummary
        projectPerformanceSummaryView=scapSummaryView.projectPerformanceSummaryView()
      />
    </@fdsAccordion.accordionSection>
  </@fdsAccordion.accordion>
</#macro>
