<#include '../../layout/layout.ftl'>
<#import 'projectDetailsSummary.ftl' as projectDetailsSummary>


<#macro summary projectDetailsSummaryView>
  <#-- @ftlvariable name="projectDetailsSummaryView" type="uk.co.nstauthority.scap.scap.summary.ProjectDetailsSummaryView" -->

  <@fdsAccordion.accordion accordionId="scap-summary">
    <@fdsAccordion.accordionSection sectionHeading="Project details">
      <@projectDetailsSummary.projectDetailsSummary projectDetailsView=projectDetailsSummaryView />
    </@fdsAccordion.accordionSection>
  </@fdsAccordion.accordion>
</#macro>
