<#include '../../layout/layout.ftl'>

<#macro relatedPathfinderProjectsSummary relatedPathfinderProjectsSummaryView>
<#-- @ftlvariable name="relatedPathfinderProjectsSummaryView" type="uk.co.nstauthority.scap.scap.summary.RelatedPathfinderProjectsSummaryView" -->
  <#if relatedPathfinderProjectsSummaryView.hasPathfinderProjects()?has_content>
    <#if relatedPathfinderProjectsSummaryView.hasPathfinderProjects()>
      <@relatedPathfinderProjectsSummaryCard relatedPathfinderProjectsSummaryView=relatedPathfinderProjectsSummaryView />
    <#else>
      <@fdsInsetText.insetText>
        This SCAP doesn't have any related Pathfinder projects
      </@fdsInsetText.insetText>
    </#if>
  <#else>
    <@fdsInsetText.insetText>
      No information on related Pathfinder projects has been provided
    </@fdsInsetText.insetText>
  </#if>

</#macro>

<#macro relatedPathfinderProjectsSummaryCard relatedPathfinderProjectsSummaryView>
<#-- @ftlvariable name="relatedPathfinderProjectsSummaryView" type="uk.co.nstauthority.scap.scap.summary.RelatedPathfinderProjectsSummaryView" -->
  <@fdsSummaryList.summaryListCard
    headingText="Related Pathfinder projects"
    summaryListId="pathfinder-projects-summary-card"
  >
    <#list relatedPathfinderProjectsSummaryView.pathfinderProjectNames() as pathfinderProjectName>
      <@fdsSummaryList.summaryListRowNoAction keyText="Pathfinder project ${pathfinderProjectName_index + 1}">
        ${pathfinderProjectName}
      </@fdsSummaryList.summaryListRowNoAction>
    </#list>
  </@fdsSummaryList.summaryListCard>
</#macro>
