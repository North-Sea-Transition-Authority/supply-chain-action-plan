<#include '../../layout/layout.ftl'>
<#import '../summary/scapSummary.ftl' as scapSummary>
<#import '../summary/scapSummaryCard.ftl' as scapSummaryCard>
<#import '../timeline/scapTimelineEvents.ftl' as timeline>

<#assign pageTitle = projectName!'' />

<#-- @ftlvariable name="backLinkUrl" type="java.lang.String" -->
<#-- @ftlvariable name="projectReference" type="java.lang.String"-->
<#-- @ftlvariable name="projectName" type="java.lang.String"-->
<#-- @ftlvariable name="operator" type="java.lang.String" -->
<#-- @ftlvariable name="scapSummaryView" type="uk.co.nstauthority.scap.scap.summary.ScapSummaryView" -->
<#-- @ftlvariable name="timelineEvents" type="java.util.List<uk.co.nstauthority.scap.scap.timeline.TimelineEventView>" -->

<@defaultPage
htmlTitle=pageTitle
caption=operator
pageHeading=projectReference
pageSize=PageSize.FULL_WIDTH
backLinkUrl=springUrl(backLinkUrl)
>
  <@scapSummaryCard.summaryCard/>
  <@fdsTabs.tabs tabsHeading="SCAP overview tabs">
    <@fdsTabs.tabList>
      <@fdsTabs.tab tabLabel="Application form" tabAnchor="summary-tab"/>
      <#if timelineEvents?has_content>
        <@fdsTabs.tab tabLabel="Case Events" tabAnchor="events-tab"/>
      </#if>
    </@fdsTabs.tabList>
    <@fdsTabs.tabContent tabAnchor="summary-tab">
      <@scapSummary.summary scapSummaryView/>
    </@fdsTabs.tabContent>
      <#if timelineEvents?has_content>
        <@fdsTabs.tabContent tabAnchor="events-tab">
          <@timeline.timeline timelineEvents=timelineEvents/>
        </@fdsTabs.tabContent>
      </#if>
  </@fdsTabs.tabs>
</@defaultPage>
