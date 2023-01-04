<#include '../../layout/layout.ftl'>
<#import '../summary/scapSummary.ftl' as scapSummary>
<#import '../summary/scapSummaryCard.ftl' as scapSummaryCard>

<#assign pageTitle = projectName!'' />

<#-- @ftlvariable name="backLinkUrl" type="java.lang.String" -->
<#-- @ftlvariable name="projectReference" type="java.lang.String"-->
<#-- @ftlvariable name="projectName" type="java.lang.String"-->
<#-- @ftlvariable name="operator" type="java.lang.String" -->
<#-- @ftlvariable name="scapSummaryView" type="uk.co.nstauthority.scap.scap.summary.ScapSummaryView" -->

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
    </@fdsTabs.tabList>
    <@fdsTabs.tabContent tabAnchor="summary-tab">
      <@scapSummary.summary scapSummaryView/>
    </@fdsTabs.tabContent>
  </@fdsTabs.tabs>
</@defaultPage>
