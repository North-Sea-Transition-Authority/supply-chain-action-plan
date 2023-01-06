<#include '../layout/layout.ftl'>

<#-- @ftlvariable name="canStartScap" type="java.lang.Boolean" -->
<#-- @ftlvariable name="workAreaItems" type="java.util.List<uk.co.nstauthority.scap.workarea.WorkAreaItem>" -->

<#assign pageTitle = "Work area" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
>
  <#if canStartScap>
    <@fdsAction.link linkText="Start new SCAP" linkClass="govuk-button" linkUrl=springUrl(startScapUrl)/>
  </#if>
  <@fdsResultList.resultList resultCount=workAreaItems?size>
    <#list workAreaItems as workAreaItem>
      <@scapResultItem workAreaItem=workAreaItem />
    </#list>
  </@fdsResultList.resultList>
</@defaultPage>

<#macro scapResultItem workAreaItem>
<#-- @ftlvariable name="workAreaItem" type="uk.co.nstauthority.scap.workarea.WorkAreaItem" -->
  <@fdsResultList.resultListItem
    linkHeadingText=workAreaItem.reference()
    linkHeadingUrl=springUrl(workAreaItem.url())
    captionHeadingText=workAreaItem.operator()
  >
    <@fdsResultList.resultListDataItem>
      <@fdsResultList.resultListDataValue key="Project name" value=workAreaItem.projectName()!""/>
      <@fdsResultList.resultListDataValue key="Status" value=workAreaItem.status().displayName!""/>
      <@fdsResultList.resultListDataValue key="Submission stage" value=workAreaItem.submissionStage().displayName!""/>
    </@fdsResultList.resultListDataItem>
  </@fdsResultList.resultListItem>
</#macro>
