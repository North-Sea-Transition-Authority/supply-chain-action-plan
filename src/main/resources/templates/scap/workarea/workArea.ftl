<#include '../layout/layout.ftl'>

<#-- @ftlvariable name="canStartScap" type="java.lang.Boolean" -->
<#-- @ftlvariable name="workAreaItems" type="java.util.List<uk.co.nstauthority.scap.workarea.WorkAreaItem>" -->

<#assign pageTitle = "Work area" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.FULL_WIDTH
  wrapperWidth=true
>
  <#if canStartScap>
    <@fdsAction.link linkText="Start new SCAP" linkClass="govuk-button" linkUrl=springUrl(startScapUrl)/>
  </#if>
  <@fdsSearch.searchFilter
    oneThirdWidth=true
  >
    <@fdsSearch.searchFilterList
      clearFilterUrl=springUrl(clearFiltersUrl)
      filterButtonClass="govuk-button govuk-button--secondary"
    >
      <@fdsSearch.searchFilterItem itemName="SCAP status">
        <@fdsSearch.searchCheckboxes
          path="form.scapStatuses"
          checkboxes=statusCheckboxes
        />
      </@fdsSearch.searchFilterItem>
    </@fdsSearch.searchFilterList>
  </@fdsSearch.searchFilter>
  <@fdsSearch.searchPageContent twoThirdsWidth=true>
    <#--noinspection FtlCallsInspection-->
    <@fdsResultList.resultList resultCount=workAreaItems?size>
      <#list workAreaItems as workAreaItem>
        <@scapResultItem workAreaItem=workAreaItem />
      </#list>
    </@fdsResultList.resultList>
  </@fdsSearch.searchPageContent>
</@defaultPage>

<#macro scapResultItem workAreaItem>
<#-- @ftlvariable name="workAreaItem" type="uk.co.nstauthority.scap.workarea.WorkAreaItem" -->
  <#assign tagContentInfoRequest>
    <#if workAreaItem.outstandingInformationRequest()>
      <@fdsResultList.resultListTag tagClass="govuk-tag--blue" tagText="Further information requested"/>
    </#if>
  </#assign>
  <@fdsResultList.resultListItem
    linkHeadingText=workAreaItem.reference()
    linkHeadingUrl=springUrl(workAreaItem.url())
    captionHeadingText=workAreaItem.operator()
    itemTag=tagContentInfoRequest
  >
    <@fdsResultList.resultListDataItem>
      <@fdsResultList.resultListDataValue key="Project name" value=workAreaItem.projectName()!""/>
      <@fdsResultList.resultListDataValue key="Status" value=workAreaItem.status().displayName!""/>
      <@fdsResultList.resultListDataValue key="Submission stage" value=workAreaItem.submissionStage().displayName!""/>
    </@fdsResultList.resultListDataItem>
  </@fdsResultList.resultListItem>
</#macro>
