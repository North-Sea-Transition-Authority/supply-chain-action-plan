<#include '../layout/layout.ftl'>

<#-- @ftlvariable name="canStartScap" type="java.lang.Boolean" -->
<#-- @ftlvariable name="workAreaItems" type="java.util.List<uk.co.nstauthority.scap.workarea.WorkAreaItem>" -->

<#assign pageTitle = "Work area" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.FULL_WIDTH
  wrapperWidth=true
  topNavigation=true
>
  <#if canStartScap>
    <@fdsAction.link linkText="Start new SCAP" linkClass="govuk-button" linkUrl=springUrl(startScapUrl)/>
  </#if>
  <@fdsSearch.searchPage>
    <@fdsSearch.searchFilter
      oneThirdWidth=true
    >
      <@fdsSearch.searchFilterList
        clearFilterUrl=springUrl(clearFiltersUrl)
        filterButtonClass="govuk-button govuk-button--secondary"
      >
        <@referenceFilter form=form />
        <#if isRegulator>
          <@operatorFilter form=form preselectedOperator=prefilledOperator />
        </#if>
        <@fieldFilter form=form preselectedField=prefilledField />
        <@statusFilter form=form statusCheckboxes=statusCheckboxes />
        <@projectTypeFilter form=form projectTypeCheckboxes=projectTypeCheckboxes />
        <@updateStatusFilter form=form updateRequestRadios=updateRequestRadios />
      </@fdsSearch.searchFilterList>
    </@fdsSearch.searchFilter>
    <#if workAreaItems?has_content>
      <@fdsSearch.searchPageContent twoThirdsWidth=true>
        <#--noinspection FtlCallsInspection-->
        <@fdsResultList.resultList resultCount=workAreaItems?size>
          <#list workAreaItems as workAreaItem>
            <@scapResultItem workAreaItem=workAreaItem />
          </#list>
        </@fdsResultList.resultList>
      </@fdsSearch.searchPageContent>
    <#else>
      <@fdsSearch.searchPageContent twoThirdsWidth=true>
        <h2 class="govuk-heading-s">There are no matching results</h2>
        <p class="govuk-body">Improve your results by:</p>
        <ul class="govuk-list govuk-list--bullet">
          <li>removing filters</li>
          <li>double-checking your spelling</li>
          <li>using fewer keywords</li>
          <li>searching for something less specific</li>
        </ul>
      </@fdsSearch.searchPageContent>
    </#if>

  </@fdsSearch.searchPage>
</@defaultPage>

<#macro scapResultItem workAreaItem>
<#-- @ftlvariable name="workAreaItem" type="uk.co.nstauthority.scap.workarea.WorkAreaItem" -->
  <#assign tagContentInfoRequest>
    <#if workAreaItem.updateInProgress()>
      <@fdsTag.tag tagClass="govuk-tag--blue">Update in progress</@fdsTag.tag>
    <#elseif workAreaItem.requestDueBy()?has_content>
      <@fdsTag.tag tagClass="govuk-tag--blue">Update due by ${workAreaItem.requestDueBy()}</@fdsTag.tag>
    <#elseif workAreaItem.outstandingInformationRequest()>
      <@fdsTag.tag tagClass="govuk-tag--blue">Further information requested</@fdsTag.tag>
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

<#macro statusFilter form statusCheckboxes>
  <@fdsSearch.searchFilterItem itemName="Status" expanded=form.getScapStatuses()?has_content>
    <@fdsSearch.searchCheckboxes
      path="form.scapStatuses"
      checkboxes=statusCheckboxes
    />
  </@fdsSearch.searchFilterItem>
</#macro>

<#macro updateStatusFilter form updateRequestRadios>
  <@fdsSearch.searchFilterItem itemName="Update status" expanded=form.getUpdateRequestStatusRadioOptions()?has_content>
    <@fdsSearch.searchRadio
      path="form.updateRequestStatusRadioOptions"
      radioItems=updateRequestRadios
    />
  </@fdsSearch.searchFilterItem>
</#macro>

<#macro referenceFilter form>
  <@fdsSearch.searchFilterItem itemName="SCAP reference" expanded=form.referenceSearchTerm?has_content>
    <@fdsSearch.searchTextInput
      path="form.referenceSearchTerm"
      labelText=""
      suffixScreenReaderPrompt="SCAP reference"
    />
  </@fdsSearch.searchFilterItem>
</#macro>

<#macro operatorFilter form preselectedOperator>
<#-- @ftlvariable name="preselectedOperator" type="uk.co.nstauthority.scap.fds.searchselector.RestSearchItem" -->
  <@fdsSearch.searchFilterItem itemName="Operator" expanded=prefilledOperator.id()?has_content>
    <@fdsSearchSelector.searchSelectorRest
      path="form.operatorId"
      restUrl=springUrl(organisationGroupSearchUrl)
      labelText="Operator name"
      labelClass="govuk-visually-hidden"
      selectorMinInputLength=2
      preselectedItems={prefilledOperator.id(): prefilledOperator.text()}
    />
  </@fdsSearch.searchFilterItem>
</#macro>

<#macro fieldFilter form preselectedField>
<#-- @ftlvariable name="preselectedField" type="uk.co.nstauthority.scap.fds.searchselector.RestSearchItem" -->
  <@fdsSearch.searchFilterItem itemName="Field" expanded=form.getFieldId()?has_content>
    <@fdsSearchSelector.searchSelectorRest
      path="form.fieldId"
      restUrl=springUrl(fieldSearchUrl)
      labelText="Associated field"
      labelClass="govuk-visually-hidden"
      selectorMinInputLength=3
      preselectedItems={prefilledField.id() : prefilledField.text()}
    />
  </@fdsSearch.searchFilterItem>
</#macro>

<#macro projectTypeFilter form projectTypeCheckboxes>
  <@fdsSearch.searchFilterItem itemName="Project types" expanded=form.getProjectTypes()?has_content>
      <@fdsSearch.searchCheckboxes path="form.projectTypes" checkboxes=projectTypeCheckboxes />
  </@fdsSearch.searchFilterItem>
</#macro>
