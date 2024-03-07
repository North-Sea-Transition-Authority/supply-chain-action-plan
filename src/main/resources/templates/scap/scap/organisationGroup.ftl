<#include '../layout/layout.ftl'>

<#-- @ftlvariable name="errorItems" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->

<#assign pageTitle = "Who is the operator for this SCAP?" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=""
  errorItems=errorItems
  backLinkUrl=springUrl(backLinkUrl)
  >
  <@fdsForm.htmlForm>
    <@fdsSelect.select
      path="form.organisationGroupId"
      labelText=pageTitle
      pageHeading=true
      options=permittedOrganisationGroups
    />

    <@fdsDetails.summaryDetails summaryTitle="The operator I want to create a SCAP for is not listed">
      If the operator you want to create a SCAP for is not shown in the list then you must contact the access manager for the operator to provide you with access to their organisation.
    </@fdsDetails.summaryDetails>

    <#assign tierOneContractorQuestionPath = "form.isTierOneContractor" />
    <@fdsRadio.radioGroup
      labelText="Are you submitting a SCAP on behalf of a Tier 1 Contractor?"
      hintText="Contractors who have been awarded a contract as part of an FDP(a)/DP/CCS Permit which exceeds a value of £25 million are required to submit a “Tier 1 Contractor SCAP”. "
      path=tierOneContractorQuestionPath
      hiddenContent=true
    >
      <@fdsRadio.radioYes path=tierOneContractorQuestionPath>
        <#assign preselectedItems = preselectedScap?has_content?then({preselectedScap.id(): preselectedScap.text()}, {}) />
        <@fdsSearchSelector.searchSelectorRest
          path="form.parentScapId"
          labelText="Parent SCAP reference"
          nestingPath=tierOneContractorQuestionPath
          restUrl=springUrl(scapSearchRestUrl)
          selectorMinInputLength=3
          preselectedItems=preselectedItems
        />
      </@fdsRadio.radioYes>
      <@fdsRadio.radioNo path=tierOneContractorQuestionPath />
    </@fdsRadio.radioGroup>

    <@fdsAction.button buttonText="Save and continue"/>
  </@fdsForm.htmlForm>

</@defaultPage>
