<#include '../layout/layout.ftl'>

<#-- @ftlvariable name="errorItems" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->

<#assign pageTitle = "Who is the operator for this SCAP?" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=""
  pageSize=PageSize.TWO_THIRDS_COLUMN
  errorItems=errorItems
  >
  <@fdsForm.htmlForm actionUrl=springUrl(submitPostUrl)>
    <@fdsSearchSelector.searchSelectorRest path="form.organisationGroupId.inputValue"
    restUrl=springUrl(organisationGroupSearchRestUrl)
    labelText=pageTitle
    pageHeading=true
    selectorMinInputLength=2
    preselectedItems=preselectedItems
    />

    <@fdsDetails.summaryDetails summaryTitle="The operator I want to create a SCAP for is not listed">
      If the operator you want to create a SCAP for is not shown in the list then you must contact the access manager for the operator to provide you with access to their organisation.
    </@fdsDetails.summaryDetails>
    <@fdsAction.submitButtons
    primaryButtonText="Save and continue"
    linkSecondaryAction=true
    linkSecondaryActionUrl=springUrl(backLinkUrl)
    secondaryLinkText="Cancel"/>
  </@fdsForm.htmlForm>

</@defaultPage>
