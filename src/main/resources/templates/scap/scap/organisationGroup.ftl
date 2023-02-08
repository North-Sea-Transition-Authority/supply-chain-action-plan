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
      path="form.organisationGroupId.inputValue"
      labelText=pageTitle
      options=permittedOrganisationGroups
    />

    <@fdsDetails.summaryDetails summaryTitle="The operator I want to create a SCAP for is not listed">
      If the operator you want to create a SCAP for is not shown in the list then you must contact the access manager for the operator to provide you with access to their organisation.
    </@fdsDetails.summaryDetails>
    <@fdsAction.button buttonText="Save and continue"/>
  </@fdsForm.htmlForm>

</@defaultPage>
