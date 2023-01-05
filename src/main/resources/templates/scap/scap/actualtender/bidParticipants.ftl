<#include '../../layout/layout.ftl'>

<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->
<#assign pageTitle = "Bid participants"/>

<@defaultPage
htmlTitle=pageTitle
pageHeading=""
pageSize=PageSize.TWO_THIRDS_COLUMN
errorItems=errorList
backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsForm.htmlForm>
    <@fdsCheckbox.checkboxes
      path="form.selectedBidParticipantIds"
      fieldsetHeadingClass="govuk-fieldset__legend--xl"
      checkboxes=bidParticipantCheckboxes
      fieldsetHeadingText=pageTitle
    />

    <@fdsAction.button buttonText="Save and continue"/>
  </@fdsForm.htmlForm>
</@defaultPage>