<#include '../../layout/layout.ftl'>

<#-- @ftlvariable name="errorItems" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->

<#assign pageTitle = "Have any of the awarded contracts been fully closed out?" />

<@defaultPage
htmlTitle=pageTitle
pageHeading=""
pageSize=PageSize.TWO_THIRDS_COLUMN
errorItems=errorItems
backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsForm.htmlForm>
    <@fdsRadio.radio
      path="form.hasContractingPerformance"
      radioItems=radioItems
      labelText=pageTitle
      fieldsetHeadingClass="govuk-fieldset__legend--xl"
      fieldsetHeadingSize="h1"
    />
    <@fdsAction.button buttonText="Save and continue"/>
  </@fdsForm.htmlForm>
</@defaultPage>
