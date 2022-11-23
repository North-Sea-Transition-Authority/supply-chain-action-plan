<#include '../../layout/layout.ftl'>

<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->

<#assign pageTitle = "Contracting performance" />

<@defaultPage
htmlTitle=pageTitle
pageHeading=pageTitle
pageSize=PageSize.TWO_THIRDS_COLUMN
errorItems=errorList
backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsForm.htmlForm>
    <@fdsSelect.select
      path="form.actualTenderActivityId"
      labelText="Scope title"
      options=scopeTitlesMap
    />
    <@fdsTextInput.textInput
      path="form.outturnCost.inputValue"
      labelText="Outturn cost"
      prefix="£"
      suffix="million"
      suffixScreenReaderPrompt="in millions"
      inputClass="govuk-input--width-7"
    />
    <@fdsTextarea.textarea
      path="form.outturnRationale.inputValue"
      labelText="Provide a brief rationale if the outturn was greater than the award value"
      optionalLabel=true
    />

    <@fdsAction.button buttonText="Save and continue"/>

  </@fdsForm.htmlForm>
</@defaultPage>
