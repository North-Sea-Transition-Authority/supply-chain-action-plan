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
    <@fdsDetails.summaryDetails summaryTitle="The scope title I want to provide contracting performance for is not shown">
      <p class="govuk-body">
        If your scope is not shown then you must add it as an actual tendering activity at "Contract has been awarded" stage.
      </p>
      <p class="govuk-body">
        You can only provide a single contracting performance for each actual tendering activity.
      </p>
      <p class="govuk-body">
        If you have split a tender into multiple contract awards, then you will need to provide an actual tender activity for each award individually.
      </p>
    </@fdsDetails.summaryDetails>
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
