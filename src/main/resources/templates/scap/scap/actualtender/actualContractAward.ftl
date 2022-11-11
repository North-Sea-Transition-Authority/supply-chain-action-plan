<#include '../../layout/layout.ftl'>

<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->
<#assign pageTitle = "Actual contract award"/>

<@defaultPage
htmlTitle=pageTitle
pageHeading=pageTitle
pageSize=PageSize.TWO_THIRDS_COLUMN
errorItems=errorList
backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsForm.htmlForm>
    <@fdsSelect.select
      path="form.preferredBidderId"
      options=bidParticipantsMap
      labelText="Preferred bidder"
    />
    <@fdsTextInput.textInput
      path="form.awardValue.inputValue"
      labelText="Award value"
      prefix="£"
      suffix="million"
      suffixScreenReaderPrompt="in millions"
      inputClass="govuk-input--width-7"
    />
    <@fdsTextarea.textarea
      path="form.awardRationale.inputValue"
      labelText="Award rationale"
    />
    <@fdsSearchSelector.searchSelectorRest
      path="form.preferredBidderLocation"
      restUrl=springUrl(countrySearchRestUrl)
      labelText="Location of preferred bidder"
      selectorMinInputLength=2
      preselectedItems=preselectedCountry!{}
    />

    <@fdsAction.button buttonText="Save and continue"/>
  </@fdsForm.htmlForm>
</@defaultPage>