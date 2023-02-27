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
      path="form.preferredBidderCountryId"
      restUrl=springUrl(countrySearchRestUrl)
      labelText="Location of preferred bidder"
      selectorMinInputLength=2
      preselectedItems=preselectedCountry!{}
    />

    <@fdsDateInput.dateInput
      formId="actual-contract-award-date"
      labelText="Contract award date"
      dayPath="form.contractAwardDate.dayInput.inputValue"
      monthPath="form.contractAwardDate.monthInput.inputValue"
      yearPath="form.contractAwardDate.yearInput.inputValue"
    />

    <@fdsRadio.radioGroup
      path="form.paymentTermsRadio"
      labelText="What are the payment terms for this contract?"
      hiddenContent=true
    >
      <#list paymentTermsRadioOptions as value, displayName>
        <@fdsRadio.radioItem
          path="form.paymentTermsRadio"
          itemMap={value: displayName}
        >
          <#if value == "OTHER">
            <@fdsTextInput.textInput
              path="form.otherPaymentTerm.inputValue"
              labelText="Provide the days for the payment terms"
              suffix="days"
              inputClass="govuk-input--width-3"
              nestingPath="form.paymentTermsRadio"
            />
          </#if>
        </@fdsRadio.radioItem>
      </#list>
    </@fdsRadio.radioGroup>

    <@fdsAction.button buttonText="Save and continue"/>
  </@fdsForm.htmlForm>
</@defaultPage>