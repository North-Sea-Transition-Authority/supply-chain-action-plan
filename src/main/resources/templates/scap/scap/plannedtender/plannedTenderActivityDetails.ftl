<#include '../../layout/layout.ftl'>

<#assign pageTitle = "Planned tender activity"/>

<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
  errorItems=errorList
  backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsForm.htmlForm>
    <@fdsTextarea.textarea path="form.scopeDescription.inputValue" labelText="Scope description"/>

    <@fdsTextInput.textInput
      path="form.estimatedValue.inputValue"
      labelText="Estimated value"
      prefix="£"
      suffix="million"
      suffixScreenReaderPrompt="in millions"
      inputClass="govuk-input--width-7"
    />

      <@fdsRadio.radioGroup path="form.remunerationModel" labelText="Remuneration model" hiddenContent=true>
        <#assign firstItem=true/>
        <#list remunerationModels as key, value>
          <@fdsRadio.radioItem path="form.remunerationModel" itemMap={key : value} isFirstItem=firstItem>
            <#if key == "OTHER">
              <@fdsTextarea.textarea
                path="form.remunerationModelName.inputValue"
                labelText="Provide the remuneration model"
                nestingPath="form.remunerationModel"
              />
            </#if>
            <#assign firstItem=false/>
          </@fdsRadio.radioItem>
        </#list>
      </@fdsRadio.radioGroup>

      <@fdsTextarea.textarea path="form.awardRationale.inputValue" labelText="Award rationale"/>

      <@fdsDateInput.dateInput
        formId="indicative-actual-tender-start-date"
        labelText="Indicative actual tender start date"
        dayPath="form.indicativeActualTenderStartDate.dayInput.inputValue"
        monthPath="form.indicativeActualTenderStartDate.monthInput.inputValue"
        yearPath="form.indicativeActualTenderStartDate.yearInput.inputValue"
      />

      <@fdsDateInput.dateInput
        formId="indicative-contract-award-date"
        labelText="Indicative contract award date"
        dayPath="form.indicativeContractAwardDate.dayInput.inputValue"
        monthPath="form.indicativeContractAwardDate.monthInput.inputValue"
        yearPath="form.indicativeContractAwardDate.yearInput.inputValue"
      />

      <@fdsAction.button buttonText="Save and continue"/>
  </@fdsForm.htmlForm>
</@defaultPage>
