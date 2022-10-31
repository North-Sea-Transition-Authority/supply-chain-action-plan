<#include '../layout/layout.ftl'>

<#-- @ftlvariable name="errorItems" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->

<#assign pageTitle = "Project details" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
  errorItems=errorItems
  backLinkUrl=springUrl(backLinkUrl)
  errorItems=errorItems
>
  <@fdsForm.htmlForm>
    <@fdsTextInput.textInput
      path="form.projectName.inputValue"
      labelText="Project name"
      inputClass="govuk-!-width-two-thirds"
    />
    <@fdsCheckbox.checkboxes
      fieldsetHeadingText="Project type"
      path="form.projectTypes"
      checkboxes=projectTypesMap
    />
    <@fdsTextInput.textInput
      labelText="Project cost estimate"
      path="form.projectCostEstimate.inputValue"
      prefix="£"
      suffix="million"
      inputClass="govuk-input--width-5"
    />
    <@fdsTextInput.textInput
      labelText="Estimated value local content"
      path="form.estimatedValueLocalContent.inputValue"
      prefix="£"
      suffix="million"
      inputClass="govuk-input--width-5"
    />
    <#assign localContentText>
      Local content is defined as a UK registered corporate entity employing a UK workforce, paying UK taxes.

      This includes companies who may have an overseas headquarters or Parent Company but are performing the contracted
      scope through a UK registered corporate entity with UK workforce, paying UK taxes.
    </#assign>
    <@fdsDetails.details
      detailsTitle="How is 'local content' defined?"
      detailsText=localContentText
    />
    <@fdsSearchSelector.searchSelectorRest
      path="form.fieldId.inputValue"
      restUrl=springUrl(fieldSearchRestUrl)
      labelText="Field"
      inputClass="govuk-!-width-one-half"
    />
    <@fdsDateInput.dateInput
      formId="startDate"
      dayPath="form.startDay.inputValue"
      monthPath="form.startMonth.inputValue"
      yearPath="form.startYear.inputValue"
      labelText="Indicative planned execution start date"
      hintText="For example, 31 3 2023"
    />
    <@fdsDateInput.dateInput
      formId="endDate"
      dayPath="form.endDay.inputValue"
      monthPath="form.endMonth.inputValue"
      yearPath="form.endYear.inputValue"
      labelText="Indicative planned commissioning or completion date"
      hintText="For example, 31 3 2024"
      />
    <@fdsAction.button buttonText="Save and continue"/>
  </@fdsForm.htmlForm>
</@defaultPage>
