<#include '../layout/layout.ftl'>

<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->

<#assign pageTitle = "Project performance and close-out" />
<#assign yesContent>
  <@fdsDateInput.dateInput
    formId="startDate"
    dayPath="form.startDay.inputValue"
    monthPath="form.startMonth.inputValue"
    yearPath="form.startYear.inputValue"
    labelText="Actual execution start date"
    hintText="For example, 31 3 2023"
    nestingPath="form.projectCompleted"
  />
  <@fdsDateInput.dateInput
    formId="endDate"
    dayPath="form.completionDay.inputValue"
    monthPath="form.completionMonth.inputValue"
    yearPath="form.completionYear.inputValue"
    labelText="Actual commissioning or completion date"
    hintText="For example, 31 3 2024"
    nestingPath="form.projectCompleted"
  />
  <@fdsTextInput.textInput
    path="form.outturnCost.inputValue"
    labelText="Project outturn cost"
    prefix="£"
    suffix="million"
    suffixScreenReaderPrompt="in millions"
    inputClass="govuk-input--width-7"
    nestingPath="form.projectCompleted"
  />
</#assign>

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=""
  pageSize=PageSize.TWO_THIRDS_COLUMN
  errorItems=errorList
  backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsForm.htmlForm>
    <@fdsRadio.radioGroup
      path="form.projectCompleted"
      labelText="Has the full project been completed?"
      fieldsetHeadingClass="govuk-fieldset__legend--xl"
      fieldsetHeadingSize="h1"
      hintText="For example, fully commissioned or fully decommissioned"
      hiddenContent=true
    >
      <@fdsRadio.radioYes path="form.projectCompleted">
        ${yesContent}
      </@fdsRadio.radioYes>
      <@fdsRadio.radioNo path="form.projectCompleted"/>
    </@fdsRadio.radioGroup>
    <@fdsAction.button buttonText="Save and complete"/>
  </@fdsForm.htmlForm>
</@defaultPage>
