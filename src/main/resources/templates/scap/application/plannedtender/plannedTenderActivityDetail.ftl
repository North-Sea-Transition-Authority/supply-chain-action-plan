<#include '../../layout/layout.ftl'>

<#assign pageTitle = "Planned tender activity"/>

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
  errorItems=errorItems
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
      inputClass="govuk-!-width-one-third"/>

      <@fdsRadio.radioGroup path="form.remunerationModel" labelText="Remuneration model" hiddenContent=true>
        <#assign firstItem=true/>
        <#list remunerationModels as key, value>
          <@fdsRadio.radioItem path="form.remunerationModel" itemMap={key : value} isFirstItem=firstItem>
            <#if key == "OTHER">
              <@fdsTextInput.textInput
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

      <@fdsAction.button buttonText="Save and continue"/>
  </@fdsForm.htmlForm>
</@defaultPage>
