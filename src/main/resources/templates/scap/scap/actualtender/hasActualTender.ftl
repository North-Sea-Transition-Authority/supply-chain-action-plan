<#include '../../layout/layout.ftl'>

<#assign pageTitle = "Does this SCAP have any actual tendering activity?" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=""
  pageSize=PageSize.TWO_THIRDS_COLUMN
  errorItems=errorItems
  backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsForm.htmlForm>
    <@fdsRadio.radioGroup
      path="form.hasActualTender"
      labelText=pageTitle
      hintText="Including any stage between request for information and the contract being awarded"
      fieldsetHeadingClass="govuk-fieldset__legend--xl"
      fieldsetHeadingSize="h1"
    >
      <#assign firstItem=true/>
      <#list hasActualTender as key, value>
        <@fdsRadio.radioItem path="form.hasActualTender" itemMap={key : value} isFirstItem=firstItem />
        <#assign firstItem=false/>
      </#list>
    </@fdsRadio.radioGroup>
    <@fdsAction.button buttonText="Save and continue"/>
  </@fdsForm.htmlForm>
</@defaultPage>
