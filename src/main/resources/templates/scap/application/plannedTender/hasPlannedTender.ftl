<#include '../../layout/layout.ftl'>

<#assign pageTitle = "Does this SCAP have any planned tender activities?" />

<@defaultPage
htmlTitle=pageTitle
pageHeading=""
pageSize=PageSize.TWO_THIRDS_COLUMN
errorItems=errorItems
backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsForm.htmlForm actionUrl=springUrl(submitPostUrl)>
    <@fdsRadio.radioGroup
      path="form.hasPlannedTender"
      labelText=pageTitle
      fieldsetHeadingClass="govuk-fieldset__legend--xl"
      fieldsetHeadingSize="h1"
    >
      <#assign firstItem=true/>
      <#list hasPlannedTender as key, value>
        <@fdsRadio.radioItem path="form.hasPlannedTender" itemMap={key : value} isFirstItem=firstItem />
        <#assign firstItem=false/>
      </#list>
    </@fdsRadio.radioGroup>
    <@fdsAction.submitButtons
      primaryButtonText="Save and continue"
      secondaryButtonText="Cancel"
      secondaryLinkText=springUrl(backLinkUrl)/>
  </@fdsForm.htmlForm>
</@defaultPage>
