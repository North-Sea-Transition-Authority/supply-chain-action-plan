<#include '../../layout/layout.ftl'>

<#assign pageTitle = "Check your answers before submitting your SCAP" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
  backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsForm.htmlForm>
    <@fdsAction.submitButtons
      primaryButtonText="Submit"
      linkSecondaryAction=true
      linkSecondaryActionUrl=springUrl(backLinkUrl)
      secondaryLinkText="Cancel"
    />
  </@fdsForm.htmlForm>
</@defaultPage>
