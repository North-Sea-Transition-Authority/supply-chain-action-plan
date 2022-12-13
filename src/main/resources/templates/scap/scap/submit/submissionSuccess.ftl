<#include '../../layout/layout.ftl'>

<#assign pageTitle = "SCAP submitted" />

<#-- @ftlvariable name="customerBranding" type="uk.co.nstauthority.scap.branding.CustomerConfigurationProperties" -->

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=""
  pageSize=PageSize.TWO_THIRDS_COLUMN
>
  <@fdsForm.htmlForm>
    <@fdsPanel.panel
      panelTitle="SCAP submitted"
      panelText="Your reference number"
      panelRef=scapReference
    />

    <h2 class="govuk-heading-m">What happens next</h2>
    <p class="govuk-body">We've sent your application to the ${customerBranding.name()} (${customerBranding.mnemonic()})</p>
    <p class="govuk-body">They will contact you either to confirm if approved, or to ask for more information.</p>

    <@fdsAction.link
      linkText="Back to work area"
      linkUrl=springUrl(workAreaUrl)
    />
  </@fdsForm.htmlForm>
</@defaultPage>
