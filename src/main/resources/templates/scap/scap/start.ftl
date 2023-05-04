<#include '../layout/layout.ftl'>

<#assign pageTitle = "Start new SCAP" />
<#assign scapGuidanceDocumentUrl = customerBranding.guidanceDocumentUrl()/>

<@defaultPage
htmlTitle=pageTitle
pageHeading=pageTitle
pageSize=PageSize.TWO_THIRDS_COLUMN
backLinkUrl=springUrl(backLinkUrl)
>
  <p class="govuk-body">The purpose of supply chain action plans (SCAPs) is to assist operators in demonstrating their
    contract strategies and concepts are comprehensive and well-positioned to deliver ‘best value’ in accordance with
    their Field Development Plan (FDP) or Decommissioning Programme (DP).</p>
  <p class="govuk-body">
    The ${customerBranding.mnemonic()} updated its SCAP guidance in August 2022 to ensure alignment with its Strategy and the North Sea Transition
    Deal.
  </p>
  <p class="govuk-body">
    Read
      <@fdsAction.link linkText='SCAPs' linkUrl=scapGuidanceDocumentUrl/>
    for more information
  </p>
  <@fdsAction.link
  linkText="Start now"
  start=true
  linkClass="govuk-!-margin-top-2"
  linkUrl=springUrl(startScapRedirectUrl)/>
</@defaultPage>
