<#include '../layout/layout.ftl'>

<#assign pageTitle = "Start new SCAP" />
<#assign scapGuidanceDocumentUrl = customerBranding.guidanceDocumentUrl()/>

<@defaultPage
htmlTitle=pageTitle
pageHeading=pageTitle
pageSize=PageSize.TWO_THIRDS_COLUMN
backLinkUrl=backLinkUrl
>
  <p class="govuk-body">The purpose of supply chain action plans (SCAPs) is to assist operators in demonstrating their
    contract strategies and concepts are comprehensive and well-positioned to deliver ‘best value’ in accordance with
    their Field Development Plan (FDP) or Decommissioning Programme (DP).</p>
  <p class="govuk-body">
    The ${customerBranding.mnemonic()} updated its SCAP guidance in August 2022 to ensure alignment with its Strategy and the North Sea Transition
    Deal. The full guidance document can be found at
      <@fdsAction.link linkText=scapGuidanceDocumentUrl linkUrl=scapGuidanceDocumentUrl/>
  </p>
  <@fdsAction.link linkText="Start now" start=true linkClass="govuk-!-margin-top-2" linkUrl=startScapRedirectUrl/>
</@defaultPage>
