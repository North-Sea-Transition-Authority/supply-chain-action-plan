<#include '../layout/layout.ftl'>

<#-- @ftlvariable name="technicalSupport" type="uk.co.nstauthority.scap.technicalsupport.TechnicalSupportConfigurationProperties" -->
<#-- @ftlvariable name="serviceBranding" type="uk.co.nstauthority.scap.branding.ServiceConfigurationProperties" -->

<#assign pageTitle = "Page not found" />

<@defaultPage
htmlTitle=pageTitle
pageHeading=pageTitle
phaseBanner=false
>
  <p class="govuk-body">
    If you typed the web address, check it is correct.
  </p>
  <p class="govuk-body">
    If you pasted the web address, check you copied the entire address.
  </p>
  <p class="govuk-body">
    If the web address is correct or you selected a link or button, contact the service desk using the details below:
  </p>
  <ul class="govuk-list">
    <li>${technicalSupport.name()}</li>
    <li>${technicalSupport.phoneNumber()}</li>
    <li>
        <@fdsAction.link
        linkText=technicalSupport.emailAddress()
        linkUrl="mailto:${technicalSupport.emailAddress()}?subject=${serviceBranding.name()} - Page Not Found"
        />
    </li>
  </ul>
</@defaultPage>