<#include '../layout/layout.ftl'>
<#include 'errorReference.ftl'>

<#-- @ftlvariable name="errorRef" type="String" -->
<#-- @ftlvariable name="technicalSupport" type="uk.co.nstauthority.scap.technicalsupport.TechnicalSupportConfigurationProperties" -->
<#-- @ftlvariable name="serviceBranding" type="uk.co.nstauthority.scap.branding.ServiceConfigurationProperties" -->

<#assign pageTitle = "Sorry, there is a problem with the service" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  phaseBanner=false
  topNavigation=false
>
  <p class="govuk-body">Try again later.</p>
  <p class="govuk-body">
    If you continue to experience this problem, contact the service desk using the
    details below. Be sure to include the error reference below in any correspondence.
  </p>
    <@errorReference reference=errorRef!>
      <ul class="govuk-list">
        <li>${technicalSupport.name()}</li>
        <li>${technicalSupport.phoneNumber()}</li>
        <li>
            <@fdsAction.link
            linkText=technicalSupport.emailAddress()
            linkUrl="mailto:${technicalSupport.emailAddress()}?subject=${serviceBranding.name()} - Error reference ${errorRef}"
            />
        </li>
      </ul>
    </@errorReference>
</@defaultPage>