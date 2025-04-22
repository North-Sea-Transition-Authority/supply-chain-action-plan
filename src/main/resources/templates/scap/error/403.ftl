<#include '../layout/layout.ftl'>
<#include 'errorReference.ftl'>

<#assign pageTitle = "You do not have permission to view this page" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  phaseBanner=false
  topNavigation=false
>
  <p class="govuk-body">
    To be provided with access speak with the person responsible for managing access permissions in your organisation.
  </p>
</@defaultPage>