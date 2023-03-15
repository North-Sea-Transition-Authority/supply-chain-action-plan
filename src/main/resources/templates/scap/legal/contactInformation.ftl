<#include '../layout/layout.ftl'>
<#import '../layout/serviceContact.ftl' as serviceContact/>

<#-- @ftlvariable name="technicalSupport" type="uk.co.nstauthority.scap.technicalsupport.TechnicalSupportConfigurationProperties" -->
<#-- @ftlvariable name="businessSupport" type="uk.co.nstauthority.scap.branding.BusinessSupportConfiguration" -->

<@defaultPage
  htmlTitle="Contact"
  pageHeading="Contact"
  pageSize=PageSize.FULL_WIDTH
  topNavigation=false
  backLink=true
  phaseBanner=false
>
  <h2 class="govuk-heading-m">${businessSupport.name()}</h2>
  <span class="govuk-hint">For example, questions about filling in your application, the information you need to provide or to provide feedback on the service</span>
  <@serviceContact.serviceContact serviceContactInfo=businessSupport />

  <h2 class="govuk-heading-m">${technicalSupport.name()}</h2>
  <span class="govuk-hint">For example, unexpected problems using the service or system errors being received</span>
  <@serviceContact.serviceContact serviceContactInfo=technicalSupport />
</@defaultPage>
