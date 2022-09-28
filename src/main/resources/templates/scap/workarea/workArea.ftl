<#include '../layout/layout.ftl'>

<#assign pageTitle = "Work area" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
>
  <@fdsAction.link linkText="Start new SCAP" linkClass="govuk-button" linkUrl=springUrl(startScapUrl)/>
</@defaultPage>
