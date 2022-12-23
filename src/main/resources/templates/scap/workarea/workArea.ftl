<#include '../layout/layout.ftl'>

<#-- @ftlvariable name="canStartScap" type="java.lang.Boolean" -->

<#assign pageTitle = "Work area" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
>
  <#if canStartScap>
    <@fdsAction.link linkText="Start new SCAP" linkClass="govuk-button" linkUrl=springUrl(startScapUrl)/>
  </#if>
</@defaultPage>
