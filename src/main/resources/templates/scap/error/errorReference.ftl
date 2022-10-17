<#include '../layout/layout.ftl'>

<#-- @ftlvariable name="stackTrace" type="String" -->

<#macro errorReference reference>
    <#if reference?has_content>
      <p class="govuk-body">
        Error reference: <span class="govuk-!-font-weight-bold">${reference}</span>
      </p>
        <#nested/>
        <#if stackTrace?has_content>
          <h2 class="govuk-heading-l">Stack trace</h2>
          <pre class="govuk-body">
<code>${stackTrace}</code>
          </pre>
        </#if>
    </#if>
</#macro>