<#include '../../layout/layout.ftl'>

<#macro noObjectionGuidance>
  <@fdsDetails.summaryDetails summaryTitle="What does 'no objection' mean?">
    <p class="govuk-body">
      When all evaluation criteria are met, the SCAP will receive a 'no objection' from the ${customerMnemonic}. For the avoidance of
      doubt a ‘no objection’ does not constitute endorsement of any other required submissions, out with the SCAP, made
      to the ${customerMnemonic}.
    </p>
    <p class="govuk-body">
      Full guidance can be found in the <a href="https://www.nstauthority.co.uk/news-publications/supply-chain-action-plan-scap-guidance/" class="govuk-link" target="_blank">${customerMnemonic} website</a>
    </p>
  </@fdsDetails.summaryDetails>
</#macro>