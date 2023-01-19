<#include '../../layout/layout.ftl'>

<#macro contractingPerformanceSummaryContent summaryView>
<#-- @ftlvariable name="summaryView" type="uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryView"-->

  <@simpleSummaryRow keyText="Scope description" valueText=summaryView.scopeDescription()!"" />

  <#assign awardValue = (summaryView.awardValue()?has_content)?then("£${summaryView.awardValue()} million", "") >
  <@simpleSummaryRow keyText="Award value" valueText=awardValue />

  <@simpleSummaryRow keyText="Remuneration model" valueText=summaryView.remunerationModel().displayName />
  <#if summaryView.remunerationModelName()?has_content>
    <@simpleSummaryRow keyText="Provide the remuneration model" valueText=summaryView.remunerationModelName() />
  </#if>
  <@simpleSummaryRow keyText="Contractor" valueText=summaryView.contractor()!"" />
  <@simpleSummaryRow keyText="Location" valueText=summaryView.location()!"" />

  <#assign outturnCost = (summaryView.outturnCost()?has_content)?then("£${summaryView.outturnCost()} million", "") >
  <@simpleSummaryRow keyText="Outturn cost" valueText=outturnCost />

  <@simpleSummaryRow
    keyText="Rationale for outturn cost being greater than the award value"
    valueText=summaryView.outturnCostRationale()!""
  />
</#macro>


<#macro simpleSummaryRow keyText valueText>
  <@fdsSummaryList.summaryListRowNoAction keyText=keyText>
    ${valueText}
  </@fdsSummaryList.summaryListRowNoAction>
</#macro>
