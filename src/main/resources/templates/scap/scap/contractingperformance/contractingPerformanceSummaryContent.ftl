<#include '../../layout/layout.ftl'>

<#macro contractingPerformanceSummaryContent summaryView countryMap>
  <#-- @ftlvariable name="summaryView" type="uk.co.nstauthority.scap.scap.contractingperformance.summary.ContractingPerformanceSummaryView"-->
  <#-- @ftlvariable name="countryMap" type="java.util.Map<String, String>"-->

  <@simpleSummaryRow keyText="Scope description" valueText=summaryView.scopeDescription() />
  <@simpleSummaryRow keyText="Award value" valueText="£${summaryView.awardValue()} million" />
  <@simpleSummaryRow keyText="Remuneration model" valueText=summaryView.remunerationModel().displayName />
  <#if summaryView.remunerationModelName()?has_content>
    <@simpleSummaryRow keyText="Provide the remuneration model" valueText=summaryView.remunerationModelName() />
  </#if>
  <@simpleSummaryRow keyText="Contractor" valueText=summaryView.contractor() />
  <#assign countryKey = summaryView.countryId()?c />
  <@simpleSummaryRow keyText="Location" valueText=countryMap[countryKey]! />
  <@simpleSummaryRow keyText="Outturn cost" valueText="£${summaryView.outturnCost()} million" />
  <#if summaryView.outturnRationale()?has_content>
    <@simpleSummaryRow
      keyText="Rationale for outturn cost being greater than the award value"
      valueText=summaryView.outturnRationale()
    />
  </#if>
</#macro>


<#macro simpleSummaryRow keyText valueText>
  <@fdsSummaryList.summaryListRowNoAction keyText=keyText>
    ${valueText}
  </@fdsSummaryList.summaryListRowNoAction>
</#macro>
