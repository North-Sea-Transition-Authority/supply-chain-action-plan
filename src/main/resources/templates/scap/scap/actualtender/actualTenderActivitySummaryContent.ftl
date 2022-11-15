<#include '../../layout/layout.ftl'>

<#macro actualTenderActivitySummaryContent actualTenderActivity>
<#-- @ftlvariable name="actualTenderActivity"
  type="uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryView"
-->
  <@fdsSummaryList.summaryListRowNoAction keyText="Scope title">
    ${actualTenderActivity.scopeTitle()!""}
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Scope description">
    ${actualTenderActivity.scopeDescription()!""}
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Remuneration model">
    <#if actualTenderActivity.remunerationModel()?has_content>
      ${actualTenderActivity.remunerationModel().displayName!""}
    </#if>
  </@fdsSummaryList.summaryListRowNoAction>
  <#if actualTenderActivity.remunerationModel()?has_content && actualTenderActivity.remunerationModel().enumName == "OTHER">
    <@fdsSummaryList.summaryListRowNoAction keyText="Provide the remuneration model">
      ${actualTenderActivity.remunerationModelName()!""}
    </@fdsSummaryList.summaryListRowNoAction>
  </#if>
  <@fdsSummaryList.summaryListRowNoAction keyText="Contract stage">
    <#if actualTenderActivity.contractStage()?has_content>
      ${actualTenderActivity.contractStage().displayName!""}
    </#if>
  </@fdsSummaryList.summaryListRowNoAction>
  <#list actualTenderActivity.invitationToTenderParticipants() as invitationToTenderParticipant>
    <@fdsSummaryList.summaryListRowNoAction keyText="Invitation to tender participant ${invitationToTenderParticipant_index + 1}">
      ${invitationToTenderParticipant!""}
    </@fdsSummaryList.summaryListRowNoAction>
  </#list>
  <#list actualTenderActivity.bidParticipants() as bidParticipant>
    <@fdsSummaryList.summaryListRowNoAction keyText="Bid participant ${bidParticipant_index + 1}">
      ${bidParticipant}
    </@fdsSummaryList.summaryListRowNoAction>
  </#list>
  <#if actualTenderActivity.awardedContract()?has_content>
    <@awardedContractSummaryContent awardedContract=actualTenderActivity.awardedContract()/>
  </#if>

</#macro>

<#macro awardedContractSummaryContent awardedContract>
<#-- @ftlvariable name="awardedContract"
  type="uk.co.nstauthority.scap.scap.actualtender.summary.AwardedContractSummaryView"
-->
  <@fdsSummaryList.summaryListRowNoAction keyText="Preferred bidder">
    ${awardedContract.preferredBidder()!""}
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Award value">
    <#if awardedContract.awardValue()?has_content>
      £${awardedContract.awardValue()!""} million
    </#if>
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Award rationale">
    ${awardedContract.awardRationale()!""}
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Preferred bidder location">
    ${awardedContract.preferredBidderLocation()!""}
  </@fdsSummaryList.summaryListRowNoAction>
</#macro>
