<#include '../../layout/layout.ftl'>

<#macro actualTenderActivitySummaryContent actualTenderActivity>
<#-- @ftlvariable name="actualTenderActivity"
  type="uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderActivitySummaryView"
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
  <#assign listItemNum = 0 />
  <#list actualTenderActivity.ittParticipants() as invitationToTenderParticipant, notOnEnergyPortal>
    <@fdsSummaryList.summaryListRowNoAction keyText="Invitation to tender recipient ${listItemNum + 1}">
      ${invitationToTenderParticipant!""}
      <#if notOnEnergyPortal><@notOnEnergyPortalTag/></#if>
    </@fdsSummaryList.summaryListRowNoAction>
    <#assign listItemNum += 1 />
  </#list>
  <#assign listItemNum = 0 />
  <#list actualTenderActivity.bidParticipants() as bidParticipant, notOnEnergyPortal>
    <@fdsSummaryList.summaryListRowNoAction keyText="Bid participant ${listItemNum + 1}">
      ${bidParticipant}
    </@fdsSummaryList.summaryListRowNoAction>
    <#assign listItemNum += 1 />
  </#list>
  <#if actualTenderActivity.awardedContractSummaryView()?has_content>
    <@awardedContractSummaryContent awardedContract=actualTenderActivity.awardedContractSummaryView()/>
  </#if>

</#macro>

<#macro awardedContractSummaryContent awardedContract>
<#-- @ftlvariable name="awardedContract"
  type="uk.co.nstauthority.scap.scap.summary.actualtender.AwardedContractSummaryView"
-->
  <@fdsSummaryList.summaryListRowNoAction keyText="Preferred bidder">
    ${awardedContract.preferredBidderName()!""}
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
    ${awardedContract.preferredBidderCountry()!""}
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Contract award date">
    ${awardedContract.contractAwardDate()!""}
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Payment term">
    <#if awardedContract.paymentTerms()?has_content>
      ${awardedContract.paymentTerms()} days
    </#if>
  </@fdsSummaryList.summaryListRowNoAction>
</#macro>

<#macro notOnEnergyPortalTag>
  <strong class="govuk-tag">Not from portal</strong>
</#macro>
