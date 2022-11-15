<#include '../../layout/layout.ftl'>

<#assign pageTitle = "Actual tender activity"/>

<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->

<@defaultPage
htmlTitle=pageTitle
pageHeading=pageTitle
pageSize=PageSize.TWO_THIRDS_COLUMN
errorItems=errorList
backLinkUrl=springUrl(backLinkUrl)
>
  <#list actualTenderActivities as actualTenderActivity>
    <@actualTenderActivitySummary actualTenderActivity=actualTenderActivity index=actualTenderActivity_index/>
  </#list>
</@defaultPage>


<#macro actualTenderActivitySummary actualTenderActivity index>
<#-- @ftlvariable name="actualTenderActivity"
  type="uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryView"
-->
  <#assign cardActionsContent>
    <@fdsSummaryList.summaryListCardActionList>
      <@fdsSummaryList.summaryListCardActionItem
        itemUrl=actualTenderActivity.changeLinkUrl()
        itemText="Change"
        itemScreenReaderText="Change actual tendering activity ${index +1}"
      />
      <@fdsSummaryList.summaryListCardActionItem
        itemUrl=actualTenderActivity.deleteLinkUrl()
        itemText="Delete"
        itemScreenReaderText="Delete actual tendering activity ${index +1}"
      />
    </@fdsSummaryList.summaryListCardActionList>
  </#assign>

  <@fdsSummaryList.summaryListCard
    summaryListId="actual-tender-activity-${index}"
    headingText="Actual tender activity ${index + 1}"
    cardActionsContent=cardActionsContent
  >
    <@actualTenderActivitySummaryContent actualTenderActivity=actualTenderActivity/>
  </@fdsSummaryList.summaryListCard>
</#macro>

<#macro actualTenderActivitySummaryContent actualTenderActivity>
<#-- @ftlvariable name="actualTenderActivity"
  type="uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryView"
-->
  <@fdsSummaryList.summaryListRowNoAction keyText="Scope title">
    ${actualTenderActivity.scopeTitle()!"No scope title provided"}
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Scope description">
    ${actualTenderActivity.scopeDescription()!"No scope description provided"}
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Remuneration model">
    ${actualTenderActivity.remunerationModel().displayName!"No remuneration model provided"}
  </@fdsSummaryList.summaryListRowNoAction>
  <#if actualTenderActivity.remunerationModel().enumName == "OTHER">
    <@fdsSummaryList.summaryListRowNoAction keyText="Provide the remuneration model">
      ${actualTenderActivity.remunerationModelName()!"No remuneration model provided"}
    </@fdsSummaryList.summaryListRowNoAction>
  </#if>
  <@fdsSummaryList.summaryListRowNoAction keyText="Contract stage">
    ${actualTenderActivity.contractStage().displayName!"No contract stage provided"}
  </@fdsSummaryList.summaryListRowNoAction>
  <#list actualTenderActivity.invitationToTenderParticipants() as invitationToTenderParticipant>
    <@fdsSummaryList.summaryListRowNoAction keyText="Invitation to tender participant ${invitationToTenderParticipant_index + 1}">
      ${invitationToTenderParticipant}
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
    ${awardedContract.preferredBidder()!"No preferred bidder provided"}
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Award value">
    <#if awardedContract.awardValue()?has_content>
      £${awardedContract.awardValue()} million
    <#else>
      No award value provided
    </#if>
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Award rationale">
    ${awardedContract.awardRationale()!"No award rationale provided"}
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Preferred bidder location">
    ${awardedContract.preferredBidderLocation()!"No preferred bidder location provided"}
  </@fdsSummaryList.summaryListRowNoAction>
</#macro>
