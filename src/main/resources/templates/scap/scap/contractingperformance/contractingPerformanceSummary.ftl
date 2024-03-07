<#include '../../layout/layout.ftl'>
<#import 'contractingPerformanceSummaryContent.ftl' as summaryContent>

<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->
<#-- @ftlvariable name="summaryViews" type="java.util.List<uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryView>"-->

<#assign pageTitle = "Contracting performance" />

<@defaultPage
htmlTitle=pageTitle
pageHeading=pageTitle
pageSize=PageSize.TWO_THIRDS_COLUMN
errorItems=errorList
backLinkUrl=springUrl(backLinkUrl)
>
  <#list summaryViews as summaryView>
    <@contractingPerformanceSummaryCard summaryView=summaryView index=summaryView_index/>
  </#list>

  <@fdsForm.htmlForm>
    <@fdsRadio.radio
      path="form.hasMoreContractingPerformance"
      radioItems=radioItems
      labelText="Do you want to add further contracting performance?"
    />

    <@fdsAction.button buttonText="Save and complete"/>
  </@fdsForm.htmlForm>

</@defaultPage>

<#macro contractingPerformanceSummaryCard summaryView index>
<#-- @ftlvariable name="summaryView" type="uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryView"-->
  <#assign cardActionsContent>
    <@contractingPerformanceSummaryCardActionsContent summaryView=summaryView />
  </#assign>
  <@fdsSummaryList.summaryListCard
    summaryListId="contracting-performance-summary-${index}"
    headingText=summaryView.scopeTitle()
    cardActionsContent=cardActionsContent
  >
    <@summaryContent.contractingPerformanceSummaryContent summaryView=summaryView />
  </@fdsSummaryList.summaryListCard>
</#macro>

<#macro contractingPerformanceSummaryCardActionsContent summaryView>
<#-- @ftlvariable name="summaryView" type="uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryView"-->
  <@fdsSummaryList.summaryListCardActionList>
    <@fdsSummaryList.summaryListCardActionItem
      itemUrl=springUrl(summaryView.getChangeLinkUrl())
      itemText="Change"
      itemScreenReaderText="contracting performance for ${summaryView.scopeTitle()}"
    />
    <@fdsSummaryList.summaryListCardActionItem
      itemUrl=springUrl(summaryView.getDeleteLinkUrl())
      itemText="Delete"
      itemScreenReaderText="contracting performance for ${summaryView.scopeTitle()}"
    />
  </@fdsSummaryList.summaryListCardActionList>
</#macro>
