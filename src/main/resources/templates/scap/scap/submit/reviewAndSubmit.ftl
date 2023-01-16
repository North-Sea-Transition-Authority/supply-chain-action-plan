<#include '../../layout/layout.ftl'>
<#import '../summary/scapSummary.ftl' as scapSummary>

<#assign pageTitle = "Check your answers before submitting your SCAP" />

<#-- @ftlvariable name="scapSummaryView" type="uk.co.nstauthority.scap.scap.summary.ScapSummaryView" -->

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
  backLinkUrl=springUrl(backLinkUrl)
  singleErrorMessage=incompleteErrorMessage
>
  <@scapSummary.summary scapSummaryView=scapSummaryView />

  <@fdsForm.htmlForm>
    <#if isValid>
      <@fdsAction.submitButtons
        primaryButtonText="Submit"
        linkSecondaryAction=true
        linkSecondaryActionUrl=springUrl(backLinkUrl)
        secondaryLinkText="Cancel"
      />
    </#if>
  </@fdsForm.htmlForm>
</@defaultPage>
