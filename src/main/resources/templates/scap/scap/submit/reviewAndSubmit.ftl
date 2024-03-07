<#include '../../layout/layout.ftl'>
<#import '../summary/scapSummary.ftl' as scapSummary>

<#assign pageTitle = "Check your answers before submitting your SCAP" />
<#assign customerMnemonic = customerBranding.mnemonic() />

<#-- @ftlvariable name="scapSummaryView" type="uk.co.nstauthority.scap.scap.summary.ScapSummaryView" -->
<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
  backLinkUrl=springUrl(backLinkUrl)
  singleErrorMessage=incompleteErrorMessage
  errorItems=errorList
>
  <#if updateText?has_content>
    <@fdsDetails.summaryDetails summaryTitle="What information has the ${customerMnemonic} asked to be updated?">
      <p class="govuk-body">${updateText}</p>
    </@fdsDetails.summaryDetails>
  </#if>
  <@scapSummary.summary scapSummaryView=scapSummaryView />

  <@fdsForm.htmlForm>
    <#if isValid>
      <@fdsCheckbox.checkbox
        fieldsetHeadingText="Acknowledgements"
        labelText="I confirm that this SCAP has been checked, reviewed and approved by all of our internal stakeholders"
        path="form.approvedByStakeholders"
      />

      <@fdsAction.submitButtons
        primaryButtonText="Submit"
        linkSecondaryAction=true
        linkSecondaryActionUrl=springUrl(backLinkUrl)
        secondaryLinkText="Cancel"
      />
    </#if>
  </@fdsForm.htmlForm>
</@defaultPage>
