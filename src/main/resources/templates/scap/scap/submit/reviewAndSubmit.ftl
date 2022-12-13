<#include '../../layout/layout.ftl'>
<#import 'projectDetailsSummary.ftl' as projectDetailsSummary>

<#assign pageTitle = "Check your answers before submitting your SCAP" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
  backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsAccordion.accordion accordionId="scap-summary">
    <@fdsAccordion.accordionSection sectionHeading="Project details">
      <@projectDetailsSummary.projectDetailsSummary projectDetailsView=projectDetailsView />
    </@fdsAccordion.accordionSection>
  </@fdsAccordion.accordion>

  <@fdsForm.htmlForm>
    <@fdsAction.submitButtons
      primaryButtonText="Submit"
      linkSecondaryAction=true
      linkSecondaryActionUrl=springUrl(backLinkUrl)
      secondaryLinkText="Cancel"
    />
  </@fdsForm.htmlForm>
</@defaultPage>
