<#include '../../../layout/layout.ftl'>
<@fdsSlideOutPanel.slideOutPanel panelId="Reinstate-scap-panel" headingText="Reinstate SCAP">
  <h2 class="govuk-heading-s">
    Are you sure you want to Reinstate this SCAP?
  </h2>
  <@fdsForm.htmlForm actionUrl=springUrl(reinstateScapUrl)>
      <@fdsTextarea.textarea
      path="scapReinstateForm.reinstateComments.inputValue"
      labelText="Reasons for reinstating SCAP"/>
      <@fdsAction.button buttonText="reinstate SCAP" buttonName="REINSTATE" buttonClass="govuk-button"/>
  </@fdsForm.htmlForm>
</@fdsSlideOutPanel.slideOutPanel>