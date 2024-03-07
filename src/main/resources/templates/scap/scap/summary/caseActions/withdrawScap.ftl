<#include '../../../layout/layout.ftl'>
<@fdsSlideOutPanel.slideOutPanel panelId="Withdraw-scap-panel" headingText="Withdraw SCAP">
  <h2 class="govuk-heading-s">
    Are you sure you want to withdraw this SCAP?
  </h2>
  <@fdsForm.htmlForm actionUrl=springUrl(WithdrawScapUrl)>
      <@fdsTextarea.textarea
      path="scapWithdrawForm.withdrawComments.inputValue"
      labelText="Reasons for withdrawal"/>
      <@fdsAction.button buttonText="Withdraw SCAP" buttonName="WITHDRAWN" buttonClass="govuk-button govuk-button--warning"/>
  </@fdsForm.htmlForm>
</@fdsSlideOutPanel.slideOutPanel>
