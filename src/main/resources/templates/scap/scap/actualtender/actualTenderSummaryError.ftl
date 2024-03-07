<#function actualTenderSummaryErrorMessage actualTenderActivity>
<#-- @ftlvariable name="actualTenderActivity"
  type="uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderActivitySummaryView"
-->
  <#return actualTenderActivity.isValid()?then(
      "", "There are problems with this actual tender activity. Change the activity to fix the problems or remove it."
  ) />
</#function>