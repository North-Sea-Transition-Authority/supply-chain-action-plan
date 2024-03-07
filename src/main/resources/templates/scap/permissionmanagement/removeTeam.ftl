<#include "../layout/layout.ftl">

<#-- @ftlvariable name="pageTitle" type="java.lang.String" -->
<#-- @ftlvariable name="backLinkUrl" type="java.lang.String" -->
<#-- @ftlvariable name="removeUrl" type="java.lang.String" -->
<#-- @ftlvariable name="team" type="uk.co.nstauthority.scap.permissionmanagement.Team" -->

<@defaultPage
    htmlTitle="Archive team"
    pageHeading=pageTitle
    pageSize=PageSize.TWO_THIRDS_COLUMN
    backLinkUrl=springUrl(backLinkUrl)
    topNavigation=true
>
    <@fdsSummaryList.summaryListWrapper headingText="${team.getDisplayName()}" summaryListId="summary-list-one">
      <@fdsSummaryList.summaryList>
        <@fdsSummaryList.summaryListRowNoAction keyText="Team Type">${team.getTeamType().getDisplayText()}</@fdsSummaryList.summaryListRowNoAction>
      </@fdsSummaryList.summaryList>
    </@fdsSummaryList.summaryListWrapper>


    <@fdsForm.htmlForm actionUrl=springUrl(removeUrl)>
      <@fdsAction.buttonGroup>
        <@fdsAction.button buttonText="Remove" buttonClass="govuk-button govuk-button--warning"/>
        <@fdsAction.link linkText="Cancel" linkUrl=springUrl(backLinkUrl) linkClass="fds-link-button"/>
      </@fdsAction.buttonGroup>
    </@fdsForm.htmlForm>
</@defaultPage>
