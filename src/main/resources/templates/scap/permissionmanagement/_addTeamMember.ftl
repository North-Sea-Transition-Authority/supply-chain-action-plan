<#include "../layout/layout.ftl">

<#macro addTeamMember email registrationUrl cancelUrl>
  <@fdsForm.htmlForm>

    <@fdsTextInput.textInput
      path="${email}.inputValue"
      labelText="What is the UK Energy Portal email of the user?"
      pageHeading=true
    />

    <@fdsDetails.summaryDetails summaryTitle="The user I want to add does not have an account">
      <p class="govuk-body">
        The user must have an account on the UK Energy Portal in order to be added to the team.
      </p>
      <p class="govuk-body">
        A user can register for an account on the UK Energy Portal using the following link:
      </p>
      <p class="govuk-body">
        <@fdsAction.link linkText=registrationUrl linkUrl=registrationUrl openInNewTab=true/>
      </p>
    </@fdsDetails.summaryDetails>

    <@fdsAction.submitButtons
      primaryButtonText="Continue"
      secondaryLinkText="Cancel"
      linkSecondaryAction=true
      linkSecondaryActionUrl=cancelUrl
    />
  </@fdsForm.htmlForm>
</#macro>
