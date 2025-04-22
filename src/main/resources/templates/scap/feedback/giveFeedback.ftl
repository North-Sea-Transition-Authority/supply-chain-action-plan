<#include '../layout/layout.ftl'>

<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->
<#-- @ftlvariable name="scapId" type="uk.co.nstauthority.scap.scap.scap.ScapId" -->

<#assign pageTitle = "Give feedback on ${serviceName}" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  phaseBanner=false
  errorItems=errorList
>
  <@fdsForm.htmlForm>
    <#if scapId?has_content>
      <input type="hidden" name="applicationId" value="${scapId.scapId()}"/>
    </#if>
    <@fdsRadio.radio
      path="form.satisfactionRating"
      labelText="Overall, how satisfied were you with this service?"
      radioItems=satisfactionRadioItems
    />
    <@fdsTextarea.textarea
      path="form.comments.inputValue"
      labelText="How could we improve this service?"
      optionalLabel=true
    />
    <@fdsAction.submitButtons
      primaryButtonText="Send feedback"
      linkSecondaryAction=true
      linkSecondaryActionUrl=springUrl(cancelUrl)
      secondaryLinkText="Cancel"
    />
  </@fdsForm.htmlForm>
</@defaultPage>
