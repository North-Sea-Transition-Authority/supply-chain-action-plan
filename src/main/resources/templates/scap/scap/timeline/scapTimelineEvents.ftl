<#include '../../layout/layout.ftl'>

<#-- @ftlvariable name="caseEvents" type="java.util.List<uk.co.nstauthority.scap.scap.casemanagement.CaseEventView>" -->

<#macro timeline caseEvents>
  <@fdsTimeline.timeline>
    <@fdsTimeline.timelineSection>
      <#list caseEvents as caseEvent>
        <@timelineEventCards caseEvent=caseEvent/>
      </#list>
      <@fdsTimeline.timelineTimeStamp timeStampHeading="" nodeNumber=""></@fdsTimeline.timelineTimeStamp>
    </@fdsTimeline.timelineSection>
  </@fdsTimeline.timeline>
</#macro>

<#macro timelineEventCards caseEvent>
  <@fdsTimeline.timelineTimeStamp timeStampHeading=caseEvent.caseEventSubject() nodeNumber="">
    <@fdsTimeline.timelineEvent>
      <@fdsDataItems.dataItem>
        <@fdsDataItems.dataValues key="Event date" value=caseEvent.formattedTime()></@fdsDataItems.dataValues>
        <@fdsDataItems.dataValues key="Added by" value=caseEvent.userDisplayName()></@fdsDataItems.dataValues>
        <@fdsDataItems.dataValues key="Application Version" value=caseEvent.versionNumber()></@fdsDataItems.dataValues>
      </@fdsDataItems.dataItem>
      <#if caseEvent.hasComments()>
        <@fdsDataItems.dataItem>
          <@fdsDataItems.dataValues key="Comments" value=caseEvent.comments()></@fdsDataItems.dataValues>
        </@fdsDataItems.dataItem>
      </#if>
      <#if caseEvent.hasSupportingDocument()>
        <@fdsDataItems.dataItem>
          <#assign fileUploadLink>
            <@fdsAction.link linkText=caseEvent.supportingDocument().fileName()!"" linkUrl=springUrl(caseEvent.supportingDocument().fileUrl())/>
          </#assign>
          <@fdsDataItems.dataValues key="Supporting document" value='${fileUploadLink}'></@fdsDataItems.dataValues>
          <@fdsDataItems.dataValues key="Document description" value=caseEvent.supportingDocument().fileDescription()!""></@fdsDataItems.dataValues>
          <@fdsDataItems.dataValues key="" value=''></@fdsDataItems.dataValues>
        </@fdsDataItems.dataItem>
      </#if>
    </@fdsTimeline.timelineEvent>
  </@fdsTimeline.timelineTimeStamp>
</#macro>
