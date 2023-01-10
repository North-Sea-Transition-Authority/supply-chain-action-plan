<#import '../../../fds/components/timeline/timeline.ftl' as fdsTimeline>
<#import '../../../fds/components/dataItems/dataItems.ftl' as fdsDataItems>

<#-- @ftlvariable name="timelineEvents" type="java.util.List<uk.co.nstauthority.scap.scap.timeline.TimelineEventView>" -->

<#macro timeline timelineEvents>
  <@fdsTimeline.timeline>
    <@fdsTimeline.timelineSection>
      <#list timelineEvents as timelineEvent>
        <@timelineEventCards timelineEvent=timelineEvent/>
      </#list>
    </@fdsTimeline.timelineSection>
  </@fdsTimeline.timeline>
</#macro>

<#macro timelineEventCards timelineEvent>
    <@fdsTimeline.timelineTimeStamp timeStampHeading=timelineEvent.timelineEventSubject() nodeNumber="">
    <@fdsTimeline.timelineEvent>
      <@fdsDataItems.dataItem>
        <@fdsDataItems.dataValues key="Event date" value=timelineEvent.formattedTime()></@fdsDataItems.dataValues>
        <@fdsDataItems.dataValues key="Added by" value=timelineEvent.userDisplayName()></@fdsDataItems.dataValues>
        <@fdsDataItems.dataValues key="Application Version" value=timelineEvent.versionNumber()></@fdsDataItems.dataValues>
      </@fdsDataItems.dataItem>
    </@fdsTimeline.timelineEvent>
</@fdsTimeline.timelineTimeStamp>
</#macro>