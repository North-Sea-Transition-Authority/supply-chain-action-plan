<#-- @ftlvariable name="roles" type="java.util.List<uk.co.nstauthority.scap.teammangement.permissionmanagement.regulator.RegulatorTeamRole>" -->

<#import '../../fds/components/details/details.ftl' as fdsDetails>
<#import '../../fds/components/summaryList/summaryList.ftl' as fdsSummaryList>

<#macro roleDescriptions roles>
  <@fdsDetails.summaryDetails summaryTitle="What does each role allow a user to do?">
    <@fdsSummaryList.summaryList>
      <#list roles as role>
        <@fdsSummaryList.summaryListRowNoAction keyText="${role.getDisplayName()}">
          ${role.description}
        </@fdsSummaryList.summaryListRowNoAction>
      </#list>
    </@fdsSummaryList.summaryList>
  </@fdsDetails.summaryDetails>
</#macro>