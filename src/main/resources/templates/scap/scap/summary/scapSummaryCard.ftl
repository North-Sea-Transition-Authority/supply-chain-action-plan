<#include '../../layout/layout.ftl'>

<#macro summaryCard>
  <@fdsDataItems.dataItem>
    <@fdsDataItems.dataValues key="Project name"  value=projectName!''></@fdsDataItems.dataValues>
    <@fdsDataItems.dataValues key="SCAP status"  value=scapStatus></@fdsDataItems.dataValues>
    <@fdsDataItems.dataValues key="SCAP submission status"  value=scapSubmissionStatus></@fdsDataItems.dataValues>
  </@fdsDataItems.dataItem>
  <@fdsDataItems.dataItem>
    <@fdsDataItems.dataValues key="Operator" value=operator!''></@fdsDataItems.dataValues>
    <@fdsDataItems.dataValues key=""  value=""></@fdsDataItems.dataValues>
  </@fdsDataItems.dataItem>
</#macro>