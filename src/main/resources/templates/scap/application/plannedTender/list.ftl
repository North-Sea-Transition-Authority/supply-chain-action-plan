<#include '../../layout/layout.ftl'>

<#assign pageTitle = "Planned tender activity" />

<@defaultPage
htmlTitle=pageTitle
pageHeading=pageTitle
pageSize=PageSize.TWO_THIRDS_COLUMN
backLinkUrl=springUrl(backLinkUrl)
>
  <#list plannedTenderDetailsList as plannedTenderDetail>
    <@plannedTenderListItem plannedTender=plannedTenderDetail/>
  </#list>
</@defaultPage>


<#macro plannedTenderListItem plannedTender>

</#macro>
