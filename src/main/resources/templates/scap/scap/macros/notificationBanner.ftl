<#include '../../layout/layout.ftl'>

<#-- @ftlvariable name="notificationBannerView" type="uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerView" -->

<#macro successNotificationBanner notificationBannerView>

  <#if notificationBannerView.title?hasContent>
    <@fdsNotificationBanner.notificationBannerSuccess bannerTitleText=notificationBannerView.title>
      <@fdsNotificationBanner.notificationBannerContent>

        <#list notificationBannerView.bodyLines as bodyLine>
          <p class="${bodyLine.lineClass()!"govuk-body"}">${bodyLine.lineText()}</p>
        </#list>

      </@fdsNotificationBanner.notificationBannerContent>
    </@fdsNotificationBanner.notificationBannerSuccess>
  </#if>

</#macro>


<#macro infoNotificationBanner notificationBannerView>

  <#if notificationBannerView.title?hasContent>
    <@fdsNotificationBanner.notificationBannerInfo bannerTitleText=notificationBannerView.title>
      <@fdsNotificationBanner.notificationBannerContent>

        <#list notificationBannerView.bodyLines as bodyLine>
          <p class="${bodyLine.lineClass()!"govuk-body"}">${bodyLine.lineText()}</p>
        </#list>

      </@fdsNotificationBanner.notificationBannerContent>
    </@fdsNotificationBanner.notificationBannerInfo>
  </#if>

</#macro>
