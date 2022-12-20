<#include '../../fds/layout.ftl'>
<#import '_pageSizes.ftl' as PageSize>
<#import '../scap/macros/notificationBanner.ftl' as notificationBanner>

<#-- @ftlvariable name="serviceBranding" type="uk.co.nstauthority.scap.branding.ServiceConfigurationProperties" -->
<#-- @ftlvariable name="customerBranding" type="uk.co.nstauthority.scap.branding.CustomerConfigurationProperties" -->
<#-- @ftlvariable name="serviceHomeUrl" type="String" -->
<#-- @ftlvariable name="notificationBannerView" type="uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerView" -->
<#-- @ftlvariable name="loggedInUser" type="uk.co.nstauthority.scap.authentication.ServiceUserDetail" -->

<#if notificationBannerView??>
  <#assign notificationBannerContent>
    <#if notificationBannerView.bannerType.getValue() == "notificationBannerSuccess">
        <@notificationBanner.successNotificationBanner notificationBannerView=notificationBannerView/>
    <#else>
        <@notificationBanner.infoNotificationBanner notificationBannerView=notificationBannerView/>
    </#if>
  </#assign>
</#if>

<#macro defaultPage
  htmlTitle
  pageHeading=""
  phaseBanner=true
  errorItems=[]
  pageSize=PageSize.TWO_THIRDS_COLUMN
  backLinkUrl=""
  caption=""
>
  <#local serviceName = serviceBranding.name() />
  <#local customerMnemonic = customerBranding.mnemonic() />
  <#local serviceHomeUrl = springUrl(serviceHomeUrl) />

  <#assign fullWidthColumn=false />
  <#assign oneHalfColumn=false />
  <#assign oneThirdColumn=false />
  <#assign twoThirdsColumn=false />
  <#assign twoThirdsOneThirdColumn=false />
  <#assign oneQuarterColumn=false />

  <#if pageSize == PageSize.FULL_WIDTH>
    <#assign fullWidthColumn=true/>
  <#elseif pageSize == PageSize.ONE_HALF_COLUMN>
    <#assign oneHalfColumn=true/>
  <#elseif pageSize == PageSize.ONE_THIRD_COLUMN>
    <#assign oneThirdColumn=true/>
  <#elseif pageSize == PageSize.TWO_THIRDS_ONE_THIRD_COLUMN>
    <#assign twoThirdsOneThirdColumn=true/>
  <#elseif pageSize == PageSize.ONE_QUARTER>
    <#assign oneQuarterColumn=true/>
  <#else>
    <#assign twoThirdsColumn=true/>
  </#if>

  <#assign headerContent>
      <@fdsHeader.header
      homePageUrl=serviceHomeUrl
      serviceUrl=serviceHomeUrl
      logoProductText=customerMnemonic
      headerNav=true
      serviceName=serviceName
      headerLogo="GOV_CREST"
      >
        <#if loggedInUser?has_content>
          <@fdsHeader.headerNavigation>
            <@fdsHeader.headerNavigationItem
            itemText=loggedInUser.displayName()
            itemActive=false
            />
            <@fdsHeader.headerNavigationSignOutButton formUrl=springUrl("/logout") buttonText="Sign out"/>
          </@fdsHeader.headerNavigation>
        </#if>
      </@fdsHeader.header>
  </#assign>

  <@fdsDefaultPageTemplate
    htmlTitle=htmlTitle
    serviceName=serviceName
    htmlAppTitle=serviceName
    pageHeading=pageHeading
    headerLogo="GOV_CREST"
    logoProductText=customerMnemonic
    topNavigation=true
    navigationItems=navigationItems
    phaseBanner=phaseBanner
    serviceUrl=serviceHomeUrl
    homePageUrl=serviceHomeUrl
    fullWidthColumn=fullWidthColumn
    oneHalfColumn=oneHalfColumn
    oneThirdColumn=oneThirdColumn
    twoThirdsColumn=twoThirdsColumn
    twoThirdsOneThirdColumn=twoThirdsOneThirdColumn
    oneQuarterColumn=oneQuarterColumn
    backLink=backLinkUrl?has_content
    backLinkUrl=backLinkUrl
    caption=caption
    errorItems=errorItems
    notificationBannerContent=notificationBannerContent
    headerContent=headerContent
  >
    <#nested />
  </@fdsDefaultPageTemplate>
</#macro>