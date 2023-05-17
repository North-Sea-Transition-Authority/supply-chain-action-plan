<#include '../../fds/layout.ftl'>
<#import '_pageSizes.ftl' as PageSize>
<#import '../scap/macros/notificationBanner.ftl' as notificationBanner>

<#-- @ftlvariable name="serviceBranding" type="uk.co.nstauthority.scap.branding.ServiceConfigurationProperties" -->
<#-- @ftlvariable name="customerBranding" type="uk.co.nstauthority.scap.branding.CustomerConfigurationProperties" -->
<#-- @ftlvariable name="serviceHomeUrl" type="String" -->
<#-- @ftlvariable name="notificationBannerView" type="uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerView" -->
<#-- @ftlvariable name="loggedInUser" type="uk.co.nstauthority.scap.authentication.ServiceUserDetail" -->
<#-- @ftlvariable name="accessibilityStatementUrl" type="String" -->
<#-- @ftlvariable name="contactUrl" type="String" -->
<#-- @ftlvariable name="privacyStatementUrl" type="String" -->
<#-- @ftlvariable name="cookiesStatementUrl" type="String" -->
<#-- @ftlvariable name="feedbackUrl" type="String" -->
<#-- @ftlvariable name="analytics" type=" uk.co.nstauthority.scap.configuration.AnalyticsProperties" -->

<#if notificationBannerView??>
  <#assign notificationBannerContent>
    <#if notificationBannerView.bannerType.getValue() == "notificationBannerSuccess">
        <@notificationBanner.successNotificationBanner notificationBannerView=notificationBannerView/>
    <#else>
        <@notificationBanner.infoNotificationBanner notificationBannerView=notificationBannerView/>
    </#if>
  </#assign>
</#if>

<#assign serviceName = serviceBranding.name() />
<#macro defaultPage
  htmlTitle
  pageHeading=""
  phaseBanner=true
  errorItems=[]
  pageSize=PageSize.TWO_THIRDS_COLUMN
  backLinkUrl=""
  backLink=false
  caption=""
  singleErrorMessage=""
  wrapperWidth=false
  topNavigation=false
>
  <#assign customerMnemonic = customerBranding.mnemonic() />
  <#assign serviceHomeUrl = springUrl(serviceHomeUrl) />

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
      wrapperWidth=wrapperWidth
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

  <#assign footerMetaContent>
    <@fdsFooter.footerMeta footerMetaHiddenHeading="Support links">
      <@fdsFooter.footerMetaLink linkText="Accessibility statement" linkUrl=springUrl(accessibilityStatementUrl)/>
      <@fdsFooter.footerMetaLink linkText="Contact" linkUrl=springUrl(contactUrl)/>
      <@fdsFooter.footerMetaLink linkText="Privacy" linkUrl=privacyStatementUrl/>
      <@fdsFooter.footerMetaLink linkText="Cookies" linkUrl=springUrl(cookiesStatementUrl)/>
      <@fdsFooter.footerMetaLink linkText="Feedback" linkUrl=springUrl(feedbackUrl)/>
    </@fdsFooter.footerMeta>
  </#assign>

  <#assign footerContent>
    <@fdsFooter.footer metaLinks=true footerMetaContent=footerMetaContent wrapperWidth=wrapperWidth/>
  </#assign>
  <#assign analyticsScript>
    <script src="<@spring.url'/assets/javascript/googleAnalyticsEventTracking.js'/>"></script>
  </#assign>

  <@fdsCookieBanner.analyticsCookieBanner
    serviceName=serviceBranding.name()
    cookieSettingsUrl=springUrl(cookiesStatementUrl)
  />

  <@fdsDefaultPageTemplate
    htmlTitle=htmlTitle
    serviceName=serviceName
    htmlAppTitle=serviceName
    pageHeading=pageHeading
    headerLogo="GOV_CREST"
    logoProductText=customerMnemonic
    topNavigation=topNavigation
    phaseBanner=phaseBanner
    phaseBannerLink=springUrl(feedbackUrl)
    serviceUrl=serviceHomeUrl
    homePageUrl=serviceHomeUrl
    fullWidthColumn=fullWidthColumn
    oneHalfColumn=oneHalfColumn
    oneThirdColumn=oneThirdColumn
    twoThirdsColumn=twoThirdsColumn
    twoThirdsOneThirdColumn=twoThirdsOneThirdColumn
    oneQuarterColumn=oneQuarterColumn
    backLink=backLinkUrl?has_content || backLink
    backLinkUrl=backLinkUrl
    caption=caption
    errorItems=errorItems
    notificationBannerContent=notificationBannerContent
    headerContent=headerContent
    singleErrorMessage=singleErrorMessage
    wrapperWidth=wrapperWidth
    footerContent=footerContent
    customScriptContent=analyticsScript
  >
    <@fdsGoogleAnalytics.googleAnalytics measurementId=analytics.appTag />
    <@fdsGoogleAnalytics.googleAnalytics measurementId=analytics.globalTag />
    <#nested />
  </@fdsDefaultPageTemplate>
</#macro>
