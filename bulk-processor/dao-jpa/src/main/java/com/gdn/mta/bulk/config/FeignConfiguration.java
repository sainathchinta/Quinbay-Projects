package com.gdn.mta.bulk.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.feignConfig.AGPQueryFeign;
import com.gdn.mta.bulk.feignConfig.CMSFeign;
import com.gdn.mta.bulk.feignConfig.CustomDeserializationProblemHandler;
import com.gdn.mta.bulk.feignConfig.DsProtectedBrandFeign;
import com.gdn.mta.bulk.feignConfig.InventoryFeign;
import com.gdn.mta.bulk.feignConfig.MasterSkuReviewFeign;
import com.gdn.mta.bulk.feignConfig.OrderFeign;
import com.gdn.mta.bulk.feignConfig.OrderHistoryFeign;
import com.gdn.mta.bulk.feignConfig.PBPFeign;
import com.gdn.mta.bulk.feignConfig.PCBFeign;
import com.gdn.mta.bulk.feignConfig.PDTFeign;
import com.gdn.mta.bulk.feignConfig.PartnersEngineFeign;
import com.gdn.mta.bulk.feignConfig.PriceAnalyticsFeign;
import com.gdn.mta.bulk.feignConfig.ProductAnalyticsFeign;
import com.gdn.mta.bulk.feignConfig.ProductAssemblyFeign;
import com.gdn.mta.bulk.feignConfig.ProductCategoryPredictionFeign;
import com.gdn.mta.bulk.feignConfig.PromoAnalyticsFeign;
import com.gdn.mta.bulk.feignConfig.XBPFeign;
import com.gdn.mta.bulk.feignConfig.XCampaignFeign;
import com.gdn.mta.bulk.feignConfig.XProductFeign;
import com.gdn.mta.bulk.feignConfig.XgpFeign;
import com.gdn.partners.pbp.commons.constants.Constants;
import feign.Feign;
import feign.Request;
import feign.form.FormEncoder;
import feign.jackson.JacksonDecoder;
import feign.jackson.JacksonEncoder;
import feign.slf4j.Slf4jLogger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class FeignConfiguration {

  @Value("${businessPartner.host}")
  private String businessPartnerHost;

  @Value("${businessPartner.port}")
  private String businessPartnerPort;

  @Value("${businessPartner.contextPath}")
  private String businessPartnerContextPath;

  @Value("${businessPartner.timeout}")
  private int businessPartnerTimeout;

  @Value("${productBusinessPartner.host}")
  private String productBusinessPartnerHost;

  @Value("${productBusinessPartner.port}")
  private String productBusinessPartnerPort;

  @Value("${productBusinessPartner.contextPath}")
  private String productBusinessPartnerContextPath;

  @Value("${productBusinessPartner.timeout}")
  private Integer productBusinessPartnerTimeout;

  @Value("${productCategoryBase.host}")
  private String productCategoryBaseHost;

  @Value("${productCategoryBase.feign.host}")
  private String productCategoryBaseFeignHost;

  @Value("${productBusinessPartner.feign.host}")
  private String productBusinessPartnerFeignHost;

  @Value("${productCategoryBase.port}")
  private String productCategoryBasePort;

  @Value("${productCategoryBase.contextPath}")
  private String productCategoryBaseContextPath;

  @Value("${productCategoryBase.timeout:30000}")
  private Integer productCategoryBaseTimeout;

  @Value("${api.xorder2.host}")
  private String xorder2Host;

  @Value("${api.xorder2.port}")
  private String xorder2Port;

  @Value("${api.xorder2.username}")
  private String xorder2Username;

  @Value("${api.xorder2.password}")
  private String xorder2Password;

  @Value("${api.xorder2.timeout}")
  private String xorder2Timeout;

  @Value("${api.xorder2.contextPath}")
  private String xorder2ContextPath;

  @Value("${api.pdt.host}")
  private String pdtHost;

  @Value("${api.pdt.port}")
  private String pdtPort;

  @Value("${api.pdt.context}")
  private String pdtContext;

  @Value("${api.pdt.timeout}")
  private String pdtTimeout;

  @Value("${api.restweb.timeout}")
  private int restWebTimout;

  @Value("${xCampaign.feign.host}")
  private String xCampaignFeignHost;

  @Value("${xCampaign.contextPath}")
  private String xCampaignContextPath;

  @Value("${agpQuery.host}")
  private String agpQueryHost;

  @Value("${agpQuery.port}")
  private String agpQueryPort;

  @Value("${agpQuery.contextPath}")
  private String agpQueryPath;

  @Value("${agpQuery.timeout}")
  private Integer agpQueryTimeout;

  @Value("${api.partners.engine.host}")
  private String partnersEngineHost;

  @Value("${api.partners.engine.port}")
  private String partnersEnginePort;

  @Value("${api.partners.engine.context}")
  private String partnersEngineContext;

  @Value("${api.partners.engine.timeout}")
  private Integer partnersEngineTimeout;

  @Value("${pdt.feign.host}")
  private String pdtFeignHost;

  @Value("${pdt.timeout:30000}")
  private Integer productDistributionTimeout;

  @Value("${masterSkuReview.host}")
  private String masterSkuReviewHost;

  @Value("${masterSkuReview.port}")
  private String masterSkuReviewPort;

  @Value("${masterSkuReview.contextPath}")
  private String masterSkuReviewPath;

  @Value("${masterSkuReview.timeout}")
  private Integer masterSkuReviewTimeout;

  @Value("${productAnalytics.host}")
  private String productAnalyticsHost;

  @Value("${productAnalytics.port}")
  private String productAnalyticsPort;

  @Value("${productAnalytics.contextPath}")
  private String productAnalyticsPath;

  @Value("${productAnalytics.timeout}")
  private Integer productAnalyticsTimeout;

  @Value("${priceAnalytics.host}")
  private String priceAnalyticsHost;

  @Value("${priceAnalytics.port}")
  private String priceAnalyticsPort;

  @Value("${priceAnalytics.contextPath}")
  private String priceAnalyticsPath;

  @Value("${priceAnalytics.timeout}")
  private Integer priceAnalyticsTimeout;

  @Value("${api.campaign.host}")
  private String campaignHost;

  @Value("${api.campaign.port}")
  private String campaignPort;

  @Value("${api.campaign.timeout}")
  private Integer campaignTimeout;

  @Value("${x-product.feign.host}")
  private String xProductHost;

  @Value("${x-product.port}")
  private String xProductPort;

  @Value("${x-product.contextPath}")
  private String xProductContextPath;

  @Value("${x-product.timeout:30000}")
  private Integer xProductTimeout;

  @Value("${inventory.feign.host}")
  private String inventoryHost;

  @Value("${inventory.port}")
  private String inventoryPort;

  @Value("${inventory.contextPath}")
  private String inventoryContextPath;

  @Value("${inventory.timeout:30000}")
  private Integer inventoryTimeout;

  @Value("${product.assembly.feign.host}")
  private String productAssemblyHost;

  @Value("${product.assembly.port}")
  private String productAssemblyPort;

  @Value("${product.assembly.contextPath}")
  private String productAssemblyContextPath;

  @Value("${product.assembly.timeout:30000}")
  private Integer productAssemblyTimeout;

  @Value("${promo-analytics.feign.host}")
  private String promoAnalyticsHost;

  @Value("${promo-analytics.port}")
  private String promoAnalyticsPort;

  @Value("${promo-analytics.contextPath}")
  private String promoAnalyticsContextPath;

  @Value("${promo-analytics.timeout}")
  private Integer promoAnalyticsTimeout;

  @Value("${api.order.history.host}")
  private String orderHistoryHost;

  @Value("${api.order.history.port}")
  private String orderHistoryPort;

  @Value("${api.order.history.contextPath}")
  private String orderHistoryContextPath;

  @Value("${api.order.history.timeout}")
  private Integer orderHistoryTimeout;

  @Value("${api.cms.host}")
  private String cmsHost;

  @Value("${api.cms.port}")
  private String cmsPort;

  @Value("${api.cms.contextPath}")
  private String cmsContextPath;

  @Value("${api.cms.timeout}")
  private Integer cmsTimeout;

  @Value("${xgp.timeout}")
  private Integer xgpTimeout;

  @Value("${xgp.contextPath}")
  private String xgpContextPath;

  @Value("${xgp.port}")
  private String xgpPort;

  @Value("${xgp.feign.host}")
  private String xgpFeignHost;

  @Value("${product.category.prediction.host}")
  private String productCategoryPredictionHost;

  @Value("${product.category.prediction.timeout}")
  private Integer productCategoryPredictionTimeout;

  @Value("${product.category.prediction.port}")
  private String productCategoryPredictionPort;

  @Value("${ds.protected.brand.host}")
  private String dsProtectedBrandHost;

  @Value("${ds.protected.brand.timeout}")
  private Integer dsProtectedBrandTimeout;

  @Value("${ds.protected.brand.port}")
  private String dsProtectedBrandPort;

  @Bean
  public static ObjectMapper objectMapper() {
    return new ObjectMapper().addHandler(FeignConfiguration.customDeserializationProblemHandler());
  }

  @Bean
  public XgpFeign xgpFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper())).logger(new Slf4jLogger(XgpFeign.class))
        .options(new Request.Options(xgpTimeout, xgpTimeout)).target(XgpFeign.class, getXgpUrl());
  }

  private String getXgpUrl() {
    return xgpFeignHost + Constants.COLON + xgpPort + Constants.DELIMITER_SLASH + xgpContextPath;
  }

  @Bean
  public static CustomDeserializationProblemHandler customDeserializationProblemHandler() {
    return new CustomDeserializationProblemHandler();
  }

  @Bean
  public PCBFeign pcbFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper())).logger(new Slf4jLogger(PCBFeign.class))
        .options(new Request.Options(productCategoryBaseTimeout, productCategoryBaseTimeout))
        .target(PCBFeign.class, getPCBUrl());
  }

  @Bean
  public ProductAssemblyFeign productAssemblyFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper()))
        .logger(new Slf4jLogger(ProductAssemblyFeign.class))
        .options(new Request.Options(productAssemblyTimeout, productAssemblyTimeout))
        .target(ProductAssemblyFeign.class,
            new StringBuilder(productAssemblyHost).append(Constants.COLON)
                .append(productAssemblyPort).append(Constants.DELIMITER_SLASH)
                .append(productAssemblyContextPath).toString());
  }

  @Bean
  public XProductFeign xProductFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper())).logger(new Slf4jLogger(XProductFeign.class))
        .options(new Request.Options(xProductTimeout, xProductTimeout)).target(XProductFeign.class,
            new StringBuilder(xProductHost).append(Constants.COLON).append(xProductPort)
                .append(Constants.DELIMITER_SLASH).append(xProductContextPath).toString());
  }

  @Bean
  public InventoryFeign inventoryFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper())).logger(new Slf4jLogger(InventoryFeign.class))
        .options(new Request.Options(inventoryTimeout, inventoryTimeout))
        .target(InventoryFeign.class,
            new StringBuilder(inventoryHost).append(Constants.COLON).append(inventoryPort)
                .append(Constants.DELIMITER_SLASH).append(inventoryContextPath).toString());
  }

  @Bean
  public PromoAnalyticsFeign promoAnalyticsFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper()))
        .logger(new Slf4jLogger(PromoAnalyticsFeign.class))
        .options(new Request.Options(promoAnalyticsTimeout, promoAnalyticsTimeout))
        .target(PromoAnalyticsFeign.class,
            new StringBuilder(promoAnalyticsHost).append(Constants.COLON).append(promoAnalyticsPort)
                .append(Constants.DELIMITER_SLASH).append(promoAnalyticsContextPath).toString());
  }

  @Bean
  public PBPFeign pbpFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper())).logger(new Slf4jLogger(PBPFeign.class))
        .options(new Request.Options(productBusinessPartnerTimeout, productBusinessPartnerTimeout))
        .target(PBPFeign.class, getPBPUrl());
  }

  @Bean
  public XBPFeign xbpFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper())).logger(new Slf4jLogger(XBPFeign.class))
        .options(new Request.Options(businessPartnerTimeout, businessPartnerTimeout))
        .target(XBPFeign.class, new StringBuilder(businessPartnerHost).append(Constants.COLON)
            .append(businessPartnerPort).append(Constants.DELIMITER_SLASH)
            .append(businessPartnerContextPath).toString());
  }

  @Bean
  public OrderFeign orderFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper())).logger(new Slf4jLogger(OrderFeign.class))
        .options(
            new Request.Options(Integer.parseInt(xorder2Timeout), Integer.parseInt(xorder2Timeout)))
        .target(OrderFeign.class,
            new StringBuilder(xorder2Host).append(Constants.COLON).append(xorder2Port)
                .append(Constants.DELIMITER_SLASH).append(xorder2ContextPath).toString());
  }

  @Bean
  public XCampaignFeign xCampaignFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper())).logger(new Slf4jLogger(XCampaignFeign.class))
        .options(new Request.Options(campaignTimeout, campaignTimeout))
        .target(XCampaignFeign.class, getXCampaignUrl());
  }

  @Bean
  public AGPQueryFeign agpQueryFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper())).logger(new Slf4jLogger(AGPQueryFeign.class))
        .options(new Request.Options(agpQueryTimeout, agpQueryTimeout))
        .target(AGPQueryFeign.class, getAGPQueryUrl());
  }

  @Bean
  public PartnersEngineFeign partnersEngineFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper()))
        .logger(new Slf4jLogger(PartnersEngineFeign.class))
        .options(new Request.Options(partnersEngineTimeout, partnersEngineTimeout))
        .target(PartnersEngineFeign.class, getPartnersEngineUrl());
  }

  @Bean
  public PDTFeign pdtFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper())).logger(new Slf4jLogger(PDTFeign.class))
        .options(new Request.Options(productDistributionTimeout, productDistributionTimeout))
        .target(PDTFeign.class, getPDTUrl());
  }

  @Bean
  public MasterSkuReviewFeign masterSkuReviewFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper()))
        .logger(new Slf4jLogger(MasterSkuReviewFeign.class))
        .options(new Request.Options(masterSkuReviewTimeout, masterSkuReviewTimeout))
        .target(MasterSkuReviewFeign.class, getMasterSkuReviewUrl());
  }

  @Bean
  public ProductAnalyticsFeign productAnalyticsFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper()))
        .logger(new Slf4jLogger(ProductAnalyticsFeign.class))
        .options(new Request.Options(productAnalyticsTimeout, productAnalyticsTimeout))
        .target(ProductAnalyticsFeign.class, getProductAnalyticsUrl());
  }

  @Bean
  public PriceAnalyticsFeign priceAnalyticsFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper()))
        .logger(new Slf4jLogger(ProductAnalyticsFeign.class))
        .options(new Request.Options(priceAnalyticsTimeout, priceAnalyticsTimeout))
        .target(PriceAnalyticsFeign.class, getPriceAnalyticsUrl());
  }

  @Bean
  public OrderHistoryFeign orderHistoryFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper()))
        .logger(new Slf4jLogger(OrderHistoryFeign.class))
        .options(new Request.Options(orderHistoryTimeout, orderHistoryTimeout))
        .target(OrderHistoryFeign.class, getOrderHistoryUrl());
  }

  @Bean
  public CMSFeign cmsFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper())).logger(new Slf4jLogger(CMSFeign.class))
        .options(new Request.Options(cmsTimeout, cmsTimeout)).target(CMSFeign.class, getCMSUrl());
  }

  @Bean
  public ProductCategoryPredictionFeign productCategoryPredictionFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper()))
        .logger(new Slf4jLogger(ProductCategoryPredictionFeign.class)).options(
            new Request.Options(productCategoryPredictionTimeout, productCategoryPredictionTimeout))
        .target(ProductCategoryPredictionFeign.class, getProductCategoryPredictionUrl());
  }

  @Bean
  public DsProtectedBrandFeign dsProtectedBrandFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper())))
        .decoder(new JacksonDecoder(objectMapper()))
        .logger(new Slf4jLogger(DsProtectedBrandFeign.class))
        .options(new Request.Options(dsProtectedBrandTimeout,dsProtectedBrandTimeout))
        .target(DsProtectedBrandFeign.class,getDsProtectedBrandUrl());
  }
  private String getPCBUrl() {
    return new StringBuilder(productCategoryBaseFeignHost).append(Constants.COLON)
        .append(productCategoryBasePort).append(Constants.DELIMITER_SLASH)
        .append(productCategoryBaseContextPath).toString();
  }

  private String getPBPUrl() {
    return new StringBuilder(productBusinessPartnerFeignHost).append(Constants.COLON)
        .append(productBusinessPartnerPort).append(Constants.DELIMITER_SLASH)
        .append(productBusinessPartnerContextPath).toString();
  }

  private String getPDTUrl() {
    return new StringBuilder(pdtFeignHost).append(Constants.COLON).append(pdtPort)
        .append(Constants.DELIMITER_SLASH).append(pdtContext).toString();
  }

  private String getPartnersEngineUrl() {
    return new StringBuilder(partnersEngineHost).append(Constants.COLON).append(partnersEnginePort)
        .append(Constants.DELIMITER_SLASH).append(partnersEngineContext).toString();
  }

  private String getXCampaignUrl() {
    return new StringBuilder(xCampaignFeignHost).append(Constants.COLON).append(campaignPort)
        .append(Constants.DELIMITER_SLASH).append(xCampaignContextPath).toString();
  }

  private String getAGPQueryUrl() {
    return new StringBuilder(agpQueryHost).append(Constants.COLON).append(agpQueryPort).toString();
  }

  private String getMasterSkuReviewUrl() {
    return new StringBuilder(masterSkuReviewHost).append(Constants.COLON)
        .append(masterSkuReviewPort).append(Constants.DELIMITER_SLASH).append(masterSkuReviewPath)
        .toString();
  }

  private String getProductAnalyticsUrl() {
    return new StringBuilder(productAnalyticsHost).append(Constants.COLON)
        .append(productAnalyticsPort).append(Constants.DELIMITER_SLASH).append(productAnalyticsPath)
        .toString();
  }

  private String getPriceAnalyticsUrl() {
    return new StringBuilder(priceAnalyticsHost).append(Constants.COLON).append(priceAnalyticsPort)
        .append(Constants.DELIMITER_SLASH).append(priceAnalyticsPath).toString();
  }

  private String getOrderHistoryUrl() {
    return new StringBuilder(orderHistoryHost).append(Constants.COLON).append(orderHistoryPort)
        .append(Constants.DELIMITER_SLASH).append(orderHistoryContextPath).toString();
  }

  private String getCMSUrl() {
    return new StringBuilder(cmsHost).append(Constants.COLON).append(cmsPort)
        .append(Constants.DELIMITER_SLASH).append(cmsContextPath).toString();
  }

  private String getProductCategoryPredictionUrl() {
    return new StringBuilder(productCategoryPredictionHost).append(Constants.COLON)
        .append(productCategoryPredictionPort).toString();
  }

  private String getDsProtectedBrandUrl() {
    return new StringBuilder(dsProtectedBrandHost).append(Constants.COLON)
        .append(dsProtectedBrandPort).toString();
  }

  public String getBusinessPartnerContextPath() {
    return businessPartnerContextPath;
  }

  public String getBusinessPartnerHost() {
    return businessPartnerHost;
  }

  public String getBusinessPartnerPort() {
    return businessPartnerPort;
  }

  public String getProductBusinessPartnerContextPath() {
    return productBusinessPartnerContextPath;
  }

  public String getProductBusinessPartnerHost() {
    return productBusinessPartnerHost;
  }

  public String getProductBusinessPartnerPort() {
    return productBusinessPartnerPort;
  }

  public Integer getProductBusinessPartnerTimeout() {
    return productBusinessPartnerTimeout;
  }

  public String getProductCategoryBaseContextPath() {
    return productCategoryBaseContextPath;
  }

  public String getProductCategoryBaseHost() {
    return productCategoryBaseHost;
  }

  public String getProductCategoryBasePort() {
    return productCategoryBasePort;
  }

  public void setBusinessPartnerContextPath(String businessPartnerContextPath) {
    this.businessPartnerContextPath = businessPartnerContextPath;
  }

  public void setBusinessPartnerHost(String businessPartnerHost) {
    this.businessPartnerHost = businessPartnerHost;
  }

  public void setBusinessPartnerPort(String businessPartnerPort) {
    this.businessPartnerPort = businessPartnerPort;
  }

  public void setProductBusinessPartnerContextPath(String productBusinessPartnerContextPath) {
    this.productBusinessPartnerContextPath = productBusinessPartnerContextPath;
  }

  public void setProductBusinessPartnerHost(String productBusinessPartnerHost) {
    this.productBusinessPartnerHost = productBusinessPartnerHost;
  }

  public void setProductBusinessPartnerPort(String productBusinessPartnerPort) {
    this.productBusinessPartnerPort = productBusinessPartnerPort;
  }

  public void setProductBusinessPartnerTimeout(Integer productBusinessPartnerTimeout) {
    this.productBusinessPartnerTimeout = productBusinessPartnerTimeout;
  }

  public void setProductCategoryBaseContextPath(String productCategoryBaseContextPath) {
    this.productCategoryBaseContextPath = productCategoryBaseContextPath;
  }

  public void setProductCategoryBaseHost(String productCategoryBaseHost) {
    this.productCategoryBaseHost = productCategoryBaseHost;
  }

  public void setProductCategoryBasePort(String productCategoryBasePort) {
    this.productCategoryBasePort = productCategoryBasePort;
  }

  public String getXorder2Host() {
    return xorder2Host;
  }

  public void setXorder2Host(String xorder2Host) {
    this.xorder2Host = xorder2Host;
  }

  public String getXorder2Port() {
    return xorder2Port;
  }

  public void setXorder2Port(String xorder2Port) {
    this.xorder2Port = xorder2Port;
  }

  public String getXorder2Username() {
    return xorder2Username;
  }

  public void setXorder2Username(String xorder2Username) {
    this.xorder2Username = xorder2Username;
  }

  public String getXorder2Password() {
    return xorder2Password;
  }

  public void setXorder2Password(String xorder2Password) {
    this.xorder2Password = xorder2Password;
  }

  public String getXorder2Timeout() {
    return xorder2Timeout;
  }

  public void setXorder2Timeout(String xorder2Timeout) {
    this.xorder2Timeout = xorder2Timeout;
  }

  public String getPdtHost() {
    return pdtHost;
  }

  public void setPdtHost(String pdtHost) {
    this.pdtHost = pdtHost;
  }

  public String getPdtPort() {
    return pdtPort;
  }

  public void setPdtPort(String pdtPort) {
    this.pdtPort = pdtPort;
  }

  public String getPdtContext() {
    return pdtContext;
  }

  public void setPdtContext(String pdtContext) {
    this.pdtContext = pdtContext;
  }

  public String getPdtTimeout() {
    return pdtTimeout;
  }

  public void setPdtTimeout(String pdtTimeout) {
    this.pdtTimeout = pdtTimeout;
  }

  public int getRestWebTimout() {
    return restWebTimout;
  }

  public void setRestWebTimout(int restWebTimout) {
    this.restWebTimout = restWebTimout;
  }

  public String getCampaignHost() {
    return campaignHost;
  }

  public void setCampaignHost(String campaignHost) {
    this.campaignHost = campaignHost;
  }

  public String getCampaignPort() {
    return campaignPort;
  }

  public void setCampaignPort(String campaignPort) {
    this.campaignPort = campaignPort;
  }

  public Integer getCampaignTimeout() {
    return campaignTimeout;
  }

  public void setCampaignTimeout(Integer campaignTimeout) {
    this.campaignTimeout = campaignTimeout;
  }

  public String getxCampaignFeignHost() {
    return xCampaignFeignHost;
  }

  public String getxCampaignContextPath() {
    return xCampaignContextPath;
  }

  public String getAgpQueryHost() {
    return agpQueryHost;
  }

  public void setAgpQueryHost(String agpQueryHost) {
    this.agpQueryHost = agpQueryHost;
  }

  public String getAgpQueryPort() {
    return agpQueryPort;
  }

  public void setAgpQueryPort(String agpQueryPort) {
    this.agpQueryPort = agpQueryPort;
  }

  public String getAgpQueryPath() {
    return agpQueryPath;
  }

  public void setAgpQueryPath(String agpQueryPath) {
    this.agpQueryPath = agpQueryPath;
  }

  public Integer getAgpQueryTimeout() {
    return agpQueryTimeout;
  }

  public void setAgpQueryTimeout(Integer agpQueryTimeout) {
    this.agpQueryTimeout = agpQueryTimeout;
  }
}
