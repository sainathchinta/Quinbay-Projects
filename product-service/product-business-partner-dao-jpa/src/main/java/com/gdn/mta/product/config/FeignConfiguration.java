package com.gdn.mta.product.config;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.micro.graphics.web.model.CustomGraphicsSettings;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.AGPQuery.AGPQueryFeign;
import com.gdn.partners.pbp.outbound.calendar.CalendarFeign;
import com.gdn.partners.pbp.outbound.campaign.XCampaignFeign;
import com.gdn.partners.pbp.outbound.inventory.feign.InventoryFeign;
import com.gdn.partners.pbp.outbound.margin.feign.MarginFeign;
import com.gdn.partners.pbp.outbound.merchantEducation.feign.MerchantEducationFeign;
import com.gdn.partners.pbp.outbound.pdt.feign.PDTFeign;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.partners.pbp.outbound.productAnalytics.feign.ProductAnalyticsFeign;
import com.gdn.partners.pbp.outbound.productPricing.feign.ProductPricingFeign;
import com.gdn.partners.pbp.outbound.sap.feign.SapFeign;
import com.gdn.partners.pbp.outbound.sellerLogistics.feign.SellerLogisticsFeign;
import com.gdn.partners.pbp.outbound.warehouse.feign.OMSFeign;
import com.gdn.partners.pbp.outbound.warehouse.feign.WareHouseFeign;
import com.gdn.partners.pbp.outbound.xProduct.feign.XProductFeign;
import com.gdn.partners.pbp.outbound.xbp.feign.XbpFeign;
import com.gdn.partners.pbp.outbound.xgp.feign.XgpFeign;
import com.google.api.client.http.javanet.NetHttpTransport;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.youtube.YouTube;
import feign.Feign;
import feign.Request;
import feign.Retryer;
import feign.auth.BasicAuthRequestInterceptor;
import feign.form.FormEncoder;
import feign.jackson.JacksonDecoder;
import feign.jackson.JacksonEncoder;
import feign.slf4j.Slf4jLogger;
import lombok.Data;

@Configuration
@Data
public class FeignConfiguration {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CLIENT_ID = "com.gdn.mta.product";
  private static final String DEFAULT_CHANNEL_ID = "api";
  private static final Integer DEFAULT_PORT = 80;
  private static final Integer DEFAULT_TIMEOUT = 60000;
  private static final String APP_ID = "PBP";
  public static final String SEPARATOR = ";";
  public static final String SLASH = "/";

  @Value("${api.productcategorybase.host}")
  private String productCategoryBaseHost;

  @Value("${api.productcategorybase.port}")
  private String productCategoryBasePort;

  @Value("${api.productcategorybase.context}")
  private String productCategoryContext;

  @Value("${api.productcategorybase.timeout}")
  private String productCategoryBaseTimeout;

  @Value("${api.businesspartner.host}")
  private String businessPartnerBaseHost;

  @Value("${api.businesspartner.port}")
  private String businessPartnerBasePort;

  @Value("${api.businesspartner.context}")
  private String businessPartnerContext;

  @Value("${api.product.host}")
  private String productHost;

  @Value("${api.product.port}")
  private String productPort;

  @Value("${api.product.username}")
  private String productUsername;

  @Value("${api.product.password}")
  private String productPassword;

  @Value("${api.product.timeout}")
  private String productTimeout;

  @Value("${api.product.context}")
  private String productContext;

  @Value("${api.seller.logistics.host}")
  private String sellerHost;

  @Value("${api.seller.logistics.port}")
  private String sellerPort;

  @Value("${api.seller.logistics.timeout}")
  private String sellerTimeout;

  @Value("${api.oms.host}")
  private String omsHost;

  @Value("${api.oms.port}")
  private String omsPort;

  @Value("${api.oms.timeout}")
  private String omsTimeout;

  @Value("${api.inventory.host}")
  private String inventoryHost;

  @Value("${api.inventory.port}")
  private String inventoryPort;

  @Value("${api.inventory.context}")
  private String inventoryContext;

  @Value("${api.inventory.username}")
  private String inventoryUsername;

  @Value("${api.inventory.password}")
  private String inventoryPassword;

  @Value("${api.inventory.timeout}")
  private String inventoryTimeout;

  @Value("${api.warehouse.oms.host}")
  private String omsWareHouseHost;

  @Value("${api.warehouse.oms.port}")
  private String omsWareHousePort;

  @Value("${api.margin.feign.port}")
  private String marginFeignPort;

  @Value("${api.margin.feign.host}")
  private String marginFeignHost;

  @Value("${api.warehouse.oms.context}")
  private String omsWareHouseContext;

  @Value("${api.margin.context}")
  private String marginContext;

  @Value("${api.warehouse.oms.timeout}")
  private String omsWareHouseTimeout;

  @Value("${api.margin.timeout}")
  private String marginTimeout;

  @Value("${api.xbp.host}")
  private String xbpHost;

  @Value("${api.xbp.port}")
  private String xbpPort;

  @Value("${api.xbp.context}")
  private String xbpContext;

  @Value("${api.xbp.timeout}")
  private String xbpTimeout;

  @Value("${api.calendar.host}")
  private String calendarHost;

  @Value("${api.calendar.port}")
  private String calendarApiPort;

  @Value("${api.calendar.context}")
  private String calendarContext;

  @Value("${api.calendar.timeout}")
  private String calendarTimeout;

  @Value("${api.history.username}")
  private String historyUsername;

  @Value("${api.history.password}")
  private String historyPassword;

  @Value("${api.history.host}")
  private String historyHost;

  @Value("${api.history.port}")
  private String historyPort;

  @Value("${api.promotion.username}")
  private String promotionUsername;

  @Value("${api.promotion.password}")
  private String promotionPassword;

  @Value("${api.promotion.host}")
  private String promotionHost;

  @Value("${api.promotion.port}")
  private String promotionPort;

  @Value("${api.promotion.context}")
  private String promotionContext;

  @Value("${api.promotion.timeout}")
  private String promotionTimeout;

  @Value("${api.ext-catalog.username}")
  private String extCatalogUsername;

  @Value("${api.ext-catalog.password}")
  private String extCatalogPassword;

  @Value("${api.ext-catalog.host}")
  private String extCatalogHost;

  @Value("${api.ext-catalog.port}")
  private String extCatalogPort;

  @Value("${api.ext-catalog.context}")
  private String extCatalogContext;

  @Value("${api.ext-catalog.timeout}")
  private String extCatalogTimeout;

  @Value("${holiday.calendar.argument.username}")
  private String calendarUserName;

  @Value("${holiday.calendar.argument.password}")
  private String calendarPassword;

  @Value("${holiday.calendar.argument.host}")
  private String calendarHostName;

  @Value("${holiday.calendar.argument.port}")
  private int calendarPort;

  @Value("${holiday.calendar.argument.contextPath}")
  private String calendarContextPath;

  @Value("${api.pdt.host}")
  private String productDistributionTaskHost;

  @Value("${api.pdt.port}")
  private String productDistributionTaskPort;

  @Value("${api.pdt.context}")
  private String productDistributionTaskContext;

  @Value("${api.pdt.timeout}")
  private String productDistributionTaskTimeout;

  @Value("${mta.images.size.full.width}")
  private String mtaImagesSizeFullWidth;

  @Value("${mta.images.size.full.height}")
  private String mtaImagesSizeFullHeight;

  @Value("${mta.images.size.medium.width}")
  private String mtaImagesSizeMediumWidth;

  @Value("${mta.images.size.medium.height}")
  private String mtaImagesSizeMediumHeight;

  @Value("${mta.images.size.thumbnail.width}")
  private String mtaImagesSizeThumbnailWidth;

  @Value("${mta.images.size.thumbnail.height}")
  private String mtaImagesSizeThumbnailHeight;

  @Value("${graphic.processor.username}")
  private String graphicProcessorUserName;

  @Value("${graphic.processor.password}")
  private String graphicProcessorPassword;

  @Value("${graphic.processor.host}")
  private String graphicProcessorHost;

  @Value("${graphic.processor.port}")
  private int graphicProcessorPort;

  @Value("${graphic.processor.client.id}")
  private String graphicProcessorClientId;

  @Value("${graphic.processor.channel.id}")
  private String graphicProcessorChannelId;

  @Value("${graphic.processor.store.id}")
  private String graphicProcessorStoreId;

  @Value("${graphic.processor.timeout}")
  private int graphicProcessorTimeout;

  @Value("${resize.image.client.id.default}")
  private String graphicProcessorResizeClientId;

  @Value("${edited.products.client.id.default}")
  private String editedProductsProcessorClientId;

  @Value("${api.margin.host}")
  private String marginHost;

  @Value("${api.margin.port}")
  private String marginPort;

  @Value("${image.quality}")
  private String imageQuality;

  @Value("${product.pricing.host}")
  private String productPricingHost;

  @Value("${product.pricing.connectionTimeoutInMs}")
  private int productPricingFeignTimeout;

  @Value("${rest.template.connectionTimeoutInMs}")
  private int restTemplateConnectionTimeout;

  @Value("${rest.template.readTimeoutInMs}")
  private int restTemplateReadTimeout;

  @Value("${api.xcampaign.host}")
  private String xcampaignHost;

  @Value("${api.xcampaign.port}")
  private String xcampaignPort;

  @Value("${api.xcampaign.connectTimeout}")
  private String xcampaignConnectTimeout;

  @Value("${api.xcampaign.context}")
  private String xcampaignContext;

  @Value("${api.xcampaign.max.retry.count}")
  private int xcampaignMaxRetryCount;

  @Value("${product-analytics.feign.host}")
  private String productAnalyticsHost;

  @Value("${product-analytics.port}")
  private String productAnalyticsPort;

  @Value("${product-analytics.timeout}")
  private String productAnalyticsTimeout;

  @Value("${product-analytics.contextPath}")
  private String productAnalyticsContext;

  @Value("${merchant-education.feign.host}")
  private String merchantEducationHost;

  @Value("${merchant-education.port}")
  private String merchantEducationPort;

  @Value("${merchant-education.timeout}")
  private String merchantEducationTimeout;

  @Value("${merchant-education.contextPath}")
  private String merchantEducationContext;

  @Value("${api.sap.host}")
  private String sapHost;

  @Value("${api.sap.port}")
  private String sapPort;

  @Value("${api.sap.context}")
  private String sapContext;

  @Value("${api.sap.timeout}")
  private String sapTimeout;

  @Value("${sap.username}")
  private String sapUsername;

  @Value("${sap.password}")
  private String sapPassword;

  @Value("${agpQuery.host}")
  private String agpQueryHost;

  @Value("${agpQuery.port}")
  private String agpQueryPort;

  @Value("${agpQuery.contextPath}")
  private String agpQueryPath;

  @Value("${agpQuery.timeout}")
  private String agpQueryTimeout;

  @Value("${graphic.processor.context}")
  private String graphicProcessorContext;

  @Autowired
  private ObjectMapper objectMapper;

  @Bean
  public ProductPricingFeign productPricingFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
        .decoder(new JacksonDecoder(objectMapper))
        .logger(new Slf4jLogger(ProductPricingFeign.class))
        .options(new Request.Options(productPricingFeignTimeout, productPricingFeignTimeout))
        .target(ProductPricingFeign.class, productPricingHost);
  }


  @Bean(name = "customGraphicsSettings")
  public List<CustomGraphicsSettings> customGraphicsSettings() {
    CustomGraphicsSettings fullImageSize = new CustomGraphicsSettings(Integer.parseInt(mtaImagesSizeFullWidth),
        Integer.parseInt(mtaImagesSizeFullHeight));
    CustomGraphicsSettings mediumImageSize = new CustomGraphicsSettings(Integer.parseInt(mtaImagesSizeMediumWidth),
        Integer.parseInt(mtaImagesSizeMediumHeight));
    CustomGraphicsSettings thumbImageSize = new CustomGraphicsSettings(Integer.parseInt(mtaImagesSizeThumbnailWidth),
        Integer.parseInt(mtaImagesSizeThumbnailHeight));
    return Arrays.asList(fullImageSize, mediumImageSize, thumbImageSize);
  }

  @Bean(name = "customGraphicsSettingsForImageResizing")
  public CustomGraphicsSettings customGraphicsSettingsForImageResizing() {
    CustomGraphicsSettings customGraphicsSettings =
        new CustomGraphicsSettings(Integer.parseInt(mtaImagesSizeFullWidth), Integer.parseInt(mtaImagesSizeFullHeight));
    customGraphicsSettings.setQuality(Double.valueOf(imageQuality));
    return customGraphicsSettings;
  }

  @Bean
  public static YouTube youTube() {
    return new YouTube.Builder(new NetHttpTransport(), new JacksonFactory(), request -> {
    }).setApplicationName(APP_ID).build();
  }

  @Bean
  public PCBFeign pcbFeign() {
    return Feign.builder().encoder(new JacksonEncoder(objectMapper))
        .decoder(new JacksonDecoder(objectMapper))
        .logger(new Slf4jLogger(PCBFeign.class))
        .options(new Request.Options(Integer.parseInt(productCategoryBaseTimeout),
            Integer.parseInt(productCategoryBaseTimeout)))
        .target(PCBFeign.class, getPCBUrl());
  }

  private String getPCBUrl() {
    return new StringBuilder(productCategoryBaseHost).append(Constants.COLON).append(productCategoryBasePort)
        .append(Constants.DELIMITER_SLASH).append(productCategoryContext).toString();
  }

  @Bean
  public PDTFeign pdtFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
        .decoder(new JacksonDecoder(objectMapper))
        .logger(new Slf4jLogger(PDTFeign.class))
        .options(new Request.Options(Integer.parseInt(productDistributionTaskTimeout),
            Integer.parseInt(productDistributionTaskTimeout)))
        .target(PDTFeign.class, getPDTUrl());
  }

  private String getPDTUrl() {
    return new StringBuilder(productDistributionTaskHost).append(Constants.COLON).append(productDistributionTaskPort)
        .append(Constants.DELIMITER_SLASH).append(productDistributionTaskContext).toString();
  }

  @Bean
  public SapFeign sapFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
        .decoder(new JacksonDecoder(objectMapper)).logger(new Slf4jLogger(SapFeign.class))
        .options(new Request.Options(Integer.parseInt(sapTimeout), Integer.parseInt(sapTimeout)))
        .requestInterceptor(new BasicAuthRequestInterceptor(sapUsername, sapPassword))
        .target(SapFeign.class, getSAPUrl());
  }

  private String getSAPUrl() {
    return new StringBuilder(sapHost).append(Constants.COLON).append(sapPort).append(Constants.DELIMITER_SLASH)
        .append(sapContext).toString();
  }

  @Bean
  public XProductFeign xProductFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
        .decoder(new JacksonDecoder(objectMapper))
        .logger(new Slf4jLogger(XProductFeign.class))
        .options(new Request.Options(Integer.parseInt(productTimeout),
            Integer.parseInt(productTimeout)))
        .target(XProductFeign.class, getXProductUrl());
  }

  private String getXProductUrl() {
    return new StringBuilder(productHost).append(Constants.COLON).append(productPort)
        .append(Constants.DELIMITER_SLASH).append(productContext).toString();
  }

  @Bean
  public XgpFeign xgpFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
        .decoder(new JacksonDecoder(objectMapper)).logger(new Slf4jLogger(XgpFeign.class))
        .options(new Request.Options(graphicProcessorTimeout, graphicProcessorTimeout))
        .target(XgpFeign.class, getXgpUrl());
  }

  private String getXgpUrl() {
    return new StringBuilder(graphicProcessorHost).append(Constants.COLON).append(productPort)
        .append(Constants.DELIMITER_SLASH).append(graphicProcessorContext).toString();
  }

  @Bean
  public CalendarFeign calendarFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
        .decoder(new JacksonDecoder(objectMapper))
        .logger(new Slf4jLogger(CalendarFeign.class))
        .options(new Request.Options(Integer.parseInt(calendarTimeout),
            Integer.parseInt(calendarTimeout)))
        .target(CalendarFeign.class, getCalendarUrl());
  }

  private String getCalendarUrl() {
    return new StringBuilder(calendarHost).append(Constants.COLON).append(calendarApiPort)
        .append(Constants.DELIMITER_SLASH).append(calendarContext).toString();
  }

  @Bean
  public SellerLogisticsFeign sellerLogisticsFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
            .decoder(new JacksonDecoder(objectMapper))
            .logger(new Slf4jLogger(SellerLogisticsFeign.class))
            .options(new Request.Options(Integer.parseInt(sellerTimeout),
                    Integer.parseInt(sellerTimeout)))
            .target(SellerLogisticsFeign.class, getSellerLogisticsUrl());
  }

  @Bean
  public OMSFeign OMSFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
        .decoder(new JacksonDecoder(objectMapper))
        .logger(new Slf4jLogger(SellerLogisticsFeign.class))
        .options(new Request.Options(Integer.parseInt(omsTimeout),
            Integer.parseInt(omsTimeout)))
        .target(OMSFeign.class, getOMSUrl());
  }

  private String getOMSUrl() {
    return new StringBuilder(omsHost).append(Constants.COLON).append(omsPort)
        .append(Constants.DELIMITER_SLASH).toString();
  }

  private String getSellerLogisticsUrl() {
    return new StringBuilder(sellerHost).append(Constants.COLON).append(sellerPort)
            .append(Constants.DELIMITER_SLASH).toString();
  }

  @Bean
  public XCampaignFeign getXCampaignFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
        .decoder(new JacksonDecoder(objectMapper))
        .logger(new Slf4jLogger(XCampaignFeign.class))
        .options(new Request.Options(Integer.parseInt(xcampaignConnectTimeout),
            Integer.parseInt(xcampaignConnectTimeout)))
        .retryer(new Retryer.Default(100L, TimeUnit.SECONDS.toMillis(1L), xcampaignMaxRetryCount))
        .target(XCampaignFeign.class, getXCampaignFeignUrl());
  }

  private String getXCampaignFeignUrl() {
    return new StringBuilder(xcampaignHost).append(Constants.COLON).append(xcampaignPort)
        .append(Constants.DELIMITER_SLASH).append(xcampaignContext).toString();
  }

  @Bean
  public ProductAnalyticsFeign getProductAnalyticsFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
        .decoder(new JacksonDecoder(objectMapper))
        .logger(new Slf4jLogger(ProductAnalyticsFeign.class))
        .options(new Request.Options(Integer.parseInt(productAnalyticsTimeout),
            Integer.parseInt(productAnalyticsTimeout)))
        .target(ProductAnalyticsFeign.class, getProductAnalyticsFeignUrl());
  }

  private String getProductAnalyticsFeignUrl() {
    return new StringBuilder(productAnalyticsHost).append(Constants.COLON).append(productAnalyticsPort)
        .append(Constants.DELIMITER_SLASH).append(productAnalyticsContext).toString();
  }

  @Bean
  public AGPQueryFeign agpQueryFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
        .decoder(new JacksonDecoder(objectMapper)).logger(new Slf4jLogger(AGPQueryFeign.class))
        .options(new Request.Options(Integer.parseInt(agpQueryTimeout),Integer.parseInt(agpQueryTimeout)))
        .target(AGPQueryFeign.class, getAGPQueryUrl());
  }

  @Bean
  public InventoryFeign inventoryFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
      .decoder(new JacksonDecoder(objectMapper)).logger(new Slf4jLogger(InventoryFeign.class))
      .options(new Request.Options(Integer.parseInt(inventoryTimeout),Integer.parseInt(inventoryTimeout)))
      .target(InventoryFeign.class, getInventoryFeignUrl());
  }

  @Bean
  public XbpFeign xbpFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
        .decoder(new JacksonDecoder(objectMapper)).logger(new Slf4jLogger(XbpFeign.class))
        .options(new Request.Options(Integer.parseInt(xbpTimeout), Integer.parseInt(xbpTimeout)))
        .target(XbpFeign.class, getXbpFeignUrl());
  }

  @Bean
  public WareHouseFeign wareHouseFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
        .decoder(new JacksonDecoder(objectMapper)).logger(new Slf4jLogger(WareHouseFeign.class))
        .options(new Request.Options(Integer.parseInt(omsWareHouseTimeout),Integer.parseInt(omsWareHouseTimeout)))
        .target(WareHouseFeign.class, getWareHouseFeignUrl());
  }

  @Bean
  public MarginFeign marginFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
        .decoder(new JacksonDecoder(objectMapper)).logger(new Slf4jLogger(MarginFeign.class))
        .options(new Request.Options(Integer.parseInt(marginTimeout),Integer.parseInt(marginTimeout)))
        .target(MarginFeign.class, getMarginFeignUrl());
  }


  private String getAGPQueryUrl() {
    return new StringBuilder(agpQueryHost).append(Constants.COLON).append(agpQueryPort).toString();
  }

  @Bean
  public MerchantEducationFeign getMerchantEducationFeign() {
    return Feign.builder().encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
        .decoder(new JacksonDecoder(objectMapper))
        .logger(new Slf4jLogger(MerchantEducationFeign.class))
        .options(new Request.Options(Integer.parseInt(merchantEducationTimeout),
            Integer.parseInt(merchantEducationTimeout)))
        .target(MerchantEducationFeign.class, getMerchantEducationFeignUrl());
  }

  private String getInventoryFeignUrl() {
    return new StringBuilder(inventoryHost).append(Constants.COLON).append(inventoryPort)
        .append(Constants.DELIMITER_SLASH).append(inventoryContext).toString();
  }

  private String getWareHouseFeignUrl() {
    return new StringBuilder(omsWareHouseHost).append(Constants.COLON).append(omsWareHousePort)
        .append(Constants.DELIMITER_SLASH).append(omsWareHouseContext).toString();
  }

  private String getMarginFeignUrl() {
    return new StringBuilder(marginFeignHost).append(Constants.COLON).append(marginFeignPort)
        .append(Constants.DELIMITER_SLASH).append(marginContext).toString();
  }

  private String getXbpFeignUrl() {
    return new StringBuilder(xbpHost).append(Constants.COLON).append(xbpPort).append(Constants.DELIMITER_SLASH)
        .append(xbpContext).toString();
  }

  private String getMerchantEducationFeignUrl() {
    return new StringBuilder(merchantEducationHost).append(Constants.COLON).append(merchantEducationPort)
        .append(Constants.DELIMITER_SLASH).append(merchantEducationContext).toString();
  }

}
