package com.gdn.mta.bulk.config;

import com.google.cloud.storage.Bucket;
import com.google.cloud.storage.Storage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.Mockito.when;

public class FeignConfigurationTest {

  private static final String DEFAULT_HOST = "localhost";
  private static final Integer DEFAULT_PORT = 80;
  private static final String DEFAULT_PORT_STRING = "80";
  private static final String DEFAULT_TIMEOUT_STRING = "10";
  private static final int DEFAULT_TIMEOUT_INTEGER = 10;
  private static final String DEFAULT_CONTEXT = "context";
  private static final String DEFAULT_PASSWORD = "password";
  private static final String DEFAULT_USERNAME = "system";
  private static final String DEFAULT_PROJECT_ID = "project-id";

  @InjectMocks
  private FeignConfiguration feignConfiguration;

  @Mock
  private Storage mockStorage;
  @Mock
  private Bucket mockBucket;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    feignConfiguration = new FeignConfiguration();
    feignConfiguration.setBusinessPartnerContextPath(null);
    feignConfiguration.setBusinessPartnerHost(DEFAULT_HOST);
    feignConfiguration.setBusinessPartnerPort(String.valueOf(DEFAULT_PORT));
    feignConfiguration.setProductBusinessPartnerContextPath(null);
    feignConfiguration.setProductBusinessPartnerHost(DEFAULT_HOST);
    feignConfiguration.setProductBusinessPartnerPort(String.valueOf(DEFAULT_PORT));
    feignConfiguration.setProductBusinessPartnerTimeout(DEFAULT_TIMEOUT_INTEGER);
    feignConfiguration.setBusinessPartnerContextPath(null);
    feignConfiguration.setAgpQueryHost(DEFAULT_HOST);
    feignConfiguration.setAgpQueryPort(String.valueOf(DEFAULT_PORT));
    feignConfiguration.setAgpQueryPath(null);
    feignConfiguration.setAgpQueryTimeout(DEFAULT_TIMEOUT_INTEGER);
    feignConfiguration.setProductCategoryBaseContextPath(null);
    feignConfiguration.setProductCategoryBaseHost(DEFAULT_HOST);
    feignConfiguration.setProductCategoryBasePort(String.valueOf(DEFAULT_PORT));
    feignConfiguration.setPdtHost(DEFAULT_HOST);
    feignConfiguration.setPdtPort(String.valueOf(DEFAULT_PORT));
    feignConfiguration.setCampaignHost(DEFAULT_HOST);
    feignConfiguration.setCampaignPort("8080");
    feignConfiguration.setCampaignTimeout(DEFAULT_TIMEOUT_INTEGER);
    feignConfiguration.setRestWebTimout(DEFAULT_TIMEOUT_INTEGER);
    feignConfiguration.setPdtTimeout(DEFAULT_TIMEOUT_STRING);
    feignConfiguration.setPdtContext(DEFAULT_CONTEXT);
    feignConfiguration.setXorder2Timeout(DEFAULT_TIMEOUT_STRING);
    feignConfiguration.setXorder2Password(DEFAULT_PASSWORD);
    feignConfiguration.setXorder2Username(DEFAULT_USERNAME);
    feignConfiguration.setXorder2Port(DEFAULT_PORT_STRING);
    feignConfiguration.setXorder2Host(DEFAULT_HOST);
    when(mockStorage.get(ArgumentMatchers.anyString(), ArgumentMatchers.any())).thenReturn(mockBucket);
    ReflectionTestUtils.setField(feignConfiguration, "xorder2Port", "8080");
    ReflectionTestUtils.setField(feignConfiguration, "productBusinessPartnerPort", "8080");
    ReflectionTestUtils.setField(feignConfiguration, "xorder2Timeout", "8080");
    ReflectionTestUtils.setField(feignConfiguration, "productCategoryBaseTimeout", DEFAULT_TIMEOUT_INTEGER);
    ReflectionTestUtils.setField(feignConfiguration, "productDistributionTimeout", DEFAULT_TIMEOUT_INTEGER);
    ReflectionTestUtils.setField(feignConfiguration, "xCampaignFeignHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "xCampaignContextPath", DEFAULT_CONTEXT);
    ReflectionTestUtils.setField(feignConfiguration, "partnersEngineHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "partnersEngineContext", DEFAULT_CONTEXT);
    ReflectionTestUtils.setField(feignConfiguration, "partnersEngineTimeout", 3000);
    ReflectionTestUtils.setField(feignConfiguration, "productCategoryBaseFeignHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "productBusinessPartnerFeignHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "productCategoryBaseContextPath", DEFAULT_CONTEXT);
    ReflectionTestUtils.setField(feignConfiguration, "pdtFeignHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "productCategoryBasePort", "8080");
    ReflectionTestUtils.setField(feignConfiguration, "xProductTimeout", 3000);
    ReflectionTestUtils.setField(feignConfiguration, "xProductHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "xProductPort", "8080");
    ReflectionTestUtils.setField(feignConfiguration, "xProductContextPath", DEFAULT_CONTEXT);
    ReflectionTestUtils.setField(feignConfiguration, "inventoryTimeout", 3000);
    ReflectionTestUtils.setField(feignConfiguration, "inventoryHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "inventoryPort", "8080");
    ReflectionTestUtils.setField(feignConfiguration, "inventoryContextPath", DEFAULT_CONTEXT);
    ReflectionTestUtils.setField(feignConfiguration, "productAssemblyTimeout", 3000);
    ReflectionTestUtils.setField(feignConfiguration, "productAssemblyHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "productAssemblyPort", "8080");
    ReflectionTestUtils.setField(feignConfiguration, "productAssemblyContextPath", DEFAULT_CONTEXT);
    ReflectionTestUtils.setField(feignConfiguration, "promoAnalyticsTimeout", 3000);
    ReflectionTestUtils.setField(feignConfiguration, "promoAnalyticsHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "promoAnalyticsPort", "8080");
    ReflectionTestUtils.setField(feignConfiguration, "promoAnalyticsContextPath", DEFAULT_CONTEXT);
    ReflectionTestUtils.setField(feignConfiguration, "businessPartnerTimeout", DEFAULT_TIMEOUT_INTEGER);
    ReflectionTestUtils.setField(feignConfiguration, "productCategoryBaseTimeout", DEFAULT_TIMEOUT_INTEGER);
    ReflectionTestUtils.setField(feignConfiguration, "masterSkuReviewTimeout", DEFAULT_TIMEOUT_INTEGER);
    ReflectionTestUtils.setField(feignConfiguration, "masterSkuReviewPort", "8080");
    ReflectionTestUtils.setField(feignConfiguration, "masterSkuReviewHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "masterSkuReviewPath", DEFAULT_CONTEXT);
    ReflectionTestUtils.setField(feignConfiguration, "productAnalyticsTimeout", DEFAULT_TIMEOUT_INTEGER);
    ReflectionTestUtils.setField(feignConfiguration, "productAnalyticsPort", "8080");
    ReflectionTestUtils.setField(feignConfiguration, "productAnalyticsHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "productAnalyticsPath", DEFAULT_CONTEXT);
    ReflectionTestUtils.setField(feignConfiguration, "priceAnalyticsTimeout", 3000);
    ReflectionTestUtils.setField(feignConfiguration, "priceAnalyticsHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "promoAnalyticsPort", "8080");
    ReflectionTestUtils.setField(feignConfiguration, "promoAnalyticsContextPath", DEFAULT_CONTEXT);
    ReflectionTestUtils.setField(feignConfiguration, "orderHistoryTimeout", 3000);
    ReflectionTestUtils.setField(feignConfiguration, "orderHistoryHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "orderHistoryPort", "8080");
    ReflectionTestUtils.setField(feignConfiguration, "orderHistoryContextPath", DEFAULT_CONTEXT);
    ReflectionTestUtils.setField(feignConfiguration, "cmsTimeout", 3000);
    ReflectionTestUtils.setField(feignConfiguration, "cmsHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "cmsPort", "8080");
    ReflectionTestUtils.setField(feignConfiguration, "cmsContextPath", DEFAULT_CONTEXT);
    ReflectionTestUtils.setField(feignConfiguration, "xgpContextPath", DEFAULT_CONTEXT);
    ReflectionTestUtils.setField(feignConfiguration, "xgpPort", "8080");
    ReflectionTestUtils.setField(feignConfiguration, "xgpFeignHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "xgpTimeout", 3000);
    ReflectionTestUtils.setField(feignConfiguration, "productCategoryPredictionTimeout", 3000);
    ReflectionTestUtils.setField(feignConfiguration, "productCategoryPredictionHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "productCategoryPredictionPort", "8080");
    ReflectionTestUtils.setField(feignConfiguration, "dsProtectedBrandTimeout", 3000);
    ReflectionTestUtils.setField(feignConfiguration, "dsProtectedBrandHost", DEFAULT_HOST);
    ReflectionTestUtils.setField(feignConfiguration, "dsProtectedBrandPort", "8080");

  }

  public void setFeignConfiguration(FeignConfiguration feignConfiguration) {
    this.feignConfiguration = feignConfiguration;
  }

  @Test
  public void testGetter() throws Exception {
    feignConfiguration.getBusinessPartnerContextPath();
    feignConfiguration.getBusinessPartnerHost();
    feignConfiguration.getBusinessPartnerPort();
    feignConfiguration.getProductBusinessPartnerContextPath();
    feignConfiguration.getProductBusinessPartnerHost();
    feignConfiguration.getProductBusinessPartnerPort();
    feignConfiguration.getProductCategoryBaseContextPath();
    feignConfiguration.getProductCategoryBaseHost();
    feignConfiguration.getProductCategoryBasePort();
    feignConfiguration.getXorder2Host();
    feignConfiguration.getXorder2Port();
    feignConfiguration.getXorder2Username();
    feignConfiguration.getXorder2Password();
    feignConfiguration.getXorder2Timeout();
    feignConfiguration.getPdtHost();
    feignConfiguration.getPdtPort();
    feignConfiguration.getPdtContext();
    feignConfiguration.getPdtTimeout();
    feignConfiguration.getRestWebTimout();
    feignConfiguration.getCampaignTimeout();
    feignConfiguration.getxCampaignFeignHost();
    feignConfiguration.getxCampaignContextPath();
    feignConfiguration.xCampaignFeign();
    feignConfiguration.pbpFeign();
    feignConfiguration.productAssemblyFeign();
    feignConfiguration.xProductFeign();
    feignConfiguration.inventoryFeign();
    feignConfiguration.agpQueryFeign();
    feignConfiguration.promoAnalyticsFeign();
    feignConfiguration.getAgpQueryHost();
    feignConfiguration.getAgpQueryPort();
    feignConfiguration.getAgpQueryPath();
    feignConfiguration.getAgpQueryTimeout();
    feignConfiguration.partnersEngineFeign();
    feignConfiguration.xbpFeign();
    feignConfiguration.pcbFeign();
    feignConfiguration.pdtFeign();
    feignConfiguration.masterSkuReviewFeign();
    feignConfiguration.productAnalyticsFeign();
    feignConfiguration.priceAnalyticsFeign();
    feignConfiguration.orderFeign();
    feignConfiguration.orderHistoryFeign();
    feignConfiguration.cmsFeign();
    feignConfiguration.xgpFeign();
    feignConfiguration.productCategoryPredictionFeign();
    feignConfiguration.dsProtectedBrandFeign();
  }
}
