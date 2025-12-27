package com.gdn.mta.product.repository;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;

public class BrandWipRepositoryBeanTest {

  @InjectMocks
  private BrandWipRepositoryBean brandWipRepositoryBean;

  @Mock
  private PCBFeign pcbFeign;

  private static String BRAND_CODE = "brandCode";
  private static String BRAND_NAME = "brandName";
  private static String REQUEST_ID = "requestId";
  private static String USERNAME = "username";
  private static String BUSINESS_PARTNER_CODE = "businessPartnerCode";

  private BrandWipResponse brandWipResponse;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);

    brandWipResponse = new BrandWipResponse();
    brandWipResponse.setBrandCode(BRAND_CODE);
    brandWipResponse.setBrandName(BRAND_NAME);
  }

  @Test
  public void findBrandByBrandNameTest() throws Exception {
    GdnRestSingleResponse<BrandWipResponse> response =
        new GdnRestSingleResponse<>(null, null, true, brandWipResponse, REQUEST_ID);
    Mockito.when(pcbFeign.findBrandWipByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.eq(BRAND_NAME),
        Mockito.eq(BUSINESS_PARTNER_CODE))).thenReturn(response);
    BrandWipResponse brandWipResponse = brandWipRepositoryBean.findBrandWipByBrandNameAndBusinessPartnerCode(BRAND_NAME, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbFeign)
        .findBrandWipByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.eq(BRAND_NAME), Mockito.eq(BUSINESS_PARTNER_CODE));
    Assertions.assertNotNull(brandWipResponse);
    Assertions.assertEquals(BRAND_NAME, brandWipResponse.getBrandName());
    Assertions.assertEquals(BRAND_CODE, brandWipResponse.getBrandCode());
  }

  @Test
  public void findBrandByBrandNameTest_expectException() throws Exception {
    GdnRestSingleResponse<BrandWipResponse> response = new GdnRestSingleResponse<>(null, null, false, null, REQUEST_ID);
    Mockito.when(pcbFeign.findBrandWipByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.eq(BRAND_NAME),
        Mockito.eq(BUSINESS_PARTNER_CODE))).thenReturn(response);
    try {
      brandWipRepositoryBean.findBrandWipByBrandNameAndBusinessPartnerCode(BRAND_NAME, BUSINESS_PARTNER_CODE);
    } catch (Exception e) {
    } finally {
      Mockito.verify(pcbFeign)
          .findBrandWipByBrandNameAndBusinessPartnerCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.eq(BRAND_NAME), Mockito.eq(BUSINESS_PARTNER_CODE));
    }
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pcbFeign);
  }
}
