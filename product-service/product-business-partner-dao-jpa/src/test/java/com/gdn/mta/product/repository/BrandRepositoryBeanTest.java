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
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;

public class BrandRepositoryBeanTest {

  @InjectMocks
  private BrandRepositoryBean brandRepositoryBean;

  @Mock
  private PCBFeign pcbFeign;

  private static String BRAND_CODE = "brandCode";
  private static String BRAND_NAME = "brandName";
  private static String REQUEST_ID = "requestId";
  private static String USERNAME = "username";

  private BrandResponse brandResponse;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);

    brandResponse = new BrandResponse();
    brandResponse.setBrandCode(BRAND_CODE);
    brandResponse.setBrandName(BRAND_NAME);
  }


  @Test
  public void findBrandByBrandNameTest() throws Exception {
    GdnRestSingleResponse<BrandResponse> response =
        new GdnRestSingleResponse<>(null, null, true, brandResponse, REQUEST_ID);
    Mockito.when(
        pcbFeign.filterByBrandName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.eq(BRAND_NAME), Mockito.eq(false), Mockito.eq(false))).thenReturn(response);
    BrandResponse brandResponse = brandRepositoryBean.findBrandByBrandName(BRAND_NAME);
    Mockito.verify(pcbFeign)
        .filterByBrandName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.eq(BRAND_NAME), Mockito.eq(false), Mockito.eq(false));
    Assertions.assertNotNull(brandResponse);
    Assertions.assertEquals(BRAND_NAME, brandResponse.getBrandName());
    Assertions.assertEquals(BRAND_CODE, brandResponse.getBrandCode());
  }

  @Test
  public void findBrandByBrandNameTest_expectException() throws Exception {
    GdnRestSingleResponse<BrandResponse> response = new GdnRestSingleResponse<>(null, null, false, null, REQUEST_ID);
    Mockito.when(
        pcbFeign.filterByBrandName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.eq(BRAND_NAME), Mockito.eq(false), Mockito.eq(false))).thenReturn(response);
    try {
      brandRepositoryBean.findBrandByBrandName(BRAND_NAME);
    } catch (Exception e) {
    } finally {
      Mockito.verify(pcbFeign)
          .filterByBrandName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.eq(BRAND_NAME), Mockito.eq(false), Mockito.eq(false));
    }
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pcbFeign);
  }
}