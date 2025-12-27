package com.gdn.mta.product.service;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.service.util.ProductGdnSkuGeneratorUtil;
import com.gdn.partners.pbp.outbound.xbp.feign.XbpFeign;
import com.gdn.x.businesspartner.dto.ProductCounterResponse;

public class ProductGdnSkuGeneratorTest {

  @Mock
  XbpFeign xbpFeign;

  @Mock
  ApplicationProperties appProperties;

  @Mock
  ProductGdnSkuGeneratorUtil util;

  @InjectMocks
  ProductGdnSkuGeneratorBean generatorService;

  private static final String STR_SKU_DIGITS = "5";
  private static final String BP_CODE = "BLI-00106";
  private static final String REQUEST_ID = "test";
  private static final String GDN_SKU = "Smoke on the Water";
  private static final int ITEM_NO_1 = 1;
  private static final int ITEM_NO_0 = 0;
  private GdnRestSingleResponse<ProductCounterResponse> counterResponse;
  private ProductBusinessPartner data;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    ProductCounterResponse counterResponseContent = new ProductCounterResponse();
    counterResponseContent.setCounter(10L);
    counterResponseContent.setBusinessPartnerCode(BP_CODE);
    counterResponse =
        new GdnRestSingleResponse<ProductCounterResponse>(counterResponseContent, REQUEST_ID);

    data = new ProductBusinessPartner();
    data.setBusinessPartnerId(BP_CODE);

    Mockito.when(appProperties.getSkuDigits()).thenReturn(STR_SKU_DIGITS);
    Mockito.when(
        xbpFeign.incrementAndGetProductCounter(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),
            Mockito.anyString())).thenReturn(counterResponse);
    Mockito.when(
        util.appendWithSerial(Mockito.anyString(), Mockito.any(Number.class), Mockito.anyInt()))
        .thenReturn(GDN_SKU);
  }

  @Test
  public void testGenerateProductGdnSku() throws Exception {
    generatorService.generateProductGdnSku(data.getBusinessPartnerId());
    Mockito.verify(xbpFeign).incrementAndGetProductCounter(
        Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString());
    Mockito.verify(appProperties).getSkuDigits();
    Mockito.verify(util).appendWithSerial(Mockito.anyString(), Mockito.any(Number.class),
        Mockito.anyInt());
  }

  @Test
  public void testGenerateProductGdnSku_Error() throws Exception {
    try {
      Mockito.when(
          xbpFeign.incrementAndGetProductCounter(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString())).thenThrow(new RuntimeException("error nih"));
      Assertions.assertThrows(Exception.class, () -> {
        generatorService.generateProductGdnSku(data.getBusinessPartnerId());
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(xbpFeign).incrementAndGetProductCounter(
        Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString());
  }

  @Test
  public void testGenerateProductItemGdnSku1() throws Exception {
    generatorService.generateProductItemGdnSku(GDN_SKU, ITEM_NO_1);
    Mockito.verify(appProperties).getSkuDigits();
    Mockito.verify(util).appendWithSerial(Mockito.anyString(), Mockito.any(Number.class),
        Mockito.anyInt());
  }

  @Test
  public void testGenerateProductItemGdnSku0() throws Exception {
    generatorService.generateProductItemGdnSku(GDN_SKU, ITEM_NO_0);
    Mockito.verify(appProperties).getSkuDigits();
    Mockito.verify(util).appendWithSerial(Mockito.anyString(), Mockito.any(Number.class),
        Mockito.anyInt());
  }

  @AfterEach
  public void finalizeTest() {
    Mockito.verifyNoMoreInteractions(xbpFeign);
    Mockito.verifyNoMoreInteractions(appProperties);
    Mockito.verifyNoMoreInteractions(util);
  }
}
