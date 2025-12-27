package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.service.validator.ProductGdnSkuGeneratorValidator;
import com.gdn.x.businesspartner.dto.ProductCounterResponse;

public class ProductGdnSkuGeneratorServiceTest {

  @Mock
  ProductGdnSkuGeneratorValidator validator;

  @Mock
  ProductGdnSkuGenerator gdnSkuGenerator;

  @InjectMocks
  ProductGdnSkuGeneratorServiceBean generatorService;

  private static final String STR_SKU_DIGITS = "5";
  private static final String BP_CODE = "BLI-00106";
  private static final String REQUEST_ID = "test";
  private static final String GDN_SKU = "TEST";
  private GdnRestSingleResponse<ProductCounterResponse> counterResponse;
  private ProductBusinessPartner data;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);

    data = new ProductBusinessPartner();
    data.setBusinessPartnerId(BP_CODE);
    data.setGdnProductSku(GDN_SKU);

    List<ProductItemBusinessPartner> items = new ArrayList<>();
    ProductItemBusinessPartner item1 = new ProductItemBusinessPartner();
    item1.setGdnProductItemSku(GDN_SKU);

    ProductItemBusinessPartner item2 = new ProductItemBusinessPartner();
    item2.setGdnProductItemSku(GDN_SKU);
    items.add(item1);
    items.add(item2);

    data.setProductItemBusinessPartners(items);
  }

  @Test
  public void testGenerateGdnSkuOnProduct() {
    Mockito.when(gdnSkuGenerator.generateProductGdnSku(Mockito.anyString())).thenReturn(GDN_SKU);
    generatorService.generateGdnSkuOnProduct(data, false);
    Mockito.verify(validator).validateGenerateProductGdnSkuData(data);
    Mockito.verify(gdnSkuGenerator).generateProductGdnSku(data.getBusinessPartnerId());
    Mockito.verify(validator, Mockito.times(data.getProductItemBusinessPartners().size()))
        .validateGenerateProductItemGdnSkuData(Mockito.anyInt());
    Mockito.verify(gdnSkuGenerator, Mockito.times(data.getProductItemBusinessPartners().size()))
        .generateProductItemGdnSku(Mockito.eq(GDN_SKU), Mockito.anyInt());
  }

  @Test
  public void testGenerateGdnSkuOnProductMpp() {
    Mockito.when(gdnSkuGenerator.generateProductGdnSku(Mockito.anyString())).thenReturn(GDN_SKU);
    data.getProductItemBusinessPartners().get(0).setGdnProductItemSku(StringUtils.EMPTY);
    data.getProductItemBusinessPartners().get(1).setGdnProductItemSku(StringUtils.EMPTY);
    generatorService.generateGdnSkuOnProduct(data, true);
    Mockito.verify(validator).validateGenerateProductGdnSkuData(data);
    Mockito.verify(gdnSkuGenerator).generateProductGdnSku(data.getBusinessPartnerId());
    Mockito.verify(validator, Mockito.times(data.getProductItemBusinessPartners().size()))
        .validateGenerateProductItemGdnSkuData(Mockito.anyInt());
    Mockito.verify(gdnSkuGenerator, Mockito.times(data.getProductItemBusinessPartners().size()))
        .generateProductItemGdnSku(Mockito.eq(GDN_SKU), Mockito.anyInt());
  }

  @Test
  public void testGenerateGdnSkuOnProductMpp1() {
    Mockito.when(gdnSkuGenerator.generateProductGdnSku(Mockito.anyString())).thenReturn(GDN_SKU);
    generatorService.generateGdnSkuOnProduct(data, true);
    Mockito.verify(validator).validateGenerateProductGdnSkuData(data);
    Mockito.verify(gdnSkuGenerator).generateProductGdnSku(data.getBusinessPartnerId());
    Mockito.verify(validator).validateGenerateProductItemGdnSkuData(Mockito.anyInt());
    Mockito.verify(gdnSkuGenerator).generateProductItemGdnSku(Mockito.eq(GDN_SKU), Mockito.anyInt());
  }

  @Test
  public void testConvertToGeneratedGdnSkus() {
    generatorService.convertToGeneratedGdnSkus(data);
  }

  @Test
  public void testConvertToGeneratedGdnSkus_Null() {
    generatorService.convertToGeneratedGdnSkus(null);
  }

  @Test
  public void testConvertToGeneratedGdnSkus_NullItems() {
    data.setProductItemBusinessPartners(null);
    generatorService.convertToGeneratedGdnSkus(data);
  }


  @AfterEach
  public void finalizeTest() {
    Mockito.verifyNoMoreInteractions(validator);
    Mockito.verifyNoMoreInteractions(gdnSkuGenerator);
  }
}
