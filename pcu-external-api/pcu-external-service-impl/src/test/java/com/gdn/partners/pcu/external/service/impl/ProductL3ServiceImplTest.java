package com.gdn.partners.pcu.external.service.impl;

import com.gda.mta.product.dto.ProductL3CommonImageResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.web.model.response.ProductL3DetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3DetailsResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailMapResponse;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailResponse;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;


public class ProductL3ServiceImplTest {

  @InjectMocks
  private ProductL3ServiceImpl productL3Service;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private PCBFeign pcbFeign;

  public static final String PURCHASE_TERM = "PURCHASE_TERM";
  private static final String EMAIL = "EMAIL";
  private static final String CATEGORY_ID = "CATEGORY_ID";
  private static final String BUSINESSPARTNER_CODE = "DR6-44662";
  private static final String REQUEST_ID = "requestId";
  private static final String PRODUCT_SKU_1 = "DR6-44662-50926";
  private static final String STORE_ID = "10001";
  private static final String SIZE_CHART_CODE = "sizeChartCode";
  private static final String SIZE_CHART_NAME = "sizeChartName";
  private static final String GDN_SKU = "GDN_SKU";
  private ProfileResponse profileResponse;
  private CompanyDTO company;


  @BeforeEach
  public void setUp(){
    MockitoAnnotations.initMocks(this);
    company = new CompanyDTO();
    company.setBusinessPartnerName(BUSINESSPARTNER_CODE);
    company.setMerchantFlag(true);
    company.setInternationalFlag(false);
    company.setMerchantType("CM");
    company.setOfflineToOnlineFlag(true);
    company.setInventoryFulfillment("BL");
    company.setOfflineToOnlineFlag(true);
    company.setEmail(EMAIL);
    company.setPurchaseTerm(PURCHASE_TERM);

    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    profileResponse.setMerchantStatus("ACTIVE");
    profileResponse.setCompany(company);
  }


  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(pbpFeign);
    verifyNoMoreInteractions(businessPartnerService, pcbFeign);
  }

  @Test
  public void getL3DetailByProductSkuTest() throws Exception {
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.getL3ProductDetailsByProductSku(PRODUCT_SKU_1, false, false))
      .thenReturn(new GdnRestSingleResponse<>(new ProductL3DetailsResponse(), REQUEST_ID));
    this.productL3Service.getL3DetailsByProductSku(STORE_ID, true,
      BUSINESSPARTNER_CODE, false, PRODUCT_SKU_1, false);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(pbpFeign).getL3ProductDetailsByProductSku(PRODUCT_SKU_1, false, false);
  }

  @Test
  public void getL3DetailByProductSkuSwitchOnTest() {
    ReflectionTestUtils.setField(productL3Service, "overrideProductEditableFlagBasedOnSynchronize", true);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    ProductL3DetailsResponse productL3DetailsResponse = new ProductL3DetailsResponse();
    productL3DetailsResponse.setProductEditable(true);
    productL3DetailsResponse.setSynchronize(true);
    when(this.pbpFeign.getL3ProductDetailsByProductSku(PRODUCT_SKU_1, false, false))
        .thenReturn(new GdnRestSingleResponse<>(productL3DetailsResponse, REQUEST_ID));
    ProductL3DetailWebResponse productL3DetailWebResponse =
        this.productL3Service.getL3DetailsByProductSku(STORE_ID, true, BUSINESSPARTNER_CODE, false, PRODUCT_SKU_1,
            false);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(pbpFeign).getL3ProductDetailsByProductSku(PRODUCT_SKU_1, false, false);
    Assertions.assertTrue(productL3DetailWebResponse.isProductEditable());
  }

  @Test
  public void getL3DetailByProductSkuSwitchOnSyncFalseTest() {
    ReflectionTestUtils.setField(productL3Service, "overrideProductEditableFlagBasedOnSynchronize", true);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    ProductL3DetailsResponse productL3DetailsResponse = new ProductL3DetailsResponse();
    productL3DetailsResponse.setProductEditable(true);
    productL3DetailsResponse.setSynchronize(false);
    when(this.pbpFeign.getL3ProductDetailsByProductSku(PRODUCT_SKU_1, false, false))
        .thenReturn(new GdnRestSingleResponse<>(productL3DetailsResponse, REQUEST_ID));
    ProductL3DetailWebResponse productL3DetailWebResponse =
        this.productL3Service.getL3DetailsByProductSku(STORE_ID, true, BUSINESSPARTNER_CODE, false, PRODUCT_SKU_1,
            false);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(pbpFeign).getL3ProductDetailsByProductSku(PRODUCT_SKU_1, false, false);
    Assertions.assertFalse(productL3DetailWebResponse.isProductEditable());
  }

  @Test
  void getL3DetailsByProductSkuExceptionTest() {
    profileResponse.setMerchantStatus("INACTIVE");
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
        profileResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productL3Service.getL3DetailsByProductSku(STORE_ID, true, BUSINESSPARTNER_CODE,
              false, PRODUCT_SKU_1, false));
    } finally {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(pbpFeign, times(0)).getL3ProductDetailsByProductSku(GDN_SKU, false, false);
    }
  }


  @Test
  public void getL3DetailsByProductSku_nullBusinessPartnerExceptionTest() {
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productL3Service.getL3DetailsByProductSku(STORE_ID, true, BUSINESSPARTNER_CODE,
              false, PRODUCT_SKU_1, false));
    } finally {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(pbpFeign, times(0)).getL3ProductDetailsByProductSku(GDN_SKU, false, false);
    }
  }

  @Test
  public void getL3DetailsByProductSkuCogsViewableTest() throws Exception {
    ReflectionTestUtils.setField(productL3Service,"resizeImageRemoval",true);
    ReflectionTestUtils.setField(productL3Service,"resizeImagePathList","resize/,ecommerece/");
    profileResponse.getCompany().setPurchaseTerm("PO");
    ProductL3DetailsResponse productLevel3DetailResponse = new ProductL3DetailsResponse();
    productLevel3DetailResponse.setCategoryId(CATEGORY_ID);
    List<ProductL3CommonImageResponse> productL3CommonImageResponsesList = new ArrayList<>();
    ProductL3CommonImageResponse productL3CommonImageResponse = new ProductL3CommonImageResponse();
    ProductL3CommonImageResponse productL3CommonImageResponse1 = new ProductL3CommonImageResponse();
    productL3CommonImageResponse.setLocationPath("resize/path");
    productL3CommonImageResponse1.setLocationPath("path");
    productL3CommonImageResponsesList.add(productL3CommonImageResponse);
    productL3CommonImageResponsesList.add(productL3CommonImageResponse1);
    productLevel3DetailResponse.setCommonImages(productL3CommonImageResponsesList);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.getL3ProductDetailsByProductSku(PRODUCT_SKU_1, false, false))
      .thenReturn(new GdnRestSingleResponse<>(productLevel3DetailResponse, REQUEST_ID));
    ProductL3DetailWebResponse productL3DetailWebResponse =
      this.productL3Service.getL3DetailsByProductSku(STORE_ID, true,
        BUSINESSPARTNER_CODE, false, PRODUCT_SKU_1, false);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(pbpFeign).getL3ProductDetailsByProductSku(PRODUCT_SKU_1, false, false);
    assertTrue(productL3DetailWebResponse.getCogsViewable());
    assertEquals(1,productL3DetailWebResponse.getCommonImages().size());
  }

  @Test
  public void getL3DetailsByProductSkuCogsViewableReSizePathRemovaSwitchOnAndSizeChartPhase2_Switch_On_Test() throws Exception {
    ReflectionTestUtils.setField(productL3Service,"resizeImageRemoval",true);
    ReflectionTestUtils.setField(productL3Service,"sizeChartAdditionForProduct",true);
    profileResponse.getCompany().setPurchaseTerm("PO");
    ProductL3DetailsResponse productLevel3DetailResponse = new ProductL3DetailsResponse();
    productLevel3DetailResponse.setCategoryId(CATEGORY_ID);
    productLevel3DetailResponse.setSizeChartCode(SIZE_CHART_CODE);
    productLevel3DetailResponse.setVideoUrl(SIZE_CHART_CODE);
    productLevel3DetailResponse.setUrl(SIZE_CHART_CODE);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.getL3ProductDetailsByProductSku(PRODUCT_SKU_1, false, false))
        .thenReturn(new GdnRestSingleResponse<>(productLevel3DetailResponse, REQUEST_ID));
    BasicSizeChartDetailResponse basicSizeChartDetailResponse = new BasicSizeChartDetailResponse();
    basicSizeChartDetailResponse.setSizeChartName(SIZE_CHART_NAME);
    basicSizeChartDetailResponse.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    when(this.pcbFeign.getBasicSizeChartDetails(List.of(SIZE_CHART_CODE))).thenReturn(
        new GdnRestSingleResponse<>(new BasicSizeChartDetailMapResponse(
            Map.of(SIZE_CHART_CODE, basicSizeChartDetailResponse)), REQUEST_ID));
    ProductL3DetailWebResponse productL3DetailWebResponse =
        this.productL3Service.getL3DetailsByProductSku(STORE_ID, true,
            BUSINESSPARTNER_CODE, false, PRODUCT_SKU_1, false);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(pbpFeign).getL3ProductDetailsByProductSku(PRODUCT_SKU_1, false, false);
    verify(pcbFeign).getBasicSizeChartDetails(List.of(SIZE_CHART_CODE));
    assertTrue(productL3DetailWebResponse.getCogsViewable());
    assertEquals(StringUtils.EMPTY, productL3DetailWebResponse.getUrl());
  }

  @Test
  public void getL3DetailsByProductSkuCogsViewableReSizePathRemovaSwitchOnAndSizeChartPhase2_Switch_On_AndNullPCBResponse_Test()
      throws Exception {
    ReflectionTestUtils.setField(productL3Service, "resizeImageRemoval", true);
    ReflectionTestUtils.setField(productL3Service, "sizeChartAdditionForProduct", true);
    profileResponse.getCompany().setPurchaseTerm("PO");
    ProductL3DetailsResponse productLevel3DetailResponse = new ProductL3DetailsResponse();
    productLevel3DetailResponse.setCategoryId(CATEGORY_ID);
    productLevel3DetailResponse.setSizeChartCode(SIZE_CHART_CODE);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
        profileResponse);
    when(this.pbpFeign.getL3ProductDetailsByProductSku(PRODUCT_SKU_1, false, false)).thenReturn(
        new GdnRestSingleResponse<>(productLevel3DetailResponse, REQUEST_ID));
    Map<String, BasicSizeChartDetailResponse> map = new HashMap<>();
    map.put(SIZE_CHART_CODE, null);
    when(this.pcbFeign.getBasicSizeChartDetails(List.of(SIZE_CHART_CODE))).thenReturn(
        new GdnRestSingleResponse<>(
            new BasicSizeChartDetailMapResponse(map), REQUEST_ID));
    ProductL3DetailWebResponse productL3DetailWebResponse =
        this.productL3Service.getL3DetailsByProductSku(STORE_ID, true, BUSINESSPARTNER_CODE, false,
            PRODUCT_SKU_1, false);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(pbpFeign).getL3ProductDetailsByProductSku(PRODUCT_SKU_1, false, false);
    verify(pcbFeign).getBasicSizeChartDetails(List.of(SIZE_CHART_CODE));
    assertTrue(productL3DetailWebResponse.getCogsViewable());
  }

  @Test
  public void getL3DetailsByProductSkuCogsViewableReSizePathRemovaSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(productL3Service,"resizeImageRemoval",true);
    profileResponse.getCompany().setPurchaseTerm("PO");
    ProductL3DetailsResponse productLevel3DetailResponse = new ProductL3DetailsResponse();
    productLevel3DetailResponse.setCategoryId(CATEGORY_ID);
    productLevel3DetailResponse.setSizeChartCode(SIZE_CHART_CODE);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.getL3ProductDetailsByProductSku(PRODUCT_SKU_1, false, false))
        .thenReturn(new GdnRestSingleResponse<>(productLevel3DetailResponse, REQUEST_ID));
    BasicSizeChartDetailResponse basicSizeChartDetailResponse = new BasicSizeChartDetailResponse();
    basicSizeChartDetailResponse.setSizeChartName(SIZE_CHART_NAME);
    basicSizeChartDetailResponse.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
//    when(this.pcbFeign.getBasicSizeChartDetails(SIZE_CHART_CODE)).thenReturn(
//        new GdnRestSingleResponse<>(basicSizeChartDetailResponse, REQUEST_ID));
    ProductL3DetailWebResponse productL3DetailWebResponse =
        this.productL3Service.getL3DetailsByProductSku(STORE_ID, true,
            BUSINESSPARTNER_CODE, false, PRODUCT_SKU_1, false);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(pbpFeign).getL3ProductDetailsByProductSku(PRODUCT_SKU_1, false, false);
    assertTrue(productL3DetailWebResponse.getCogsViewable());
  }

  @Test
  public void getL3DetailByProductSkuNeedRevisionTest() throws Exception {
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.getL3ProductDetailsByProductSku(PRODUCT_SKU_1, true, false))
        .thenReturn(new GdnRestSingleResponse<>(new ProductL3DetailsResponse(), REQUEST_ID));
    this.productL3Service.getL3DetailsByProductSku(STORE_ID, true,
        BUSINESSPARTNER_CODE, true, PRODUCT_SKU_1, false);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
    verify(pbpFeign).getL3ProductDetailsByProductSku(PRODUCT_SKU_1, true, false);
  }

}
