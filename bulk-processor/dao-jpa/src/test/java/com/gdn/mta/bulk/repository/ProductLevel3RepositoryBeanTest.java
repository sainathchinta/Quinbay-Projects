package com.gdn.mta.bulk.repository;

import com.gda.mta.product.dto.BrandAndCategoryItemSummaryRequest;
import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.ItemBulkArchiveRequest;
import com.gda.mta.product.dto.ProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.SuspensionProductRequestList;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.mta.bulk.feignConfig.PBPFeign;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.models.AuditTrailInfo;
import com.gdn.mta.bulk.models.download.CampaignProductDownloadRequest;
import com.gdn.mta.product.util.SingleValueResponse;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import com.google.common.collect.Lists;
import org.apache.commons.collections.CollectionUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.slf4j.MDC;
import org.springframework.data.domain.PageRequest;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyInt;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

public class ProductLevel3RepositoryBeanTest {

  private static final String FOO = "foo";
  private static final String BAR = "bar";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "TOT-15014";
  private static final String DEFAULT_GDN_SKU = "MTA-0000001-00001-0001";
  private static final String USERNAME = "username@mail.com";
  private static final String REQUEST_ID = "12345";
  private static final String SKU_CODE = "SKU-1234";
  private static final String BP_CODE = "TOA-4567";
  private static final String CLIENT_HOST = "localhost";
  private static final String MERCHANT_CODE = "merchant-code";
  private static final List<String> BRANDS = Lists.newArrayList(FOO, BAR);
  private static final List<String> CATEGORIES = Lists.newArrayList(BAR, FOO);
  private static final String ITEM_SKU = "item-sku";
  private static final String KEYWORD = "keyword";
  private static final VerificationMode AT_LEAST_ONE = times(1);
  private static final int PAGE_SIZE = 10;
  private static final int PAGE_NUMBER = 0;
  private static final String SYSTEM_USER = "Merchant API";
  private AuditTrailInfo auditTrailInfo;
  private SuspensionProductRequest suspensionProductRequest;
  private SuspensionProductRequestList suspensionProductRequestList;

  @Captor
  ArgumentCaptor<BrandAndCategoryItemSummaryRequest> brandAndCategoryItemSummaryRequestArgumentCaptor;

  @Captor
  ArgumentCaptor<ItemBulkArchiveRequest> bulkArchiveRequestArgumentCaptor;

  @Mock
  PBPFeign pbpFeign;

  @InjectMocks
  ProductLevel3RepositoryBean productLevel3RepositoryBean;

  private GdnRestSingleResponse<ProductLevel3Response> updateResponse;
  private GdnRestSingleResponse<SingleValueResponse> validateProductSKU;

  @AfterEach
  public void finalizeTest() throws Exception {
    verifyNoMoreInteractions(pbpFeign);
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);

    updateResponse = new GdnRestSingleResponse<ProductLevel3Response>(null, null, true, null, null);
    SingleValueResponse singleValue = new SingleValueResponse(BP_CODE);
    validateProductSKU = new GdnRestSingleResponse<SingleValueResponse>(singleValue, REQUEST_ID);

    auditTrailInfo = new AuditTrailInfo();
    auditTrailInfo.setRequestId(DEFAULT_REQUEST_ID);
    auditTrailInfo.setUsername(USERNAME);
    auditTrailInfo.setRemoteAddress(CLIENT_HOST);
    auditTrailInfo.setBusinessPartnerCode(BP_CODE);

    suspensionProductRequest = new SuspensionProductRequest();
    suspensionProductRequestList = new SuspensionProductRequestList();
    suspensionProductRequestList.setSuspensionProductRequestList(Arrays.asList(suspensionProductRequest));
  }


  @Test
  public void testFindSummaryByFilterForBulkDownload_Success() throws Exception {
    BulkDownloadProductLevel3Response bulkDownloadProductLevel3Response =
        getBulkDownloadProductLevel3Response();
    GdnRestSingleResponse<BulkDownloadProductLevel3Response> response =
        new GdnRestSingleResponse<>(bulkDownloadProductLevel3Response, DEFAULT_REQUEST_ID);
    when(pbpFeign.filterSummaryForBulkDownload(anyString(), anyString(), anyString(), anyString(),
        anyString(), anyString(), anyInt(), anyInt(),
        (ProductLevel3SummaryRequest) any())).thenReturn(response);
    BulkDownloadProductLevel3Response level3Response = productLevel3RepositoryBean
        .findSummaryByFilterForBulkDownload(DEFAULT_BUSINESS_PARTNER_CODE,  PageRequest.of(0, 10),
            new ProductLevel3SummaryRequest());
    verify(pbpFeign).filterSummaryForBulkDownload(anyString(), anyString(), anyString(),
        anyString(), anyString(), anyString(), anyInt(), anyInt(),
        (ProductLevel3SummaryRequest) any());
    Assertions.assertNotNull(level3Response.getProductLevel3SummaryResponses().get(0).getItemName());
  }

  @Test
  public void testFindSummaryByFilterForBulkDownload_Success_1() throws Exception {
    BulkDownloadProductLevel3Response bulkDownloadProductLevel3Response =
        getBulkDownloadProductLevel3Response();
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    GdnRestSingleResponse<BulkDownloadProductLevel3Response> response =
        new GdnRestSingleResponse<>(bulkDownloadProductLevel3Response, DEFAULT_REQUEST_ID);
    when(pbpFeign.filterSummaryForBulkDownload(anyString(), anyString(), anyString(), anyString(),
        anyString(), anyString(), anyInt(), anyInt(),
        (ProductLevel3SummaryRequest) any())).thenReturn(response);
    BulkDownloadProductLevel3Response level3Response = productLevel3RepositoryBean
        .findSummaryByFilterForBulkDownload(DEFAULT_BUSINESS_PARTNER_CODE,  PageRequest.of(0, 10),
            new ProductLevel3SummaryRequest());
    verify(pbpFeign).filterSummaryForBulkDownload(anyString(), anyString(), anyString(),
        anyString(), anyString(), anyString(), anyInt(), anyInt(),
        (ProductLevel3SummaryRequest) any());
    Assertions.assertNotNull(level3Response.getProductLevel3SummaryResponses().get(0).getItemName());

  }

  @Test
  public void testFindSummaryByFilterForBulkDownload_Success_2() throws Exception {
    BulkDownloadProductLevel3Response bulkDownloadProductLevel3Response =
        getBulkDownloadProductLevel3Response();
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    GdnRestSingleResponse<BulkDownloadProductLevel3Response> response =
        new GdnRestSingleResponse<>(bulkDownloadProductLevel3Response, DEFAULT_REQUEST_ID);
    when(pbpFeign.filterSummaryForBulkDownload(anyString(), anyString(), anyString(), anyString(),
        anyString(), anyString(), anyInt(), anyInt(),
        (ProductLevel3SummaryRequest) any())).thenReturn(response);
    BulkDownloadProductLevel3Response level3Response = productLevel3RepositoryBean
        .findSummaryByFilterForBulkDownload(DEFAULT_BUSINESS_PARTNER_CODE,  PageRequest.of(0, 10),
            new ProductLevel3SummaryRequest());
    verify(pbpFeign).filterSummaryForBulkDownload(anyString(), anyString(), anyString(),
        anyString(), anyString(), anyString(), anyInt(), anyInt(),
        (ProductLevel3SummaryRequest) any());
    Assertions.assertNotNull(level3Response.getProductLevel3SummaryResponses().get(0).getItemName());
  }

  @Test
  public void testFindSummaryByFilterForBulkDownload_Failed() throws Exception {
    GdnRestSingleResponse<BulkDownloadProductLevel3Response> response = new GdnRestSingleResponse<>(
        "Read Timeout", ErrorCategory.UNSPECIFIED.getCode(), false, null, DEFAULT_REQUEST_ID);
    when(pbpFeign.filterSummaryForBulkDownload(anyString(), anyString(), anyString(), anyString(),
        anyString(), anyString(), anyInt(), anyInt(),
        (ProductLevel3SummaryRequest) any())).thenReturn(response);
    try {
      Assertions.assertThrows(Exception.class,
          () -> productLevel3RepositoryBean.findSummaryByFilterForBulkDownload(
              DEFAULT_BUSINESS_PARTNER_CODE, PageRequest.of(0, 10),
              new ProductLevel3SummaryRequest()));
    } catch (Exception e) {} finally {
      verify(pbpFeign).filterSummaryForBulkDownload(anyString(), anyString(), anyString(),
          anyString(), anyString(), anyString(), anyInt(), anyInt(),
          (ProductLevel3SummaryRequest) any());
    }
  }

  @Test
  public void testFindSummaryByCategoryAndBrand() throws Exception {
    CampaignItemSummaryRequest campaignItemSummaryRequest = new CampaignItemSummaryRequest();
    campaignItemSummaryRequest.setMerchantCode(MERCHANT_CODE);
    campaignItemSummaryRequest.setCategories(CATEGORIES);
    campaignItemSummaryRequest.setBrands(BRANDS);
    campaignItemSummaryRequest.setItemSku(ITEM_SKU);
    campaignItemSummaryRequest.setKeyword(KEYWORD);
    CampaignProductDownloadRequest request = new CampaignProductDownloadRequest();
    request.setRequestId(REQUEST_ID);
    request.setCampaignItemSummaryRequest(campaignItemSummaryRequest);
    doReturn(new GdnRestListResponse<>(Lists.newArrayList(new ProductLevel3SummaryResponse()),
        new PageMetaData(PAGE_SIZE, PAGE_NUMBER, 100), REQUEST_ID)).when(pbpFeign)
        .filterSummaryByCategoryAndBrand(anyString(), anyString(), anyString(), anyString(),
            eq(null), eq(PAGE_NUMBER), eq(PAGE_SIZE), eq(null), eq(null),
            brandAndCategoryItemSummaryRequestArgumentCaptor.capture());
    List<ProductLevel3SummaryResponse> responses =
        productLevel3RepositoryBean.findSummaryByCategoryAndBrand(PAGE_NUMBER, PAGE_SIZE, request);
    verify(pbpFeign).filterSummaryByCategoryAndBrand(anyString(), anyString(), anyString(),
        anyString(), eq(null), eq(PAGE_NUMBER), eq(PAGE_SIZE), eq(null),
        eq(null), brandAndCategoryItemSummaryRequestArgumentCaptor.capture());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(responses));
    BrandAndCategoryItemSummaryRequest brandAndCategoryItemSummaryRequestCapture =
        brandAndCategoryItemSummaryRequestArgumentCaptor.getValue();
//    Assertions.assertTrue(
//        brandAndCategoryItemSummaryRequestCapture.getBrands().contains(Arrays.asList(FOO, BAR)));
    Assertions.assertEquals(2, brandAndCategoryItemSummaryRequestCapture.getBrands().size());
//    Assertions.assertTrue(
//        brandAndCategoryItemSummaryRequestCapture.getCategories().contains(Arrays.asList(FOO, BAR)));
    Assertions.assertEquals(2, brandAndCategoryItemSummaryRequestCapture.getCategories().size());
    Assertions.assertEquals(brandAndCategoryItemSummaryRequestCapture.getMerchantCode(), MERCHANT_CODE);
  }

  @Test
  public void testFindSummaryByCategoryAndBrand_nullValue() throws Exception {
    CampaignItemSummaryRequest campaignItemSummaryRequest = new CampaignItemSummaryRequest();
    campaignItemSummaryRequest.setMerchantCode(MERCHANT_CODE);
    campaignItemSummaryRequest.setCategories(CATEGORIES);
    campaignItemSummaryRequest.setBrands(BRANDS);
    campaignItemSummaryRequest.setItemSku(ITEM_SKU);
    campaignItemSummaryRequest.setKeyword(KEYWORD);
    CampaignProductDownloadRequest request = new CampaignProductDownloadRequest();
    request.setRequestId(REQUEST_ID);
    request.setCampaignItemSummaryRequest(campaignItemSummaryRequest);
    doReturn(
        new GdnRestListResponse<>(null, new PageMetaData(PAGE_SIZE, PAGE_NUMBER, 100), REQUEST_ID)).when(
            pbpFeign)
        .filterSummaryByCategoryAndBrand(anyString(), anyString(), anyString(), anyString(),
            eq(null),
                eq(PAGE_NUMBER), eq(PAGE_SIZE), eq(null), eq(null),
                brandAndCategoryItemSummaryRequestArgumentCaptor.capture());
    List<ProductLevel3SummaryResponse> responses =
        productLevel3RepositoryBean.findSummaryByCategoryAndBrand(PAGE_NUMBER, PAGE_SIZE, request);
    verify(pbpFeign).filterSummaryByCategoryAndBrand(anyString(),anyString(),anyString(),anyString(), eq(null),
        eq(PAGE_NUMBER), eq(PAGE_SIZE), eq(null), eq(null),
        brandAndCategoryItemSummaryRequestArgumentCaptor.capture());
    Assertions.assertTrue(CollectionUtils.isEmpty(responses));
    BrandAndCategoryItemSummaryRequest brandAndCategoryItemSummaryRequestCapture =
        brandAndCategoryItemSummaryRequestArgumentCaptor.getValue();
    Assertions.assertFalse(
        brandAndCategoryItemSummaryRequestCapture.getBrands().contains(Arrays.asList(FOO, BAR)));
    Assertions.assertEquals(2, brandAndCategoryItemSummaryRequestCapture.getBrands().size());
    Assertions.assertFalse(
        brandAndCategoryItemSummaryRequestCapture.getCategories().contains(Arrays.asList(FOO, BAR)));
    Assertions.assertEquals(2, brandAndCategoryItemSummaryRequestCapture.getCategories().size());
    Assertions.assertEquals(brandAndCategoryItemSummaryRequestCapture.getMerchantCode(), MERCHANT_CODE);
  }

  private BulkDownloadProductLevel3Response getBulkDownloadProductLevel3Response() {
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    productLevel3SummaryResponse.setItemName("test_item");
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    BulkDownloadProductLevel3Response bulkDownloadProductLevel3Response =
        new BulkDownloadProductLevel3Response();
    bulkDownloadProductLevel3Response
        .setProductLevel3SummaryResponses(productLevel3SummaryResponses);
    return bulkDownloadProductLevel3Response;
  }

  @Test
  public void doBulkSuspensionProductsActionsTest_WithBulkResponseAsNull() throws Exception {
    GdnRestListResponse<SuspensionProductResponse> response =
        new GdnRestListResponse<>(null, new PageMetaData(), DEFAULT_REQUEST_ID);
    when(pbpFeign.doBulkSuspensionProductsActions( Mockito.anyString(),  Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(SuspensionProductRequestList.class))).thenReturn(response);
    List<SuspensionProductResponse> failedProducts = productLevel3RepositoryBean
        .doBulkSuspensionProductsActions(Arrays.asList(suspensionProductRequest), DEFAULT_REQUEST_ID, USERNAME);
    verify(pbpFeign).doBulkSuspensionProductsActions( Mockito.anyString(),  Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(),  Mockito.anyString(),
        eq(suspensionProductRequestList));
    Assertions.assertNull(failedProducts);
  }

  @Test
  public void doBulkSuspensionProductsActionsTest_WithBulkResponse() throws Exception {
    GdnRestListResponse<SuspensionProductResponse> response =
        new GdnRestListResponse<>(Arrays.asList(new SuspensionProductResponse()), new PageMetaData(),
            DEFAULT_REQUEST_ID);
    when(pbpFeign.doBulkSuspensionProductsActions(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(SuspensionProductRequestList.class))).thenReturn(response);
    List<SuspensionProductResponse> failedProducts = productLevel3RepositoryBean
        .doBulkSuspensionProductsActions(Arrays.asList(suspensionProductRequest), DEFAULT_REQUEST_ID, USERNAME);
    verify(pbpFeign).doBulkSuspensionProductsActions( Mockito.anyString(),  Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        eq(suspensionProductRequestList));
    Assertions.assertNotNull(failedProducts);
  }

  @Test
  public void doBulkSuspensionProductsActionsTest_WithException() throws Exception {
    GdnRestListResponse<SuspensionProductResponse> response =
        new GdnRestListResponse<>(null, null, false, DEFAULT_REQUEST_ID);
    when(
        pbpFeign.doBulkSuspensionProductsActions(anyString(), anyString(), anyString(), anyString(),
            anyString(), Mockito.any(SuspensionProductRequestList.class))).thenReturn(response);
    try {
      List<SuspensionProductResponse> failedProducts = productLevel3RepositoryBean
          .doBulkSuspensionProductsActions(Arrays.asList(suspensionProductRequest), DEFAULT_REQUEST_ID, USERNAME);
    } catch (Exception e) {
      verify(pbpFeign).doBulkSuspensionProductsActions(anyString(), anyString(), anyString(),
          eq(DEFAULT_REQUEST_ID), eq(USERNAME), eq(suspensionProductRequestList));
    }
  }
}
