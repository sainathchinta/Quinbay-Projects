package com.gdn.mta.bulk.service.util;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import com.gdn.mta.bulk.BulkDownloadException;
import com.gdn.mta.bulk.models.FailedRebateResponse;
import com.gdn.mta.bulk.models.SkuRebateUpdateRequest;
import com.gdn.mta.bulk.models.download.BulkPriceRecommendationDownloadRequest;
import com.gdn.mta.bulk.models.download.UpdateRemoveProductTaggingRequest;
import com.gdn.mta.bulk.models.download.responsedata.FailedReasonResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.TaggedProductFilterRequest;
import com.gdn.mta.bulk.dto.TaggedProductFilterResponse;
import com.gdn.mta.bulk.feignConfig.PriceAnalyticsFeign;
import com.gdn.mta.bulk.models.download.responsedata.BulkRebateResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkRebateUpdateRequest;
import com.gdn.mta.bulk.models.download.responsedata.DownloadSkuResponse;
import com.gdn.mta.bulk.service.PriceAnalyticsServiceImpl;

public class PriceAnalyticsOutboundServiceImplTest {

  public static final String REQUEST_ID_KEY_PARAMETER = "requestId";
  public static final String USERNAME_KEY_PARAMETER = "username";
  public static final String STORE_ID_KEY_PARAMETER = "storeId";
  public static final String CHANNEL_ID_KEY_PARAMETER = "channelId";
  public static final String CLIENT_ID_KEY_PARAMETER = "clientId";
  private static final String BRAND_NAME = "brand_name";
  private static final String BUSINESS_PARTNER_CODE = "TEB-24219";
  private static final String CATEGORY_CODE = "KA-1000039";
  private static final String PRODUCT_TYPE = "product_type";

  @InjectMocks
  private PriceAnalyticsServiceImpl priceAnalyticsService;

  @Mock
  private PriceAnalyticsFeign priceAnalyticsFeign;

  private BulkPriceRecommendationDownloadRequest bulkPriceRecommendationDownloadRequest;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    bulkPriceRecommendationDownloadRequest =
        BulkPriceRecommendationDownloadRequest.builder()
            .brandNames(Collections.singleton(BRAND_NAME))
            .businessPartnerCodes(Collections.singleton(BUSINESS_PARTNER_CODE))
            .categoryCodes(Collections.singleton(CATEGORY_CODE))
            .productTypes(Collections.singleton(PRODUCT_TYPE))
            .build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(priceAnalyticsFeign);
  }

  @Test
  public void getDownloadSkusTest(){
    GdnRestListResponse<DownloadSkuResponse> mockResponse = new GdnRestListResponse<>();
    mockResponse.setSuccess(true);
    mockResponse.setContent(List.of(new DownloadSkuResponse()));
    PageMetaData pageMetaData = new PageMetaData();
    pageMetaData.setTotalRecords(2);
    mockResponse.setPageMetaData(pageMetaData);
    ReflectionTestUtils.setField(priceAnalyticsService,"bulkPriceRecommendationBatchSize",1);
    ReflectionTestUtils.setField(priceAnalyticsService,"getBulkPriceRecommendationExcelRowsSize",2);
    Mockito.when(priceAnalyticsFeign.getSkusForDownload(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), "email", 0,
        1, bulkPriceRecommendationDownloadRequest)).thenReturn(mockResponse);
    Mockito.when(priceAnalyticsFeign.getSkusForDownload(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), "email", 1,
        1, bulkPriceRecommendationDownloadRequest)).thenReturn(mockResponse);
    priceAnalyticsService.getDownloadSkus("email", bulkPriceRecommendationDownloadRequest);
    Mockito.verify(priceAnalyticsFeign).getSkusForDownload(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), "email", 0,
        1, bulkPriceRecommendationDownloadRequest);
    Mockito.verify(priceAnalyticsFeign).getSkusForDownload(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), "email", 1,
        1, bulkPriceRecommendationDownloadRequest);
  }


  @Test
  public void getDownloadSkusRowSizeLimitTest(){
    GdnRestListResponse<DownloadSkuResponse> mockResponse = new GdnRestListResponse<>();
    mockResponse.setSuccess(true);
    mockResponse.setContent(List.of(new DownloadSkuResponse(),new DownloadSkuResponse()));
    ReflectionTestUtils.setField(priceAnalyticsService,"bulkPriceRecommendationBatchSize",1);
    ReflectionTestUtils.setField(priceAnalyticsService,"getBulkPriceRecommendationExcelRowsSize",1);
    Mockito.when(priceAnalyticsFeign.getSkusForDownload(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), "email", 0,
        1, bulkPriceRecommendationDownloadRequest)).thenReturn(mockResponse);
    List<DownloadSkuResponse> downloadSkuResponseList =
        priceAnalyticsService.getDownloadSkus("email", bulkPriceRecommendationDownloadRequest);
    Mockito.verify(priceAnalyticsFeign).getSkusForDownload(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), "email", 0,
        1, bulkPriceRecommendationDownloadRequest);
    Assertions.assertEquals(1,downloadSkuResponseList.size());

  }

  @Test()
  public void getDownloadSkusForResponseFalseTest(){
    try {
      GdnRestListResponse<DownloadSkuResponse> mockResponse = new GdnRestListResponse<>();
      mockResponse.setSuccess(false);
      mockResponse.setContent(null);
      ReflectionTestUtils.setField(priceAnalyticsService, "bulkPriceRecommendationBatchSize", 25);
      Mockito.when(priceAnalyticsFeign.getSkusForDownload(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), "email", 0,
          25, bulkPriceRecommendationDownloadRequest)).thenReturn(mockResponse);
      priceAnalyticsService.getDownloadSkus("email", bulkPriceRecommendationDownloadRequest);
    }catch(ApplicationRuntimeException ex){
      Assertions.assertEquals("Can not communicate :",ex.getErrorCodes().getMessage());
    }
    finally {
      Mockito.verify(priceAnalyticsFeign).getSkusForDownload(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), "email", 0,
          25, bulkPriceRecommendationDownloadRequest);
    }
  }

  @Test()
  public void getDownloadSkusForResponseFalseContentNotEmptyTest(){
    try {
      GdnRestListResponse<DownloadSkuResponse> mockResponse = new GdnRestListResponse<>();
      mockResponse.setSuccess(false);
      mockResponse.setContent(List.of(new DownloadSkuResponse()));
      ReflectionTestUtils.setField(priceAnalyticsService, "bulkPriceRecommendationBatchSize", 25);
      Mockito.when(priceAnalyticsFeign.getSkusForDownload(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), "email", 0,
          25, bulkPriceRecommendationDownloadRequest)).thenReturn(mockResponse);
      priceAnalyticsService.getDownloadSkus("email", bulkPriceRecommendationDownloadRequest);
    }catch(ApplicationRuntimeException ex){
      Assertions.assertEquals("Can not communicate :",ex.getErrorCodes().getMessage());
    }
    finally {
      Mockito.verify(priceAnalyticsFeign).getSkusForDownload(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), "email", 0,
          25, bulkPriceRecommendationDownloadRequest);
    }
  }

  @Test()
  public void getDownloadSkusContentNotEmptyTest(){
    try {
      GdnRestListResponse<DownloadSkuResponse> mockResponse = new GdnRestListResponse<>();
      mockResponse.setSuccess(true);
      mockResponse.setContent(null);
      ReflectionTestUtils.setField(priceAnalyticsService, "bulkPriceRecommendationBatchSize", 25);
      Mockito.when(priceAnalyticsFeign.getSkusForDownload(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), "email", 0,
          25, bulkPriceRecommendationDownloadRequest)).thenReturn(mockResponse);
      priceAnalyticsService.getDownloadSkus("email", bulkPriceRecommendationDownloadRequest);
    }catch(ApplicationRuntimeException ex){
      Assertions.assertEquals("Can not communicate :",ex.getErrorCodes().getMessage());
    }
    finally {
      Mockito.verify(priceAnalyticsFeign).getSkusForDownload(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), "email", 0,
          25, bulkPriceRecommendationDownloadRequest);
    }
  }


  @Test
  public void GetDownloadTaggedProductsTest() throws BulkDownloadException {
    GdnRestListResponse<TaggedProductFilterResponse> mockResponse = new GdnRestListResponse<>();
    mockResponse.setSuccess(true);
    mockResponse.setContent(List.of(new TaggedProductFilterResponse()));
    PageMetaData pageMetaData = new PageMetaData();
    pageMetaData.setTotalRecords(2);
    mockResponse.setPageMetaData(pageMetaData);
    ReflectionTestUtils.setField(priceAnalyticsService,"bulkProductTypeTaggingBatchSize",1);
    ReflectionTestUtils.setField(priceAnalyticsService,"getBulkTaggedProductsExcelRowsSize",2);
    Mockito.when(priceAnalyticsFeign.getTaggedProducts(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(),0,
        1,new TaggedProductFilterRequest())).thenReturn(mockResponse);
    Mockito.when(priceAnalyticsFeign.getTaggedProducts(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(),1,
        1,new TaggedProductFilterRequest())).thenReturn(mockResponse);
    priceAnalyticsService.getDownloadTaggedProducts(new TaggedProductFilterRequest());
    Mockito.verify(priceAnalyticsFeign).getTaggedProducts(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), 0,
        1,new TaggedProductFilterRequest());
    Mockito.verify(priceAnalyticsFeign).getTaggedProducts(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), 1,
        1,new TaggedProductFilterRequest());
  }

  @Test
  public void getDownloadTaggedProductsRowLimitTest() {
    GdnRestListResponse<TaggedProductFilterResponse> mockResponse = new GdnRestListResponse<>();
    mockResponse.setSuccess(true);
    mockResponse.setContent(List.of(new TaggedProductFilterResponse(), new TaggedProductFilterResponse()));
    ReflectionTestUtils.setField(priceAnalyticsService, "bulkProductTypeTaggingBatchSize", 1);
    ReflectionTestUtils.setField(priceAnalyticsService, "getBulkTaggedProductsExcelRowsSize", 1);
    Mockito.when(priceAnalyticsFeign.getTaggedProducts(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), 0, 1,
        new TaggedProductFilterRequest())).thenReturn(mockResponse);
    List<TaggedProductFilterResponse> taggedProductFilterResponsesList =
        priceAnalyticsService.getDownloadTaggedProducts(new TaggedProductFilterRequest());
    Mockito.verify(priceAnalyticsFeign).getTaggedProducts(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), 0, 1,
        new TaggedProductFilterRequest());
    Assertions.assertEquals(1, taggedProductFilterResponsesList.size());
  }

  @Test()
  public void getDownloadTaggedProductSuccessFalseTest(){
    try {
      GdnRestListResponse<TaggedProductFilterResponse> mockResponse = new GdnRestListResponse<>();
      mockResponse.setSuccess(false);
      mockResponse.setContent(null);
      ReflectionTestUtils.setField(priceAnalyticsService,"bulkProductTypeTaggingBatchSize",25);
      Mockito.when(priceAnalyticsFeign.getTaggedProducts(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(),0,
          25,new TaggedProductFilterRequest())).thenReturn(mockResponse);
      priceAnalyticsService.getDownloadTaggedProducts(new TaggedProductFilterRequest());
    }catch(ApplicationRuntimeException ex){
      Assertions.assertEquals("Can not communicate :",ex.getErrorCodes().getMessage());
    }
    finally {
      Mockito.verify(priceAnalyticsFeign).getTaggedProducts(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), 0,
          25,new TaggedProductFilterRequest());
    }
  }

  @Test()
  public void getDownloadTaggedProductResponseFalseContentNotEmptyTest(){
    try {
      GdnRestListResponse<TaggedProductFilterResponse> mockResponse = new GdnRestListResponse<>();
      mockResponse.setSuccess(false);
      mockResponse.setContent(List.of(new TaggedProductFilterResponse()));
      ReflectionTestUtils.setField(priceAnalyticsService,"bulkProductTypeTaggingBatchSize",25);
      Mockito.when(priceAnalyticsFeign.getTaggedProducts(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(),0,
          25,new TaggedProductFilterRequest())).thenReturn(mockResponse);
      priceAnalyticsService.getDownloadTaggedProducts(new TaggedProductFilterRequest());
    }catch(ApplicationRuntimeException ex){
      Assertions.assertEquals("Can not communicate :",ex.getErrorCodes().getMessage());
    }
    finally {
      Mockito.verify(priceAnalyticsFeign).getTaggedProducts(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), 0,
          25,new TaggedProductFilterRequest());
    }
  }

  @Test()
  public void getDownloadTaggedProductContentNullSuccessTrueTest(){
    try {
      GdnRestListResponse<TaggedProductFilterResponse> mockResponse = new GdnRestListResponse<>();
      mockResponse.setSuccess(true);
      mockResponse.setContent(null);
      ReflectionTestUtils.setField(priceAnalyticsService,"bulkProductTypeTaggingBatchSize",25);
      Mockito.when(priceAnalyticsFeign.getTaggedProducts(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(),0,
          25,new TaggedProductFilterRequest())).thenReturn(mockResponse);
      priceAnalyticsService.getDownloadTaggedProducts(new TaggedProductFilterRequest());
    }catch(ApplicationRuntimeException ex){
      Assertions.assertEquals("Can not find data :",ex.getErrorCodes().getMessage());
      Assertions.assertEquals("Can not find data :No tagged Products are present for given request",ex.getMessage());
    }
    finally {
      Mockito.verify(priceAnalyticsFeign).getTaggedProducts(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), 0,
          25,new TaggedProductFilterRequest());
    }
  }

  @Test
  public void updatePriceRebateTest() {
    BulkRebateUpdateRequest bulkRebateUpdateRequest = new BulkRebateUpdateRequest();
    Mockito.when(priceAnalyticsFeign.updateRebate(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), "emailAddress",
        bulkRebateUpdateRequest)).thenReturn(
        new GdnRestSingleResponse<>(new BulkRebateResponse(), GdnMandatoryRequestParameterUtil.getRequestId()));
    String responseString = priceAnalyticsService.updatePriceRebate(bulkRebateUpdateRequest, "emailAddress");
    Mockito.verify(priceAnalyticsFeign)
        .updateRebate(GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getUsername(),
            GdnMandatoryRequestParameterUtil.getRequestId(), "emailAddress", bulkRebateUpdateRequest);
    Assertions.assertNull(responseString);
  }

  @Test
  public void updatePriceRebateErrorTest() {
    BulkRebateUpdateRequest bulkRebateUpdateRequest = new BulkRebateUpdateRequest();
    Mockito.when(priceAnalyticsFeign.updateRebate(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), "emailAddress",
        bulkRebateUpdateRequest)).thenReturn(new GdnRestSingleResponse<>(null, null, false, new BulkRebateResponse(),
        GdnMandatoryRequestParameterUtil.getRequestId()));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> priceAnalyticsService.updatePriceRebate(bulkRebateUpdateRequest, "emailAddress"));
    } finally {
      Mockito.verify(priceAnalyticsFeign)
          .updateRebate(GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getClientId(),
              GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getUsername(),
              GdnMandatoryRequestParameterUtil.getRequestId(), "emailAddress", bulkRebateUpdateRequest);
    }
  }

  @Test
  public void updateTagging_SuccessfulResponse_Test() {
    GdnRestListResponse<FailedReasonResponse> mockResponse = new GdnRestListResponse<>();
    mockResponse.setSuccess(true);
    mockResponse.setContent(Collections.singletonList(new FailedReasonResponse()));
    String officerEmailAddress = "test@example.com";
    List<UpdateRemoveProductTaggingRequest> taggingRequests = Collections.emptyList();
    Mockito.when(priceAnalyticsFeign.updateTagging(
        GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(),
        officerEmailAddress, taggingRequests))
      .thenReturn(mockResponse);
    List<FailedReasonResponse> result = priceAnalyticsService.updateTagging(officerEmailAddress, taggingRequests);
    Mockito.verify(priceAnalyticsFeign).updateTagging(GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
      GdnMandatoryRequestParameterUtil.getUsername(),
      GdnMandatoryRequestParameterUtil.getRequestId(), officerEmailAddress, taggingRequests);
    Assertions.assertEquals(1, result.size());
  }

  @Test
  public void updateTagging_FailureResponse_Test() {
    GdnRestListResponse<FailedReasonResponse> mockResponse = new GdnRestListResponse<>();
    mockResponse.setSuccess(false);
    mockResponse.setContent(Collections.emptyList());
    String officerEmailAddress = "test@example.com";
    List<UpdateRemoveProductTaggingRequest> taggingRequests = Collections.emptyList();
    Mockito.when(priceAnalyticsFeign.updateTagging(
        GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(),
        officerEmailAddress, taggingRequests))
      .thenReturn(mockResponse);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> priceAnalyticsService.updateTagging(officerEmailAddress, taggingRequests));
    }
    finally {
      Mockito.verify(priceAnalyticsFeign).updateTagging(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), officerEmailAddress, taggingRequests);
    }
  }

  @Test
  public void updateTagging_FailureResponse_TestWithNullResponse() {
    GdnRestListResponse<FailedReasonResponse> mockResponse = new GdnRestListResponse<>();
    mockResponse.setSuccess(true);
    mockResponse.setContent(null);
    String officerEmailAddress = "test@example.com";
    List<UpdateRemoveProductTaggingRequest> taggingRequests = Collections.emptyList();
    Mockito.when(priceAnalyticsFeign.updateTagging(
        GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(),
        officerEmailAddress, taggingRequests))
      .thenReturn(mockResponse);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> priceAnalyticsService.updateTagging(officerEmailAddress, taggingRequests));
    }
    finally {
      Mockito.verify(priceAnalyticsFeign).updateTagging(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), officerEmailAddress, taggingRequests);
    }
  }

  @Test
  public void removeTagging_SuccessfulResponse_Test() {
    GdnRestListResponse<FailedReasonResponse> mockResponse = new GdnRestListResponse<>();
    mockResponse.setSuccess(true);
    mockResponse.setContent(Collections.singletonList(new FailedReasonResponse()));
    String officerEmailAddress = "test@example.com";
    List<UpdateRemoveProductTaggingRequest> taggingRequests = Collections.emptyList();
    Mockito.when(priceAnalyticsFeign.removeTagging(
        GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(),
        officerEmailAddress, taggingRequests))
      .thenReturn(mockResponse);
    List<FailedReasonResponse> result = priceAnalyticsService.removeTagging(officerEmailAddress,
      taggingRequests);
    Mockito.verify(priceAnalyticsFeign).removeTagging(GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
      GdnMandatoryRequestParameterUtil.getUsername(),
      GdnMandatoryRequestParameterUtil.getRequestId(), officerEmailAddress, taggingRequests);
    Assertions.assertEquals(1, result.size());
  }

  @Test
  public void removeTagging_FailureResponse_Test() {
    GdnRestListResponse<FailedReasonResponse> mockResponse = new GdnRestListResponse<>();
    mockResponse.setSuccess(false);
    mockResponse.setContent(Collections.emptyList());
    String officerEmailAddress = "test@example.com";
    List<UpdateRemoveProductTaggingRequest> taggingRequests = Collections.emptyList();
    Mockito.when(priceAnalyticsFeign.removeTagging(
        GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(),
        officerEmailAddress, taggingRequests))
      .thenReturn(mockResponse);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> priceAnalyticsService.removeTagging(officerEmailAddress, taggingRequests));
    }
    finally {
      Mockito.verify(priceAnalyticsFeign).removeTagging(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), officerEmailAddress, taggingRequests);
    }
  }

  @Test
  public void removeTagging_FailureResponse_TestWithNullResponse() {
    GdnRestListResponse<FailedReasonResponse> mockResponse = new GdnRestListResponse<>();
    mockResponse.setSuccess(true);
    mockResponse.setContent(null);
    String officerEmailAddress = "test@example.com";
    List<UpdateRemoveProductTaggingRequest> taggingRequests = Collections.emptyList();
    Mockito.when(priceAnalyticsFeign.removeTagging(
        GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(),
        officerEmailAddress, taggingRequests))
      .thenReturn(mockResponse);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> priceAnalyticsService.removeTagging(officerEmailAddress, taggingRequests));
    }
    finally {
      Mockito.verify(priceAnalyticsFeign).removeTagging(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), officerEmailAddress, taggingRequests);
    }
  }

  @Test
  public void updateSkuRebate_successTest() {
    SkuRebateUpdateRequest request = new SkuRebateUpdateRequest();
    Mockito.when(priceAnalyticsFeign.updateSkuRebate(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), request))
      .thenReturn(new GdnRestSingleResponse<>(null, null, true, new FailedRebateResponse(),
        GdnMandatoryRequestParameterUtil.getRequestId()));
    String result = priceAnalyticsService.updateSkuRebate(request);
    Mockito.verify(priceAnalyticsFeign).updateSkuRebate(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), request);
  }

  @Test
  public void updateSkuRebate_failedResponseTest() {
    SkuRebateUpdateRequest request = new SkuRebateUpdateRequest();
    Mockito.when(priceAnalyticsFeign.updateSkuRebate(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), request))
      .thenReturn(new GdnRestSingleResponse<>(null, null, false, new FailedRebateResponse(),
        GdnMandatoryRequestParameterUtil.getRequestId()));
    try {
      Assertions.assertThrows(RuntimeException.class, () -> priceAnalyticsService.updateSkuRebate(request));
    } finally {
      Mockito.verify(priceAnalyticsFeign).updateSkuRebate(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), request);
    }
  }

  @Test
  public void updateSkuRebate_nullResponseTest() {
    SkuRebateUpdateRequest request = new SkuRebateUpdateRequest();
    Mockito.when(priceAnalyticsFeign.updateSkuRebate(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), request))
      .thenReturn(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> priceAnalyticsService.updateSkuRebate(request));
    } finally {
      Mockito.verify(priceAnalyticsFeign).updateSkuRebate(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), request);
    }
  }

  @Test
  public void getOfficerTaggedSkus_successTest() {
    Mockito.when(priceAnalyticsFeign.getOfficerTaggedSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), Set.of()))
      .thenReturn(new GdnRestListResponse<>(List.of(), new PageMetaData(), GdnMandatoryRequestParameterUtil.getRequestId()));
    priceAnalyticsService.getOfficerTaggedSkus(Set.of());
    Mockito.verify(priceAnalyticsFeign).getOfficerTaggedSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), Set.of());
  }

  @Test
  public void getOfficerTaggedSkus_failedResponseTest() {
    Mockito.when(priceAnalyticsFeign.getOfficerTaggedSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), Set.of()))
      .thenReturn(new GdnRestListResponse<>(null, null, false, GdnMandatoryRequestParameterUtil.getRequestId()));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> priceAnalyticsService.getOfficerTaggedSkus(Set.of()));
    } finally {
      Mockito.verify(priceAnalyticsFeign).getOfficerTaggedSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), Set.of());
    }
  }

  @Test
  public void getOfficerTaggedSkus_nullResponseTest() {
    Mockito.when(priceAnalyticsFeign.getOfficerTaggedSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), Set.of()))
      .thenReturn(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> priceAnalyticsService.getOfficerTaggedSkus(Set.of()));
    } finally {
      Mockito.verify(priceAnalyticsFeign).getOfficerTaggedSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(), Set.of());
    }
  }
}
