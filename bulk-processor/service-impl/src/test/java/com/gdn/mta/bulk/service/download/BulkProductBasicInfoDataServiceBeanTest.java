package com.gdn.mta.bulk.service.download;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.anyInt;
import static org.mockito.Mockito.anyList;
import static org.mockito.Mockito.anyMap;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.ProductBasicInfoDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDownloadProductBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductSkuResponse;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.service.PCBOutboundService;
import com.gdn.mta.bulk.service.XProductOutboundService;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;

class BulkProductBasicInfoDataServiceBeanTest {

  @InjectMocks
  private BulkProductBasicInfoDataServiceBean bulkProductBasicInfoDataServiceBean;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @Mock
  private PCBOutboundService pcbOutboundService;

  @Mock
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @BeforeEach
  void setUp() {
    initMocks(this);
    ReflectionTestUtils.setField(bulkProductBasicInfoDataServiceBean, "maxProductSize", 100);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(bulkDownloadServiceBeanUtil);
    verifyNoMoreInteractions(pcbOutboundService);
    verifyNoMoreInteractions(businessPartnerRepository);
    verifyNoMoreInteractions(xProductOutboundService);
  }
  @Test
  void testGetData_withValidRequest_shouldReturnProductResponse() throws Exception {
    // Arrange
    ProductBasicInfoDownloadRequest request = new ProductBasicInfoDownloadRequest();
    request.setMerchantId("merchant-123");
    request.setRequestId("req-001");
    request.setUsername("test-user");
    ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
    request.setProductSummaryRequest(productSummaryRequest);

    // Business partner
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setOfflineToOnlineFlag(true);
    profileResponse.setCompany(company);
    profileResponse.setBusinessPartnerCode("BP001");
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(any(), any()))
        .thenReturn(profileResponse);

    // Product SKU Response
    ProductSkuResponse skuResponse = new ProductSkuResponse();
    skuResponse.setProductSku("SKU123");
    Pageable pageable = PageRequest.of(0, 100);
    PageImpl<ProductSkuResponse> skuPage = new PageImpl<>(List.of(skuResponse), pageable, 1);

    when(xProductOutboundService.getProductSkuResponse(any(), anyInt(), anyInt(), any(), any()))
        .thenReturn(skuPage);

    // Product Info Response
    BulkDownloadProductBasicInfoResponse productInfoResponse = new BulkDownloadProductBasicInfoResponse();
    productInfoResponse.setProductBasicInfoResponseList(new ArrayList<>());
    productInfoResponse.setExceptionMap(Map.of("SKU123", "Not Found"));

    when(xProductOutboundService.getProductBasicInfoDetails(any()))
        .thenReturn(productInfoResponse);

    when(pcbOutboundService.filterCategoryHierarchyByCategoryCodes(any()))
        .thenReturn(Collections.emptyList());

    when(bulkDownloadServiceBeanUtil.getAllProductBasicInfoValues(anyBoolean(), any(), anyMap(), anyMap()))
        .thenReturn(List.of(List.of("data1", "data2")));

    // Act
    BulkDataResponse response = bulkProductBasicInfoDataServiceBean.getData(request);

    // Assert
    assertNotNull(response);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(any(), any());
    verify(xProductOutboundService, atLeastOnce()).getProductSkuResponse(any(), anyInt(), anyInt(), any(), any());
    verify(xProductOutboundService, atLeastOnce()).getProductBasicInfoDetails(any());
    verify(bulkDownloadServiceBeanUtil).getAllProductBasicInfoValues(anyBoolean(), any(), anyMap(), anyMap());
    verify(pcbOutboundService).filterCategoryHierarchyByCategoryCodes(any());
  }

  @Test
  void testGetData_withValidRequest_shouldReturnProductResponseException() throws Exception {
    // Arrange
    ProductBasicInfoDownloadRequest request = new ProductBasicInfoDownloadRequest();
    request.setMerchantId("merchant-123");
    request.setRequestId("req-001");
    request.setUsername("test-user");
    ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
    request.setProductSummaryRequest(productSummaryRequest);

    // Business partner
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setOfflineToOnlineFlag(true);
    profileResponse.setCompany(company);
    profileResponse.setBusinessPartnerCode("BP001");
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(any(), any()))
        .thenReturn(profileResponse);

    // Product SKU Response
    ProductSkuResponse skuResponse = new ProductSkuResponse();
    skuResponse.setProductSku("SKU123");
    Pageable pageable = PageRequest.of(0, 100);
    PageImpl<ProductSkuResponse> skuPage = new PageImpl<>(List.of(skuResponse), pageable, 1);

    when(xProductOutboundService.getProductSkuResponse(any(), anyInt(), anyInt(), any(), any()))
        .thenThrow(new RuntimeException(new ApplicationException()));

    // Product Info Response
    BulkDownloadProductBasicInfoResponse productInfoResponse = new BulkDownloadProductBasicInfoResponse();
    productInfoResponse.setProductBasicInfoResponseList(new ArrayList<>());
    productInfoResponse.setExceptionMap(Map.of("SKU123", "Not Found"));

    when(xProductOutboundService.getProductBasicInfoDetails(any()))
        .thenReturn(productInfoResponse);

    when(pcbOutboundService.filterCategoryHierarchyByCategoryCodes(any()))
        .thenReturn(Collections.emptyList());

    when(bulkDownloadServiceBeanUtil.getAllProductBasicInfoValues(anyBoolean(), any(), anyMap(), anyMap()))
        .thenReturn(List.of(List.of("data1", "data2")));

    // Act
    BulkDataResponse response = bulkProductBasicInfoDataServiceBean.getData(request);

    // Assert
    assertNotNull(response);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(any(), any());
    verify(xProductOutboundService, atLeastOnce()).getProductSkuResponse(any(), anyInt(), anyInt(), any(), any());
    verify(bulkDownloadServiceBeanUtil).getAllProductBasicInfoValues(anyBoolean(), any(), anyMap(), anyMap());
    verify(pcbOutboundService).filterCategoryHierarchyByCategoryCodes(any());
  }

  @Test
  void testGetData_withValidRequest_shouldReturnProductNullResponse() throws Exception {
    // Arrange
    ProductBasicInfoDownloadRequest request = new ProductBasicInfoDownloadRequest();
    request.setMerchantId("merchant-123");
    request.setRequestId("req-001");
    request.setUsername("test-user");
    ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
    request.setProductSummaryRequest(productSummaryRequest);

    // Business partner
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setOfflineToOnlineFlag(true);
    profileResponse.setCompany(company);
    profileResponse.setBusinessPartnerCode("BP001");
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(any(), any()))
        .thenReturn(profileResponse);

    // Product SKU Response
    ProductSkuResponse skuResponse = new ProductSkuResponse();
    skuResponse.setProductSku("SKU123");
    Pageable pageable = PageRequest.of(0, 100);
    PageImpl<ProductSkuResponse> skuPage = new PageImpl<>(List.of(skuResponse), pageable, 1);

    when(xProductOutboundService.getProductSkuResponse(any(), anyInt(), anyInt(), any(), any()))
        .thenReturn(skuPage);

    // Product Info Response
    BulkDownloadProductBasicInfoResponse productInfoResponse = new BulkDownloadProductBasicInfoResponse();
    productInfoResponse.setProductBasicInfoResponseList(new ArrayList<>());
    productInfoResponse.setExceptionMap(Map.of("SKU123", "Not Found"));

    when(xProductOutboundService.getProductBasicInfoDetails(any()))
        .thenReturn(null);

    when(pcbOutboundService.filterCategoryHierarchyByCategoryCodes(any()))
        .thenReturn(Collections.emptyList());

    when(bulkDownloadServiceBeanUtil.getAllProductBasicInfoValues(anyBoolean(), any(), anyMap(), anyMap()))
        .thenReturn(List.of(List.of("data1", "data2")));

    // Act
    BulkDataResponse response = bulkProductBasicInfoDataServiceBean.getData(request);

    // Assert
    assertNotNull(response);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(any(), any());
    verify(xProductOutboundService, atLeastOnce()).getProductSkuResponse(any(), anyInt(), anyInt(), any(), any());
    verify(xProductOutboundService, atLeastOnce()).getProductBasicInfoDetails(any());
    verify(bulkDownloadServiceBeanUtil).getAllProductBasicInfoValues(anyBoolean(), any(), anyMap(), anyMap());
    verify(pcbOutboundService).filterCategoryHierarchyByCategoryCodes(any());
  }

  @Test
  void testGetData_withValidRequest_shouldReturnProductException() throws Exception {
    // Arrange
    ProductBasicInfoDownloadRequest request = new ProductBasicInfoDownloadRequest();
    request.setMerchantId("merchant-123");
    request.setRequestId("req-001");
    request.setUsername("test-user");
    ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
    request.setProductSummaryRequest(productSummaryRequest);

    // Business partner
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setOfflineToOnlineFlag(true);
    profileResponse.setCompany(company);
    profileResponse.setBusinessPartnerCode("BP001");
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(any(), any()))
        .thenReturn(profileResponse);

    // Product SKU Response
    ProductSkuResponse skuResponse = new ProductSkuResponse();
    skuResponse.setProductSku("SKU123");
    Pageable pageable = PageRequest.of(0, 100);
    PageImpl<ProductSkuResponse> skuPage = new PageImpl<>(List.of(skuResponse), pageable, 1);

    when(xProductOutboundService.getProductSkuResponse(any(), anyInt(), anyInt(), any(), any()))
        .thenReturn(skuPage);

    // Product Info Response
    BulkDownloadProductBasicInfoResponse productInfoResponse = new BulkDownloadProductBasicInfoResponse();
    productInfoResponse.setProductBasicInfoResponseList(new ArrayList<>());
    productInfoResponse.setExceptionMap(Map.of("SKU123", "Not Found"));

    when(xProductOutboundService.getProductBasicInfoDetails(any()))
        .thenThrow(ApplicationException.class);

    when(pcbOutboundService.filterCategoryHierarchyByCategoryCodes(any()))
        .thenReturn(Collections.emptyList());

    when(bulkDownloadServiceBeanUtil.getAllProductBasicInfoValues(anyBoolean(), any(), anyMap(), anyMap()))
        .thenReturn(List.of(List.of("data1", "data2")));

    // Act
    BulkDataResponse response = bulkProductBasicInfoDataServiceBean.getData(request);

    // Assert
    assertNotNull(response);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(any(), any());
    verify(xProductOutboundService, atLeastOnce()).getProductSkuResponse(any(), anyInt(), anyInt(), any(), any());
    verify(xProductOutboundService, atLeastOnce()).getProductBasicInfoDetails(any());
    verify(bulkDownloadServiceBeanUtil).getAllProductBasicInfoValues(anyBoolean(), any(), anyMap(), anyMap());
    verify(pcbOutboundService).filterCategoryHierarchyByCategoryCodes(any());
  }

  @Test
  void testGetData_withValidRequest_shouldReturnProductEmptyResponse() throws Exception {
    // Arrange
    ProductBasicInfoDownloadRequest request = new ProductBasicInfoDownloadRequest();
    request.setMerchantId("merchant-123");
    request.setRequestId("req-001");
    request.setUsername("test-user");
    ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
    request.setProductSummaryRequest(productSummaryRequest);

    // Business partner
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setOfflineToOnlineFlag(true);
    profileResponse.setCompany(company);
    profileResponse.setBusinessPartnerCode("BP001");
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(any(), any()))
        .thenReturn(profileResponse);

    // Product SKU Response
    ProductSkuResponse skuResponse = new ProductSkuResponse();
    skuResponse.setProductSku("SKU123");
    Pageable pageable = PageRequest.of(0, 100);
    PageImpl<ProductSkuResponse> skuPage = new PageImpl<>(List.of(skuResponse), pageable, 1);

    when(xProductOutboundService.getProductSkuResponse(any(), anyInt(), anyInt(), any(), any()))
        .thenReturn(skuPage);

    // Product Info Response
    BulkDownloadProductBasicInfoResponse productInfoResponse = new BulkDownloadProductBasicInfoResponse();
    productInfoResponse.setProductBasicInfoResponseList(new ArrayList<>());
    productInfoResponse.setExceptionMap(Map.of("SKU123", "Not Found"));

    when(xProductOutboundService.getProductBasicInfoDetails(any()))
        .thenReturn(productInfoResponse);

    when(pcbOutboundService.filterCategoryHierarchyByCategoryCodes(any()))
        .thenReturn(Collections.emptyList());

    when(bulkDownloadServiceBeanUtil.getAllProductBasicInfoValues(anyBoolean(), any(), anyMap(), anyMap()))
        .thenReturn(new ArrayList<>());

    // Act
    BulkDataResponse response = bulkProductBasicInfoDataServiceBean.getData(request);

    // Assert
    assertNotNull(response);
    verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(any(), any());
    verify(xProductOutboundService, atLeastOnce()).getProductSkuResponse(any(), anyInt(), anyInt(), any(), any());
    verify(xProductOutboundService, atLeastOnce()).getProductBasicInfoDetails(any());
    verify(bulkDownloadServiceBeanUtil).getAllProductBasicInfoValues(anyBoolean(), any(), anyMap(), anyMap());
    verify(pcbOutboundService).filterCategoryHierarchyByCategoryCodes(any());
  }

  @Test
  void testFetchListOfProductSkusForExcel_shouldReturnList() {
    // Arrange
    ProductSummaryRequest summaryRequest = new ProductSummaryRequest();
    ProductSkuResponse skuResponse = new ProductSkuResponse();
    skuResponse.setProductSku("SKU001");

    Pageable pageable = PageRequest.of(0, 100);
    when(xProductOutboundService.getProductSkuResponse(any(), anyInt(), anyInt(), any(), any()))
        .thenReturn(new PageImpl<>(List.of(skuResponse), pageable, 1));

    // Act
    List<ProductSkuResponse> result =
        bulkProductBasicInfoDataServiceBean.fetchListOfProductSkusForExcel(summaryRequest, "req-01", "tester");
    verify(xProductOutboundService).getProductSkuResponse(any(), anyInt(), anyInt(), any(), any());
    // Assert
    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals("SKU001", result.get(0).getProductSku());
  }

  @Test
  void testGetProductsFromProductService_shouldReturnResponse() throws ApplicationException {
    // Arrange
    ProductSummaryRequest summaryRequest = new ProductSummaryRequest();
    summaryRequest.setPickupPointCodes(List.of("P1"));
    summaryRequest.setPromoTypes(List.of("PROMO1"));

    ProductSkuResponse skuResponse = new ProductSkuResponse();
    skuResponse.setProductSku("SKU001");

    Pageable pageable = PageRequest.of(0, 100);
    when(xProductOutboundService.getProductSkuResponse(any(), anyInt(), anyInt(), any(), any()))
        .thenReturn(new PageImpl<>(List.of(skuResponse), pageable, 1));

    BulkDownloadProductBasicInfoResponse infoResponse = new BulkDownloadProductBasicInfoResponse();
    infoResponse.setProductBasicInfoResponseList(Collections.emptyList());
    infoResponse.setExceptionMap(Map.of("SKU001", "Product Not Found"));

    when(xProductOutboundService.getProductBasicInfoDetails(any())).thenReturn(infoResponse);

    // Act
    BulkDownloadProductBasicInfoResponse result = bulkProductBasicInfoDataServiceBean
        .getProductsFromProductService("BP001", summaryRequest, "user-01", "req-02");
    verify(xProductOutboundService).getProductBasicInfoDetails(any());
    verify(xProductOutboundService).getProductSkuResponse(any(), anyInt(), anyInt(), any(), any());
    // Assert
    assertNotNull(result);
    assertTrue(result.getExceptionMap().containsKey("SKU001"));
  }

  @Test
  void testFetchListOfProductSkusForExcel_withTwoTotalElements() {
    ReflectionTestUtils.setField(bulkProductBasicInfoDataServiceBean, "maxProductSize", 1);
    // Arrange
    ProductSummaryRequest summaryRequest = new ProductSummaryRequest();
    ProductSkuResponse skuResponse1 = new ProductSkuResponse();
    skuResponse1.setProductSku("SKU001");

    ProductSkuResponse skuResponse2 = new ProductSkuResponse();
    skuResponse2.setProductSku("SKU002");

    Pageable pageable = PageRequest.of(0, 1); // Page size is 1 to simulate pagination
    when(xProductOutboundService.getProductSkuResponse(any(), eq(0), eq(1), any(), any()))
        .thenReturn(new PageImpl<>(List.of(skuResponse1), pageable, 2)); // First page, total 2 elements
    when(xProductOutboundService.getProductSkuResponse(any(), eq(1), eq(1), any(), any()))
        .thenReturn(new PageImpl<>(List.of(skuResponse2), pageable, 2)); // Second page

    // Act
    List<ProductSkuResponse> result =
        bulkProductBasicInfoDataServiceBean.fetchListOfProductSkusForExcel(summaryRequest, "req-01", "tester");

    // Assert
    assertNotNull(result);
    assertEquals(2, result.size());
    assertEquals("SKU001", result.get(0).getProductSku());
    assertEquals("SKU002", result.get(1).getProductSku());
    verify(xProductOutboundService).getProductSkuResponse(any(), eq(0), eq(1), any(), any());
    verify(xProductOutboundService).getProductSkuResponse(any(), eq(1), eq(1), any(), any());
  }

  @Test
  void testFetchListOfProductSkusForExcel_emptyResult() {
    // Arrange
    ProductSummaryRequest summaryRequest = new ProductSummaryRequest();
    Pageable pageable = PageRequest.of(0, 100);

    when(xProductOutboundService.getProductSkuResponse(any(), anyInt(), anyInt(), any(), any()))
        .thenReturn(new PageImpl<>(List.of(), pageable, 0));

    // Act
    List<ProductSkuResponse> result =
        bulkProductBasicInfoDataServiceBean.fetchListOfProductSkusForExcel(summaryRequest, "req-01", "tester");

    verify(xProductOutboundService).getProductSkuResponse(any(), anyInt(), anyInt(), any(), any());
    // Assert
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }



}