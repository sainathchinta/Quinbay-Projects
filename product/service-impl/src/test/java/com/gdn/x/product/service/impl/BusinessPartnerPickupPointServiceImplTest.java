package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.dao.api.BusinessPartnerPickupPointRepository;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.rest.web.model.request.PickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.service.api.ProductCacheableService;

public class BusinessPartnerPickupPointServiceImplTest {

  private static final String STORE_ID = "10001";
  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String CODE = "code";
  private static final String CODE_2 = "code2";

  @InjectMocks
  private BusinessPartnerPickupPointServiceImpl businessPartnerPickupPointService;

  @Mock
  private BusinessPartnerPickupPointRepository businessPartnerPickupPointRepository;

  @Mock
  private ProductCacheableService productCacheableService;

  private Product product;
  private List<BusinessPartnerPickupPoint> businessPartnerPickupPointList;
  private PickupPointSummaryRequest pickupPointSummaryRequest;

  @Value("${should.restrict.access}")
  private boolean shouldRestrictAccess;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    product = new Product();
    Set<String> codes = new HashSet<>();
    codes.add(CODE_2);
    product.setPickupPointCodes(codes);

    BusinessPartnerPickupPoint businessPartnerPickupPoint1 = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint1.setCode(CODE);
    BusinessPartnerPickupPoint businessPartnerPickupPoint2 = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint2.setCode(CODE_2);
    businessPartnerPickupPoint2.setFbbActivated(true);

    businessPartnerPickupPointList = new ArrayList<>();
    businessPartnerPickupPointList.add(businessPartnerPickupPoint1);
    businessPartnerPickupPointList.add(businessPartnerPickupPoint2);

    pickupPointSummaryRequest = new PickupPointSummaryRequest(BUSINESS_PARTNER_CODE, PRODUCT_SKU, CODE, true,false, new HashSet<>());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(businessPartnerPickupPointRepository);
  }

  @Test
  public void saveBusinessPartnerPickupPoint() {
    businessPartnerPickupPointService.saveBusinessPartnerPickupPoint(new BusinessPartnerPickupPoint());
    Mockito.verify(businessPartnerPickupPointRepository).save(new BusinessPartnerPickupPoint());
  }

  @Test
  public void getBusinessPartnerPickupPointTest() {
    businessPartnerPickupPointService.getBusinessPartnerPickupPoint(STORE_ID, BUSINESS_PARTNER_CODE, CODE);
    Mockito.verify(businessPartnerPickupPointRepository).findByBusinessPartnerCodeAndCode(BUSINESS_PARTNER_CODE, CODE);
  }

  @Test
  public void getBusinessPartnerPickupPointByPickupPointCodesExceptionTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, null));
  }

  @Test
  public void getBusinessPartnerPickupPointByPickupPointCodesNullResponseTest() {
    businessPartnerPickupPointService
        .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, Collections.singletonList(CODE));
    Mockito.verify(businessPartnerPickupPointRepository).findByCodeIn(Collections.singletonList(CODE));
  }

  @Test
  public void getBusinessPartnerPickupPointByPickupPointCodesTest() {
    Mockito.when(businessPartnerPickupPointRepository.findByCodeIn(Collections.singletonList(CODE)))
        .thenReturn(Collections.singletonList(new BusinessPartnerPickupPoint()));
    businessPartnerPickupPointService
        .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, Collections.singletonList(CODE));
    Mockito.verify(businessPartnerPickupPointRepository).findByCodeIn(Collections.singletonList(CODE));
  }

  @Test
  public void getBusinessPartnerPickupPointByBusinessPartnerCodeTest() {
    Mockito.when(businessPartnerPickupPointRepository.findByBusinessPartnerCodeAndMarkForDeleteFalseAndArchivedFalse(BUSINESS_PARTNER_CODE)).thenReturn(
        Arrays.asList(new BusinessPartnerPickupPoint()));
    businessPartnerPickupPointService.getBusinessPartnerPickupPointByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerPickupPointRepository).findByBusinessPartnerCodeAndMarkForDeleteFalseAndArchivedFalse(BUSINESS_PARTNER_CODE);
  }

  @Test
  public void getBusinessPartnerPickupPointByBusinessPartnerCodeInvalidBusinessPartnerCodeTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> businessPartnerPickupPointService.getBusinessPartnerPickupPointByBusinessPartnerCode(StringUtils.EMPTY));
  }

  @Test
  public void getBusinessPartnerPickupPointSummaryTest() {
    Set<String> validPickupPointCodes = new HashSet<>();
    validPickupPointCodes.add("code2");
    pickupPointSummaryRequest.setCodes(validPickupPointCodes);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    Mockito.when(
            businessPartnerPickupPointRepository.findBusinessPartnerPickupPointData(STORE_ID, BUSINESS_PARTNER_CODE, CODE, true,false, validPickupPointCodes))
        .thenReturn(businessPartnerPickupPointList);

    Page<BusinessPartnerPickupPointResponse> businessPartnerPickupPointResponseList =
        businessPartnerPickupPointService.getBusinessPartnerPickupPointSummary(STORE_ID, 0, 1,
            pickupPointSummaryRequest);

    Mockito.verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(businessPartnerPickupPointRepository)
        .findBusinessPartnerPickupPointData(STORE_ID, BUSINESS_PARTNER_CODE, CODE, true,false, validPickupPointCodes);

    Assertions.assertEquals(1, businessPartnerPickupPointResponseList.getContent().size());
    Assertions.assertEquals(CODE_2, businessPartnerPickupPointResponseList.getContent().get(0).getCode());
  }

  @Test
  public void getBusinessPartnerPickupPointSummaryEmptyPPRequestTest() {
    Set<String> validPickupPointCodes = new HashSet<>();
    validPickupPointCodes.add("code2");
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    Mockito.when(
            businessPartnerPickupPointRepository.findBusinessPartnerPickupPointData(STORE_ID, BUSINESS_PARTNER_CODE, CODE, true,false, new HashSet<>()))
        .thenReturn(businessPartnerPickupPointList);

    Page<BusinessPartnerPickupPointResponse> businessPartnerPickupPointResponseList =
        businessPartnerPickupPointService.getBusinessPartnerPickupPointSummary(STORE_ID, 0, 1,
            pickupPointSummaryRequest);

    Mockito.verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(businessPartnerPickupPointRepository)
        .findBusinessPartnerPickupPointData(STORE_ID, BUSINESS_PARTNER_CODE, CODE, true,false, new HashSet<>());

    Assertions.assertEquals(1, businessPartnerPickupPointResponseList.getContent().size());
    Assertions.assertEquals(CODE_2, businessPartnerPickupPointResponseList.getContent().get(0).getCode());
  }

  @Test
  public void getBusinessPartnerPickupPointSummaryPPcodeRequestNotPresentTest() {
    Set<String> validPickupPointCodes = new HashSet<>();
    validPickupPointCodes.add("inValidPPCode");
    pickupPointSummaryRequest.setCodes(validPickupPointCodes);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    Mockito.when(
            businessPartnerPickupPointRepository.findBusinessPartnerPickupPointData(STORE_ID, BUSINESS_PARTNER_CODE, CODE, true,false, validPickupPointCodes))
        .thenReturn(businessPartnerPickupPointList);

    Page<BusinessPartnerPickupPointResponse> businessPartnerPickupPointResponseList =
        businessPartnerPickupPointService.getBusinessPartnerPickupPointSummary(STORE_ID, 0, 1,
            pickupPointSummaryRequest);

    Mockito.verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(businessPartnerPickupPointRepository)
        .findBusinessPartnerPickupPointData(STORE_ID, BUSINESS_PARTNER_CODE, CODE, true,false, validPickupPointCodes);

    Assertions.assertEquals(1, businessPartnerPickupPointResponseList.getContent().size());
    Assertions.assertEquals(CODE, businessPartnerPickupPointResponseList.getContent().get(0).getCode());
  }

  @Test
  public void getBusinessPartnerPickupPointFbbPickPointsTest() {
    Set<String> validPickupPointCodes = new HashSet<>();
    validPickupPointCodes.add("code2");
    pickupPointSummaryRequest.setCodes(validPickupPointCodes);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    Mockito.when(
        businessPartnerPickupPointRepository.findBusinessPartnerPickupPointData(STORE_ID, BUSINESS_PARTNER_CODE, CODE,
            true,true, validPickupPointCodes)).thenReturn(businessPartnerPickupPointList);
    pickupPointSummaryRequest.setFbbActivated(true);
    Page<BusinessPartnerPickupPointResponse> businessPartnerPickupPointResponseList =
        businessPartnerPickupPointService.getBusinessPartnerPickupPointSummary(STORE_ID, 0, 2,
            pickupPointSummaryRequest);
    Mockito.verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(businessPartnerPickupPointRepository)
        .findBusinessPartnerPickupPointData(STORE_ID, BUSINESS_PARTNER_CODE, CODE, true,true, validPickupPointCodes);
    Mockito.verify(businessPartnerPickupPointRepository)
        .findBusinessPartnerPickupPointData(STORE_ID, BUSINESS_PARTNER_CODE, CODE, true,true, validPickupPointCodes);
    Assertions.assertEquals(CODE_2, businessPartnerPickupPointResponseList.getContent().get(0).getCode());
  }

  @Test
  public void getBusinessPartnerPickupPointSummaryEmptyBusinessPartnerTest() {
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(businessPartnerPickupPointRepository.findBusinessPartnerPickupPointData(STORE_ID,
        BUSINESS_PARTNER_CODE, CODE, true, false, new HashSet<>())).thenReturn(new ArrayList<>());
    Page<BusinessPartnerPickupPointResponse> page =
        businessPartnerPickupPointService.getBusinessPartnerPickupPointSummary(STORE_ID, 0, 1,
            pickupPointSummaryRequest);
    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(businessPartnerPickupPointRepository)
        .findBusinessPartnerPickupPointData(STORE_ID, BUSINESS_PARTNER_CODE, CODE, true, false, new HashSet<>());
    Assertions.assertEquals(0, page.getContent().size());
  }

  @Test
  public void getBusinessPartnerPickupPointSummaryNullProductTest() {
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(null);
    Mockito.when(
            businessPartnerPickupPointRepository.findBusinessPartnerPickupPointData(STORE_ID, BUSINESS_PARTNER_CODE, CODE, true,false, new HashSet<>()))
        .thenReturn(businessPartnerPickupPointList);

    Page<BusinessPartnerPickupPointResponse> businessPartnerPickupPointResponseList =
        businessPartnerPickupPointService.getBusinessPartnerPickupPointSummary(STORE_ID, 0, 1,
            pickupPointSummaryRequest);

    Mockito.verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(businessPartnerPickupPointRepository)
        .findBusinessPartnerPickupPointData(STORE_ID, BUSINESS_PARTNER_CODE, CODE, true,false, new HashSet<>());
    Assertions.assertEquals(1, businessPartnerPickupPointResponseList.getContent().size());
    Assertions.assertEquals(CODE, businessPartnerPickupPointResponseList.getContent().get(0).getCode());
  }

  @Test
  public void getBusinessPartnerPickupPointSummaryEmptyProductSkuTest() {
    pickupPointSummaryRequest.setProductSku(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> businessPartnerPickupPointService.getBusinessPartnerPickupPointSummary(STORE_ID, 0, 1, pickupPointSummaryRequest));
  }

  @Test
  public void getBusinessPartnerPickupPointSummaryEmptyBpCodeTest() {
    pickupPointSummaryRequest.setBusinessPartnerCode(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> businessPartnerPickupPointService.getBusinessPartnerPickupPointSummary(STORE_ID, 0, 1, pickupPointSummaryRequest));
  }

  @Test
  public void getBusinessPartnerPickupPointSummaryEmptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> businessPartnerPickupPointService.getBusinessPartnerPickupPointSummary(null, 0, 1, pickupPointSummaryRequest));
  }

  @Test
  public void getCncActivatedPickupPointCodesExceptionTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> businessPartnerPickupPointService.getCncActivatedPickupPointCodes(new HashSet<>()));
  }

  @Test
  public void getCncActivatedPickupPointCodesEmptyTest() {
    List<String> cncActivatedPickupPointCodes =
        businessPartnerPickupPointService.getCncActivatedPickupPointCodes(Collections.singleton(CODE));
    Mockito.verify(businessPartnerPickupPointRepository).findByCode(CODE);
    Assertions.assertTrue(cncActivatedPickupPointCodes.isEmpty());
  }

  @Test
  public void getCncActivatedPickupPointCodesTest() {
    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(CODE);
    businessPartnerPickupPoint.setCncActivated(true);
    Mockito.when(businessPartnerPickupPointRepository.findByCode(CODE)).thenReturn(businessPartnerPickupPoint);
    List<String> cncActivatedPickupPointCodes =
        businessPartnerPickupPointService.getCncActivatedPickupPointCodes(Collections.singleton(CODE));
    Mockito.verify(businessPartnerPickupPointRepository).findByCode(CODE);
    Assertions.assertFalse(cncActivatedPickupPointCodes.isEmpty());
    Assertions.assertEquals(CODE, cncActivatedPickupPointCodes.get(0));
  }

  @Test
  public void getFbbActivatedPickupPointCodesTest() {
    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(CODE);
    businessPartnerPickupPoint.setFbbActivated(true);
    Mockito.when(businessPartnerPickupPointRepository.findByCode(CODE)).thenReturn(businessPartnerPickupPoint);
    List<String> cncActivatedPickupPointCodes =
        businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(Collections.singleton(CODE));
    Mockito.verify(businessPartnerPickupPointRepository).findByCode(CODE);
    Assertions.assertEquals(CODE, cncActivatedPickupPointCodes.get(0));
  }

  @Test
  public void getFbbActivatedPickupPointCodesBusinessPartnerPickupPointNullTest() {
    Mockito.when(businessPartnerPickupPointRepository.findByCode(CODE)).thenReturn(null);
    List<String> cncActivatedPickupPointCodes =
        businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(Collections.singleton(CODE));
    Mockito.verify(businessPartnerPickupPointRepository).findByCode(CODE);
    Assertions.assertTrue(cncActivatedPickupPointCodes.isEmpty());
  }

  @Test
  public void getFbbActivatedPickupPointCodesFbbFalseTest() {
    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(CODE);
    businessPartnerPickupPoint.setFbbActivated(false);
    Mockito.when(businessPartnerPickupPointRepository.findByCode(CODE)).thenReturn(businessPartnerPickupPoint);
    List<String> cncActivatedPickupPointCodes =
        businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(Collections.singleton(CODE));
    Mockito.verify(businessPartnerPickupPointRepository).findByCode(CODE);
    Assertions.assertTrue(cncActivatedPickupPointCodes.isEmpty());
  }

  @Test
  public void getFbbActivatedPickupPointCodesExceptionTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(new HashSet<>()));
  }

}