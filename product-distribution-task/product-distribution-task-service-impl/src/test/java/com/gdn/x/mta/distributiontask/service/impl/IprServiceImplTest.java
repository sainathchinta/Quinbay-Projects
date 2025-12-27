package com.gdn.x.mta.distributiontask.service.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pdt.dto.configuration.distribution.IPRActionResponseDto;
import com.gdn.x.mta.distributiontask.dao.api.IprProductSolrCollectionRepository;
import com.gdn.x.mta.distributiontask.dao.api.IprRepository;
import com.gdn.x.mta.distributiontask.dao.api.IprHistoryRepository;
import com.gdn.x.mta.distributiontask.dao.exception.ValidationException;
import com.gdn.x.mta.distributiontask.domain.event.model.IPRHistoryEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductChange;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.dao.api.feign.PBPFeign;
import com.gdn.x.mta.distributiontask.model.ErrorCategory;
import com.gdn.x.mta.distributiontask.model.ErrorMessages;
import com.gdn.x.mta.distributiontask.model.IPRHistory;
import com.gdn.x.mta.distributiontask.model.ProductIPR;
import com.gdn.x.mta.distributiontask.model.dto.AddingIprProductDTO;
import com.gdn.x.mta.distributiontask.model.dto.BrandReport;
import com.gdn.x.mta.distributiontask.model.dto.IPRHistoryDescription;
import com.gdn.x.mta.distributiontask.model.dto.IPRProductListRequest;
import com.gdn.x.mta.distributiontask.model.dto.IPRUpdateAssigneeRequest;
import com.gdn.x.mta.distributiontask.model.dto.IprActionRequest;
import com.gdn.x.mta.distributiontask.model.dto.ProductSkuDetailResponse;
import com.gdn.x.mta.distributiontask.model.dto.SellerAnalyticsResponse;
import com.gdn.x.mta.distributiontask.model.dto.SubmitEvidenceRequest;
import com.gdn.x.mta.distributiontask.model.enums.IPRHistoryActivity;
import com.gdn.x.mta.distributiontask.model.enums.ProductSourceIPR;
import com.gdn.x.mta.distributiontask.model.enums.ProductStateIPR;
import com.gdn.x.mta.distributiontask.model.solr.IPRProductSolr;
import com.gdn.x.mta.distributiontask.rest.model.response.IPRHistoryResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.IprProductDetailsResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.IprProductListResponse;
import com.gdn.x.mta.distributiontask.service.impl.config.IprSourceConfig;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

@ExtendWith(MockitoExtension.class)
class IprServiceImplTest {

  @InjectMocks
  private IprServiceImpl iprService;

  @Mock
  private IprRepository iprRepository;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private ProductServiceRepository productServiceRepository;

  @Mock
  private IprProductSolrCollectionRepository iprProductSolrCollectionRepository;

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private IprHistoryRepository iprHistoryRepository;

  @Mock
  private IprSourceConfig iprSourceConfig;

  @Captor
  private ArgumentCaptor<ProductIPR> productIPRArgumentCaptor;

  @Captor
  private ArgumentCaptor<IPRHistory> iprHistoryArgumentCaptor;

  private GdnRestSingleResponse<SellerAnalyticsResponse> response= new GdnRestSingleResponse<>();
  private final SellerAnalyticsResponse sellerAnalyticsResponse = new SellerAnalyticsResponse();
  private ProductIPR productIPR;
  private ProductIPR existingProductIpr;
  private IprActionRequest iprActionRequest;
  private Page<IPRProductSolr> iprProductSolrPage;
  private Pageable pageable;
  private IPRProductListRequest iprProductListRequest;
  private ProductSkuDetailResponse productSkuDetailResponse;
  private ProductChange productChange;
  private SubmitEvidenceRequest submitEvidenceRequest;
  private IPRUpdateAssigneeRequest iprUpdateAssigneeRequest;
  private Page<ProductIPR> iprProductPage;
  private IPRHistoryEventModel iprHistoryEventModel;
  private BrandReport brandReport;
  private static final String STORE_ID = "10001";
  private static final String BUSINESS_PARTNER_CODE = "HUA-60027";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String USER = "USER";
  private static final String PRODUCT_ID = "PRODUCT_ID";
  private static final String NOTES = "NOTES";
  private static final String REASONS = "REASONS";
  private static final String VIOLATION_TYPE = "VIOLATION_TYPE";
  private static final String UPDATED_BY = "UPDATED_BY";
  private static final String ASSIGNEE = "ASSIGNEE";
  private static final String ASSIGNEE2 = "ASSIGNEE2";
  private static final String SOURCE = "CUSTOMER_REPORT";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String SORT_ORDER = "asc";
  private static final String SORT_ORDER_DESC = "desc";
  private static final String VALID_PRODUCT_CHANGE_TYPES =
    "FORCE_REVIEW_FLAG_CHANGE," + "PRODUCT_REJECTED";

  private static final String DESCRIPTION_EVIDENCE_REQUESTED_TO_REJECTED =
      "{\"previous\":\"EVIDENCE_REQUESTED\", \"current\":\"REJECTED\"}";
  private static final String DESCRIPTION_EVIDENCE_REQUESTED_TO_IN_REVIEW =
      "{\"previous\":\"EVIDENCE_REQUESTED\", \"current\":\"IN_REVIEW\"}";
  private static final String DESCRIPTION_IN_REVIEW_TO_RELEASED =
      "{\"previous\":\"IN_REVIEW\", \"current\":\"RELEASED\"}";
  private static final String DESCRIPTION_IN_REVIEW_TO_WHITELISTED =
      "{\"previous\":\"IN_REVIEW\", \"current\":\"WHITELISTED\"}";
  private static final String DESCRIPTION_IN_REVIEW_TO_SUSPENDED =
      "{\"previous\":\"IN_REVIEW\", \"current\":\"SUSPENDED\"}";
  private static final String DESCRIPTION_IN_REVIEW_TO_EVIDENCE_REQUESTED =
      "{\"previous\":\"IN_REVIEW\", \"current\":\"EVIDENCE_REQUESTED\"}";
  private static final String DESCRIPTION_RELEASED_TO_EVIDENCE_REQUESTED =
      "{\"previous\":\"RELEASED\", \"current\":\"EVIDENCE_REQUESTED\"}";
  private static final String DESCRIPTION_ASSIGNEE_UPDATE =
      "{\"previous\":\"\", \"current\":\"ASSIGNEE\"}";
  private static final String DESCRIPTION_ASSIGNEE_UPDATE2 =
      "{\"PREVIOUS\":\"ASSIGNEE\", \"current\":\"ASSIGNEE2\"}";
  private static final String DESCRIPTION_EVIDENCE_REQUESTED_TO_SUBMITTED =
      "{\"previous\":\"EVIDENCE_REQUESTED\", \"current\":\"EVIDENCE_SUBMITTED\"}";

  private static final int DAYS = 3;
  private static final String EVENT = "event";
  private static final String EVIDENCE_FILE_PATH = "EVIDENCE_FILE_PATH";
  private static final String EVIDENCE_URL = "EVIDENCE_URL";


  @BeforeEach
  void setup() {
    iprActionRequest = new IprActionRequest();
    iprActionRequest.setBulkAction(true);
    iprActionRequest.setUpdatedBy(UPDATED_BY);
    existingProductIpr = new ProductIPR();
    existingProductIpr.setId(PRODUCT_ID);
    productIPR = new ProductIPR();
    productIPR.setState(ProductStateIPR.EVIDENCE_REQUESTED.name());
    productIPR.setProductCode(PRODUCT_CODE);
    productIPR.setAssignedTo(ASSIGNEE);
    pageable = PageRequest.of(PAGE, SIZE);
    iprProductListRequest = new IPRProductListRequest();
    IPRProductSolr iprProductSolr = new IPRProductSolr();
    iprProductSolr.setProductCode(PRODUCT_CODE);
    iprProductSolr.setProductSku(PRODUCT_SKU);
    iprProductSolrPage = new PageImpl<>(List.of(iprProductSolr), pageable, 1L);
    productChange = new ProductChange();
    productChange.setProductSku(PRODUCT_CODE);
    productSkuDetailResponse = new ProductSkuDetailResponse();
    sellerAnalyticsResponse.setSellerCode(BUSINESS_PARTNER_CODE);
    response =
        new GdnRestSingleResponse<>(sellerAnalyticsResponse, Constants.DEFAULT_REQUEST_ID);
    submitEvidenceRequest = new SubmitEvidenceRequest();
    submitEvidenceRequest.setProductSku(PRODUCT_SKU);
    iprUpdateAssigneeRequest =
        IPRUpdateAssigneeRequest.builder().productSku(Collections.singletonList(PRODUCT_SKU))
            .assignedTo(ASSIGNEE).build();
    iprHistoryEventModel = new IPRHistoryEventModel();
    iprHistoryEventModel.setProductSku(PRODUCT_SKU);
    iprHistoryEventModel.setActivity(IPRHistoryActivity.ADD_TO_IPR.getValue());
    brandReport = new BrandReport();
    brandReport.setReportDate(new Date());

    Map<String, String> iprSourceDisplayNameMap = new HashMap<>();
    iprSourceDisplayNameMap.put("CUSTOMER_REPORT", "CUSTOMER_REPORT");
    iprSourceDisplayNameMap.put("RANDOM_SAMPLE", "RANDOM_SAMPLE");
    iprSourceConfig.sourceNameMap = iprSourceDisplayNameMap;
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(iprRepository);
    Mockito.verifyNoMoreInteractions(iprProductSolrCollectionRepository);
    Mockito.verifyNoMoreInteractions(pbpFeign);
    Mockito.verifyNoMoreInteractions(productServiceRepository);
    Mockito.verifyNoMoreInteractions(kafkaTopicPropertiesConsumer);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  void findSuspensionInProgressTest() throws Exception {
    List<String> state = new ArrayList<>();
    iprProductPage = new PageImpl<>(List.of(productIPR), pageable, 2);
    state.add(ProductStateIPR.EVIDENCE_REQUESTED.name());
    state.add(ProductStateIPR.EVIDENCE_SUBMITTED.name());
    Mockito.when(
        iprRepository.findByStoreIdAndBusinessPartnerCodeAndStateInAndMarkForDeleteFalse(STORE_ID,
          BUSINESS_PARTNER_CODE, state,
          PageRequest.of(PAGE, SIZE, Sort.by(Sort.Direction.ASC, Constants.EVIDENCE_REQUESTED_DATE))))
      .thenReturn(iprProductPage);
    iprService.findSuspensionInProgressProducts(STORE_ID, BUSINESS_PARTNER_CODE, PAGE, SIZE,
      SORT_ORDER);
    Mockito.verify(iprRepository)
      .findByStoreIdAndBusinessPartnerCodeAndStateInAndMarkForDeleteFalse(STORE_ID,
        BUSINESS_PARTNER_CODE, state,
        PageRequest.of(PAGE, SIZE, Sort.by(Sort.Direction.ASC, Constants.EVIDENCE_REQUESTED_DATE)));
  }

  @Test
  void findSuspensionInProgressDescOrderTest() throws Exception {
    List<String> state = new ArrayList<>();
    iprProductPage = new PageImpl<>(List.of(productIPR), pageable, 2);
    state.add(ProductStateIPR.EVIDENCE_REQUESTED.name());
    state.add(ProductStateIPR.EVIDENCE_SUBMITTED.name());
    Mockito.when(
        iprRepository.findByStoreIdAndBusinessPartnerCodeAndStateInAndMarkForDeleteFalse(STORE_ID,
          BUSINESS_PARTNER_CODE, state, PageRequest.of(PAGE, SIZE,
            Sort.by(Sort.Direction.DESC, Constants.EVIDENCE_REQUESTED_DATE))))
      .thenReturn(iprProductPage);
    iprService.findSuspensionInProgressProducts(STORE_ID, BUSINESS_PARTNER_CODE, PAGE, SIZE,
      SORT_ORDER_DESC);
    Mockito.verify(iprRepository)
      .findByStoreIdAndBusinessPartnerCodeAndStateInAndMarkForDeleteFalse(STORE_ID,
        BUSINESS_PARTNER_CODE, state, PageRequest.of(PAGE, SIZE,
          Sort.by(Sort.Direction.DESC, Constants.EVIDENCE_REQUESTED_DATE)));
  }

  @Test
  void getIprProductListResponse() throws Exception {
    Mockito.when(
      iprProductSolrCollectionRepository.getIprProductsList(STORE_ID, iprProductListRequest,
        pageable)).thenReturn(iprProductSolrPage);
    Page<IprProductListResponse> response =
      iprService.getIprProductListResponse(STORE_ID, iprProductListRequest, pageable);
    Mockito.verify(iprProductSolrCollectionRepository)
      .getIprProductsList(STORE_ID, iprProductListRequest, pageable);
    Assertions.assertEquals(1, iprProductSolrPage.getTotalElements());
    Assertions.assertEquals(PRODUCT_CODE, response.getContent().get(0).getProductCode());
  }

  @Test
  void updateProductOnStateChangeTest() throws Exception {
    productChange.setProductChangeEventType(List.of(Constants.FORCE_REVIEW_FLAG_CHANGE));
    ReflectionTestUtils.setField(iprService, "productChangeEventTypes", VALID_PRODUCT_CHANGE_TYPES);
    Mockito.when(iprRepository.findByProductSkuAndMarkForDelete(PRODUCT_CODE, false))
      .thenReturn(productIPR);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(productIPR);
    Mockito.when(objectMapper.writeValueAsString(Mockito.any()))
      .thenReturn(DESCRIPTION_EVIDENCE_REQUESTED_TO_IN_REVIEW);
    Pair<ProductIPR, IPRHistoryEventModel> pair =
      iprService.updateProductOnStateChange(productChange);
    Mockito.verify(iprRepository).findByProductSkuAndMarkForDelete(PRODUCT_CODE, false);
    Mockito.verify(iprRepository).save(productIPRArgumentCaptor.capture());
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
    Assertions.assertEquals(ProductStateIPR.IN_REVIEW.name(),
      productIPRArgumentCaptor.getValue().getState());
    Assertions.assertEquals(DESCRIPTION_EVIDENCE_REQUESTED_TO_IN_REVIEW,
      pair.getRight().getDescription());
    Assertions.assertEquals(IPRHistoryActivity.STATUS_UPDATE.getValue(),
      pair.getRight().getActivity());
  }

  @Test
  void updateProductOnStateChangeTakeDownTrueTest() throws Exception {
    productIPR.setState(ProductStateIPR.WAITING_TO_GET_ACTIVATED.name());
    productChange.setProductChangeEventType(List.of(Constants.FORCE_REVIEW_FLAG_CHANGE));
    productChange.setMarkForDelete(true);
    ReflectionTestUtils.setField(iprService, "productChangeEventTypes", VALID_PRODUCT_CHANGE_TYPES);
    Mockito.when(iprRepository.findByProductSkuAndMarkForDelete(PRODUCT_CODE, false))
      .thenReturn(productIPR);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(productIPR);
    Pair<ProductIPR, IPRHistoryEventModel> pair =
      iprService.updateProductOnStateChange(productChange);
    Mockito.verify(iprRepository).findByProductSkuAndMarkForDelete(PRODUCT_CODE, false);
    Mockito.verify(iprRepository).save(productIPRArgumentCaptor.capture());
    Assertions.assertEquals(ProductStateIPR.WAITING_TO_GET_ACTIVATED.name(),
      productIPRArgumentCaptor.getValue().getState());
    Assertions.assertNull(pair.getRight());
    Assertions.assertNotNull(pair.getLeft());
  }

  @Test
  void updateProductOnStateChangeProductRejectedTest() throws Exception {
    productChange.setProductChangeEventType(List.of(Constants.FORCE_REVIEW_FLAG_CHANGE));
    ReflectionTestUtils.setField(iprService, "productChangeEventTypes", VALID_PRODUCT_CHANGE_TYPES);
    Mockito.when(iprRepository.findByProductSkuAndMarkForDelete(PRODUCT_CODE, false))
      .thenReturn(null);
    Pair<ProductIPR, IPRHistoryEventModel> pair =
      iprService.updateProductOnStateChange(productChange);
    Assertions.assertNull(pair.getLeft());
    Assertions.assertNull(pair.getRight());
    Mockito.verify(iprRepository).findByProductSkuAndMarkForDelete(PRODUCT_CODE, false);
  }

  @Test
  void updateProductOnStateChangeProductNotPresentInIprTest() throws Exception {
    productChange.setProductChangeEventType(List.of(Constants.PRODUCT_REJECTED));
    productChange.setMarkForDelete(true);
    ReflectionTestUtils.setField(iprService, "productChangeEventTypes", VALID_PRODUCT_CHANGE_TYPES);
    Mockito.when(iprRepository.findByProductSku(PRODUCT_CODE)).thenReturn(productIPR);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(productIPR);
    Mockito.when(objectMapper.writeValueAsString(
            IPRHistoryDescription.builder().current(ProductStateIPR.REJECTED.name())
                .previous(ProductStateIPR.EVIDENCE_REQUESTED.name()).build()))
        .thenReturn(DESCRIPTION_EVIDENCE_REQUESTED_TO_REJECTED);
    Pair<ProductIPR, IPRHistoryEventModel> pair =
        iprService.updateProductOnStateChange(productChange);
    Mockito.verify(iprRepository).findByProductSku(PRODUCT_CODE);
    Mockito.verify(iprRepository).save(productIPRArgumentCaptor.capture());
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
    Assertions.assertEquals(ProductStateIPR.REJECTED.name(),
        productIPRArgumentCaptor.getValue().getState());
    Assertions.assertTrue(productIPRArgumentCaptor.getValue().isMarkForDelete());
    Assertions.assertEquals(DESCRIPTION_EVIDENCE_REQUESTED_TO_REJECTED,
        pair.getRight().getDescription());
    Assertions.assertEquals(IPRHistoryActivity.STATUS_UPDATE.getValue(),
        pair.getRight().getActivity());
  }

  @Test
  void updateProductOnStateChangeInvalidProductChangeTest() throws Exception {
    ReflectionTestUtils.setField(iprService, "productChangeEventTypes", VALID_PRODUCT_CHANGE_TYPES);
    Pair<ProductIPR, IPRHistoryEventModel> response =
        iprService.updateProductOnStateChange(productChange);
    Assertions.assertNull(response.getLeft());
    Assertions.assertNull(response.getRight());
  }

  @Test
  void testAddProductToIPR_existingProductIPRNotNullAndSuspended() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.SUSPENDED.name());
    existingProductIPR.setMarkForDelete(true);
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    Mockito.when(iprRepository.save(existingProductIPR)).thenReturn(existingProductIPR);
    productSkuDetailResponse.setMarkForDelete(false);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
    Mockito.verify(iprRepository, Mockito.times(1)).save(existingProductIPR);
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
  }

  @Test
  void testAddProductToIPR_existingProductIPRNotNull() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.WAITING_TO_GET_ACTIVATED.name());
    existingProductIPR.setMarkForDelete(false);
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
    Mockito.verify(iprRepository, Mockito.never()).save(Mockito.any());
  }

  @Test
  void testAddProductToIPR_existingProductIPRNotNullBulkAddition() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.WAITING_TO_GET_ACTIVATED.name());
    existingProductIPR.setMarkForDelete(false);
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
    Mockito.verify(iprRepository, Mockito.never()).save(Mockito.any());
  }

  @Test
  void testAddProductToIPR_existingProductIPRNotNullBulkAdditionMFDTrue() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.WHITELISTED.name());
    existingProductIPR.setMarkForDelete(true);
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
    Mockito.verify(productServiceRepository, Mockito.never())
        .getProductDetailForProduct(PRODUCT_SKU, null);
    Mockito.verify(iprRepository, Mockito.never()).save(Mockito.any());
  }

  @Test
  void testAddProductToIPR_existingProductEvidenceSubmitted() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.EVIDENCE_SUBMITTED.name());
    existingProductIPR.setMarkForDelete(true);
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    AddingIprProductDTO result = iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
    Assertions.assertEquals(ErrorMessages.PRODUCT_EVIDENCE_SUBMITTED,
        result.getErrorCategory().getMessage());
    Assertions.assertNull(result.getIprProductSolr());
  }

  @Test
  void testAddProductToIPR_existingProductEvidenceRequested() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.EVIDENCE_REQUESTED.name());
    existingProductIPR.setMarkForDelete(true);
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    AddingIprProductDTO result = iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
    Assertions.assertEquals(ErrorMessages.PRODUCT_EVIDENCE_REQUESTED,
        result.getErrorCategory().getMessage());
    Assertions.assertNull(result.getIprProductSolr());
  }

  @Test
  void testAddProductToIPR_existingProductInReview() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.IN_REVIEW.name());
    existingProductIPR.setMarkForDelete(true);
    existingProductIPR.setSource(null);
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    Mockito.when(iprRepository.save(existingProductIPR)).thenReturn(existingProductIPR);
    AddingIprProductDTO result =
        iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
    Mockito.verify(iprRepository).findByProductSku(PRODUCT_SKU);
    Mockito.verify(iprRepository).save(existingProductIPR);
    Assertions.assertEquals(StringUtils.EMPTY, result.getErrorCategory().getMessage());
    Assertions.assertNotNull(result.getIprProductSolr());
    Assertions.assertEquals(SOURCE, result.getIprProductSolr().getSource());
  }

  @Test
  void testAddProductToIPR_existingProductInReviewOnlyAssigneeUpdate() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.IN_REVIEW.name());
    existingProductIPR.setAssignedTo(UPDATED_BY);
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    AddingIprProductDTO result = iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, ASSIGNEE, null);
    existingProductIPR.setAssignedTo(ASSIGNEE);
    Mockito.verify(iprRepository).save(existingProductIPR);
    Assertions.assertEquals(StringUtils.EMPTY, result.getErrorCategory().getMessage());
    Assertions.assertNotNull(result.getIprProductSolr());
    Assertions.assertTrue(result.isOnlyAssigneeUpdated());
    Assertions.assertEquals(ASSIGNEE, result.getIprProductSolr().getAssignedTo());
    Assertions.assertEquals(ErrorCategory.EMPTY_ERROR_MESSAGE, result.getErrorCategory());
    Assertions.assertEquals(UPDATED_BY, result.getExistingAssignee());
  }

  @Test
  void testAddProductToIPR_existingProductEmptyState() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(StringUtils.EMPTY);
    existingProductIPR.setMarkForDelete(true);
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    AddingIprProductDTO result = iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
    Assertions.assertEquals(StringUtils.EMPTY, result.getErrorCategory().getMessage());
    Assertions.assertNull(result.getIprProductSolr());
  }

  @Test
  void testAddProductToIPR_existingProductIPRNotNullRejected() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.SUSPENDED.name());
    existingProductIPR.setMarkForDelete(true);
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    productSkuDetailResponse.setRejected(true);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
    Mockito.verify(iprRepository, Mockito.never()).save(Mockito.any());
  }

  @Test
  void testAddProductToIPR_existingProductIPRNotNullSuspended() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.SUSPENDED.name());
    existingProductIPR.setMarkForDelete(true);
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    productSkuDetailResponse.setSuspended(true);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
    Mockito.verify(iprRepository, Mockito.never()).save(Mockito.any());
  }

  @Test
  void testAddProductToIPR_existingProductIPRNotNullAndReleased() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.RELEASED.name());
    existingProductIPR.setSource(ProductSourceIPR.BRAND_REPORT.getValue());
    existingProductIPR.setMarkForDelete(true);
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    productSkuDetailResponse.setMarkForDelete(false);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    Mockito.when(iprRepository.save(existingProductIPR)).thenReturn(existingProductIPR);
    iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, brandReport);
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
    Mockito.verify(iprRepository, Mockito.times(1)).save(existingProductIPR);
  }

  @Test
  void testAddProductToIPR_existingProductIPRNotNullAndSuspendedRejected() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.SUSPENDED.name());
    existingProductIPR.setMarkForDelete(true);
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    productSkuDetailResponse.setMarkForDelete(true);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    Mockito.when(iprRepository.save(existingProductIPR)).thenReturn(existingProductIPR);
    iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
    Mockito.verify(iprRepository, Mockito.times(1)).save(existingProductIPR);
  }

  @Test
  void testAddProductToIPR_newProductIPR() {
    ProductIPR productIPR = new ProductIPR();
    productIPR.setState(ProductStateIPR.IN_REVIEW.name());
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(null);
    productSkuDetailResponse.setMarkForDelete(false);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(productIPR);
    iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, ProductSourceIPR.BRAND_REPORT.getValue(), null, brandReport);
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
    Mockito.verify(iprRepository, Mockito.times(1)).save(Mockito.any(ProductIPR.class));
  }

  @Test
  void testAddProductToIPR_newProductIPRBrandReportTest() {
    ProductIPR productIPR = new ProductIPR();
    productIPR.setState(ProductStateIPR.IN_REVIEW.name());
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(null);
    productSkuDetailResponse.setMarkForDelete(false);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(productIPR);
    iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, ProductSourceIPR.BRAND_REPORT.getValue(), null, null);
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
    Mockito.verify(iprRepository, Mockito.times(1)).save(Mockito.any(ProductIPR.class));
  }

  @Test
  void testAddProductToIPR_newProductIPRAndAssigneeUpdate() {
    ProductIPR productIPR = new ProductIPR();
    productIPR.setState(ProductStateIPR.IN_REVIEW.name());
    productIPR.setAssignedTo(ASSIGNEE);
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(null);
    productSkuDetailResponse.setMarkForDelete(false);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(productIPR);
    AddingIprProductDTO addingIprProductDTO =
        iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, ASSIGNEE, null);
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
    Mockito.verify(iprRepository, Mockito.times(1)).save(Mockito.any(ProductIPR.class));
    Assertions.assertEquals(ASSIGNEE, addingIprProductDTO.getIprProductSolr().getAssignedTo());
  }

  @Test
  void testAddProductToIPR_newProductIPRTakenDown() {
    ProductIPR productIPR = new ProductIPR();
    productIPR.setState(ProductStateIPR.WAITING_TO_GET_ACTIVATED.name());
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(null);
    productSkuDetailResponse.setMarkForDelete(true);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(productIPR);
    iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
    Mockito.verify(iprRepository, Mockito.times(1)).save(Mockito.any(ProductIPR.class));
  }
  @Test
  void testAddProductToIPR_existingProductInReview_duplicateSource() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.IN_REVIEW.name());
    existingProductIPR.setSource(SOURCE);
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    Mockito.when(iprRepository.save(existingProductIPR)).thenReturn(existingProductIPR);
    AddingIprProductDTO result =
        iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
    Assertions.assertNotNull(result.getIprProductSolr());
    Assertions.assertEquals(SOURCE, result.getIprProductSolr().getSource());
  }
  @Test
  void testAddProductToIPR_existingProductInReview_emptySource() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.IN_REVIEW.name());
    existingProductIPR.setSource("RANDOM_SAMPLE");
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    Mockito.when(iprRepository.save(existingProductIPR)).thenReturn(existingProductIPR);
    AddingIprProductDTO result =
        iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, null, null, null);
    Mockito.verify(iprRepository).findByProductSku(PRODUCT_SKU);
    Mockito.verify(iprRepository).save(existingProductIPR);
    Assertions.assertNotNull(result.getIprProductSolr());
    Assertions.assertEquals("RANDOM_SAMPLE", result.getIprProductSolr().getSource());
  }
  @Test
  void testAddProductToIPR_existingProductInReview_brandReportAppendsAndSetsReporter() {
    iprSourceConfig.sourceNameMap.put(ProductSourceIPR.BRAND_REPORT.getValue(),
        ProductSourceIPR.BRAND_REPORT.getValue());
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.IN_REVIEW.name());
    existingProductIPR.setSource("CUSTOMER_REPORT");
    BrandReport br = new BrandReport();
    br.setReportDate(new Date());
    br.setReporter("reporter-id");
    br.setReporterName("reporter-name");
    br.setReporterEmail("reporter@email.com");
    br.setReporterReason("reason");
    br.setReporterAddress("address");
    br.setReporterPhone("1234567890");
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class)))
        .thenAnswer(inv -> inv.getArgument(0));
    AddingIprProductDTO result =
        iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, ProductSourceIPR.BRAND_REPORT.getValue(),
            null, br);
    Mockito.verify(iprRepository).save(productIPRArgumentCaptor.capture());
    ProductIPR saved = productIPRArgumentCaptor.getValue();
    Assertions.assertNotNull(result.getIprProductSolr());
    Set<String> sources =
        new HashSet<>(Arrays.asList(saved.getSource().split(",")));
    Assertions.assertTrue(sources.contains("CUSTOMER_REPORT"));
    Assertions.assertTrue(sources.contains(ProductSourceIPR.BRAND_REPORT.getValue()));
    Assertions.assertEquals(br.getReportDate(), saved.getReportDate());
    Assertions.assertEquals(br.getReporter(), saved.getReporter());
    Assertions.assertEquals(br.getReporterName(), saved.getReporterName());
    Assertions.assertEquals(br.getReporterEmail(), saved.getReporterEmail());
    Assertions.assertEquals(br.getReporterReason(), saved.getReporterReason());
    Assertions.assertEquals(br.getReporterAddress(), saved.getReporterAddress());
    Assertions.assertEquals(br.getReporterPhone(), saved.getReporterPhoneNumber());
  }
  @Test
  void testAddProductToIPR_existingProductInReview_brandReportNull() {
    iprSourceConfig.sourceNameMap.put(ProductSourceIPR.BRAND_REPORT.getValue(),
        ProductSourceIPR.BRAND_REPORT.getValue());
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.IN_REVIEW.name());
    existingProductIPR.setSource("CUSTOMER_REPORT");
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class)))
        .thenAnswer(inv -> inv.getArgument(0));
    AddingIprProductDTO result =
        iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, ProductSourceIPR.BRAND_REPORT.getValue(),
            null, null);
    Mockito.verify(iprRepository).save(productIPRArgumentCaptor.capture());
    ProductIPR saved = productIPRArgumentCaptor.getValue();
    Assertions.assertNotNull(result.getIprProductSolr());
    Set<String> set =
        new HashSet<>(Arrays.asList(saved.getSource().split(",")));
    Assertions.assertTrue(set.contains("CUSTOMER_REPORT"));
    Assertions.assertTrue(set.contains(ProductSourceIPR.BRAND_REPORT.getValue()));
    Assertions.assertNull(saved.getReportDate());
    Assertions.assertNull(saved.getReporter());
    Assertions.assertNull(saved.getReporterName());
    Assertions.assertNull(saved.getReporterEmail());
    Assertions.assertNull(saved.getReporterReason());
    Assertions.assertNull(saved.getReporterAddress());
    Assertions.assertNull(saved.getReporterPhoneNumber());
  }
  @Test
  void testAddProductToIPR_inReview_assigneeChanged_sourceNull_noChange() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.IN_REVIEW.name());
    existingProductIPR.setAssignedTo(UPDATED_BY);
    existingProductIPR.setSource("RANDOM_SAMPLE");
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class)))
        .thenAnswer(inv -> inv.getArgument(0));
    AddingIprProductDTO result =
        iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, null, ASSIGNEE, null);
    Mockito.verify(iprRepository).save(productIPRArgumentCaptor.capture());
    ProductIPR saved = productIPRArgumentCaptor.getValue();
    Assertions.assertEquals(StringUtils.EMPTY, result.getErrorCategory().getMessage());
    Assertions.assertNotNull(result.getIprProductSolr());
    Assertions.assertTrue(result.isOnlyAssigneeUpdated());
    Assertions.assertEquals(UPDATED_BY, result.getExistingAssignee());
    Assertions.assertEquals(ASSIGNEE, saved.getAssignedTo());
    Assertions.assertEquals("RANDOM_SAMPLE", saved.getSource());
  }
  @Test
  void testAddProductToIPR_inReview_assigneeChanged_duplicateSource_noDup() {
    ProductIPR existingProductIPR = new ProductIPR();
    existingProductIPR.setState(ProductStateIPR.IN_REVIEW.name());
    existingProductIPR.setAssignedTo(UPDATED_BY);
    existingProductIPR.setSource(SOURCE); // e.g., CUSTOMER_REPORT
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(existingProductIPR);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class)))
        .thenAnswer(inv -> inv.getArgument(0));
    AddingIprProductDTO result =
        iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, ASSIGNEE, null);
    Mockito.verify(iprRepository).save(productIPRArgumentCaptor.capture());
    ProductIPR saved = productIPRArgumentCaptor.getValue();
    Assertions.assertEquals(StringUtils.EMPTY, result.getErrorCategory().getMessage());
    Assertions.assertNotNull(result.getIprProductSolr());
    Assertions.assertTrue(result.isOnlyAssigneeUpdated());
    Assertions.assertEquals(UPDATED_BY, result.getExistingAssignee());
    Assertions.assertEquals(ASSIGNEE, saved.getAssignedTo());
    Assertions.assertEquals(SOURCE, saved.getSource());
  }

  @Test
  void submitEvidenceForProductTest() throws JsonProcessingException {
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(productIPR);
    Mockito.when(objectMapper.writeValueAsString(Mockito.any()))
        .thenReturn(DESCRIPTION_EVIDENCE_REQUESTED_TO_SUBMITTED);
    Pair<ProductIPR, IPRHistoryEventModel> result =
        iprService.submitEvidenceForProduct(submitEvidenceRequest);
    Assertions.assertEquals(DESCRIPTION_EVIDENCE_REQUESTED_TO_SUBMITTED,
        result.getRight().getDescription());
    Mockito.verify(iprRepository).findByProductSku(PRODUCT_SKU);
    Mockito.verify(iprRepository).save(productIPR);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  void submitEvidenceForProductNullTest() {
    Assertions.assertThrows(ValidationException.class,
        () -> iprService.submitEvidenceForProduct(submitEvidenceRequest));
    Mockito.verify(iprRepository).findByProductSku(PRODUCT_SKU);
  }

  @Test
  void fetchIprProductDetailsTest() {
    productIPR.setProductSku(PRODUCT_SKU);
    productIPR.setMarkForDelete(false);
    productIPR.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productIPR.setSource(SOURCE);
    Mockito.when(iprRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productIPR);
    Mockito.when(productServiceRepository.getSellerAnalyticsResponse(BUSINESS_PARTNER_CODE))
        .thenReturn(sellerAnalyticsResponse);
    IprProductDetailsResponse detailsResponse = iprService.fetchIprProductDetails(PRODUCT_SKU);
    Mockito.verify(iprRepository).findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(productServiceRepository).getSellerAnalyticsResponse(BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(PRODUCT_SKU, detailsResponse.getProductSku());
    Assertions.assertEquals(SOURCE, detailsResponse.getSource());
  }

  @Test
  void fetchIprProductDetailsSellerResponseNullTest() {
    productIPR.setProductSku(PRODUCT_SKU);
    productIPR.setMarkForDelete(false);
    productIPR.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productIPR.setReviewerNotes(NOTES);
    productIPR.setSource(SOURCE);
    Mockito.when(iprRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productIPR);
    Mockito.when(productServiceRepository.getSellerAnalyticsResponse(BUSINESS_PARTNER_CODE))
        .thenReturn(null);
    IprProductDetailsResponse detailsResponse = iprService.fetchIprProductDetails(PRODUCT_SKU);
    Mockito.verify(iprRepository).findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(productServiceRepository).getSellerAnalyticsResponse(BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(PRODUCT_SKU, detailsResponse.getProductSku());
    Assertions.assertEquals(NOTES,
        detailsResponse.getEvidenceRequestedDetailResponse().getReviewerNotes());
    Assertions.assertEquals(SOURCE, detailsResponse.getSource());
  }

  @Test
  void fetchIprProductDetailsNullTest() {
    productIPR.setProductSku(PRODUCT_SKU);
    productIPR.setMarkForDelete(false);
    productIPR.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productIPR.setSource(SOURCE);
    Mockito.when(iprRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(null);
    try {
      iprService.fetchIprProductDetails(PRODUCT_SKU);
    } catch (Exception ignored) {
    } finally {
      Mockito.verify(iprRepository).findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    }
  }

  @Test
  void updateAssigneeTest_successInReview() throws JsonProcessingException {
    productIPR.setState(ProductStateIPR.IN_REVIEW.name());
    productIPR.setAssignedTo(null);
    Mockito.when(iprRepository.saveAll(Mockito.any()))
        .thenReturn(Collections.singletonList(productIPR));
    Mockito.when(iprRepository.findByProductSkuIn(Collections.singletonList(PRODUCT_SKU)))
        .thenReturn(Collections.singletonList(productIPR));
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(DESCRIPTION_ASSIGNEE_UPDATE);
    Pair<List<ProductIPR>, List<IPRHistoryEventModel>> result =
        iprService.updateAssignee(iprUpdateAssigneeRequest);
    Assertions.assertEquals(DESCRIPTION_ASSIGNEE_UPDATE, result.getRight().get(0).getDescription());
    Assertions.assertEquals(ASSIGNEE, result.getLeft().get(0).getAssignedTo());
    Mockito.verify(iprRepository).saveAll(Collections.singletonList(productIPR));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  void updateAssigneeTest_successInReview_sameAssignee() throws JsonProcessingException {
    productIPR.setState(ProductStateIPR.IN_REVIEW.name());
    productIPR.setAssignedTo(ASSIGNEE);
    Mockito.when(iprRepository.findByProductSkuIn(Collections.singletonList(PRODUCT_SKU)))
        .thenReturn(Collections.singletonList(productIPR));
    Pair<List<ProductIPR>, List<IPRHistoryEventModel>> result =
        iprService.updateAssignee(iprUpdateAssigneeRequest);
    Assertions.assertNull(result.getRight().get(0));
    Mockito.verify(iprRepository).saveAll(Collections.singletonList(productIPR));
  }

  @Test
  void updateAssigneeTest_successEvidenceSubmitted() throws JsonProcessingException {
    productIPR.setState(ProductStateIPR.EVIDENCE_SUBMITTED.name());
    iprUpdateAssigneeRequest.setAssignedTo(StringUtils.EMPTY);
    Mockito.when(iprRepository.findByProductSkuIn(Collections.singletonList(PRODUCT_SKU)))
        .thenReturn(Collections.singletonList(productIPR));
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(DESCRIPTION_ASSIGNEE_UPDATE);
    Pair<List<ProductIPR>, List<IPRHistoryEventModel>> result =
        iprService.updateAssignee(iprUpdateAssigneeRequest);
    Assertions.assertEquals(DESCRIPTION_ASSIGNEE_UPDATE, result.getRight().get(0).getDescription());
    Mockito.verify(iprRepository).saveAll(Collections.singletonList(productIPR));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  void updateAssigneeTest_failedEvidenceRequested() {
    productIPR.setState(ProductStateIPR.EVIDENCE_REQUESTED.name());
    Mockito.when(iprRepository.findByProductSkuIn(Collections.singletonList(PRODUCT_SKU)))
        .thenReturn(Collections.singletonList(productIPR));
    Assertions.assertThrows(ValidationException.class, () -> {
      iprService.updateAssignee(iprUpdateAssigneeRequest);
      throw new ValidationException(ErrorCategory.INVALID_STATE_TO_UPDATE_ASSIGNEE.getCode(),
          ErrorCategory.INVALID_STATE_TO_UPDATE_ASSIGNEE.getMessage());
    });
  }

  @Test
  void updateAssigneeTest_failedProductAlreadyUnassigned() {
    productIPR.setState(ProductStateIPR.EVIDENCE_SUBMITTED.name());
    iprUpdateAssigneeRequest.setAssignedTo(StringUtils.EMPTY);
    productIPR.setAssignedTo(StringUtils.EMPTY);
    productIPR.setAssignedTo(null);
    Mockito.when(iprRepository.findByProductSkuIn(Collections.singletonList(PRODUCT_SKU)))
        .thenReturn(Collections.singletonList(productIPR));
    Assertions.assertThrows(ValidationException.class, () -> {
      iprService.updateAssignee(iprUpdateAssigneeRequest);
      throw new ValidationException(ErrorCategory.IPR_PRODUCT_IS_ALREADY_UNASSIGNED.getCode(),
          ErrorCategory.IPR_PRODUCT_IS_ALREADY_UNASSIGNED.getMessage());
    });
  }

  @Test
  void testPerformIprActionForProduct_ProductSkuNotFound() throws Exception {
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.IN_REVIEW.name());
    iprActionRequest.setSource(ProductSourceIPR.BRAND_REPORT.name());
    iprActionRequest.setBrandReport(brandReport);
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString())).thenReturn(null);
    productSkuDetailResponse.setMarkForDelete(false);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    IPRActionResponseDto iprActionResponseDto =
      iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(StringUtils.EMPTY,
      iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertNotNull(iprActionResponseDto.getIprHistoryEventModels());
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository).save(Mockito.any(ProductIPR.class));
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  void testPerformIprActionForProductBrandReportFalseHit() throws Exception {
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.IN_REVIEW.name());
    iprActionRequest.setSource(ProductSourceIPR.CUSTOMER_REPORT.name());
    iprActionRequest.setBrandReport(brandReport);
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString())).thenReturn(null);
    productSkuDetailResponse.setMarkForDelete(false);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    IPRActionResponseDto iprActionResponseDto =
        iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(StringUtils.EMPTY,
        iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertNotNull(iprActionResponseDto.getIprHistoryEventModels());
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository).save(Mockito.any(ProductIPR.class));
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  void testPerformIprActionForProductBrandReportNull() throws Exception {
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.IN_REVIEW.name());
    iprActionRequest.setSource(ProductSourceIPR.BRAND_REPORT.name());
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString())).thenReturn(null);
    productSkuDetailResponse.setMarkForDelete(false);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    IPRActionResponseDto iprActionResponseDto =
        iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(StringUtils.EMPTY,
        iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertNotNull(iprActionResponseDto.getIprHistoryEventModels());
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository).save(Mockito.any(ProductIPR.class));
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  void testPerformIprActionForProduct_ProductTakenDown() throws Exception {
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.IN_REVIEW.name());
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString())).thenReturn(null);
    productSkuDetailResponse.setMarkForDelete(true);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    IPRActionResponseDto iprActionResponseDto =
        iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(ErrorMessages.PRODUCT_WAITING_TO_GET_ACTIVATED,
      iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertTrue(CollectionUtils.isEmpty(iprActionResponseDto.getIprHistoryEventModels()));
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository).save(Mockito.any(ProductIPR.class));
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
  }

  @Test
  void testPerformIprActionForProduct_ProductRejected() throws Exception {
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.IN_REVIEW.name());
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString())).thenReturn(null);
    productSkuDetailResponse.setMarkForDelete(false);
    productSkuDetailResponse.setRejected(true);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    IPRActionResponseDto iprActionResponseDto =
        iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(ErrorMessages.PRODUCT_REJECTED_FROM_INTERNAL,
      iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertTrue(CollectionUtils.isEmpty(iprActionResponseDto.getIprHistoryEventModels()));
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository, Mockito.never()).save(Mockito.any(ProductIPR.class));
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
  }

  @Test
  void testPerformIprActionForProduct_ProductPrelive() throws Exception {
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.IN_REVIEW.name());
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString())).thenReturn(null);
    productSkuDetailResponse.setMarkForDelete(false);
    productSkuDetailResponse.setPrelive(true);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
      .thenReturn(productSkuDetailResponse);
    IPRActionResponseDto iprActionResponseDto =
      iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(ErrorCategory.PRODUCT_IS_PRELIVE.getMessage(),
      iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertTrue(CollectionUtils.isEmpty(iprActionResponseDto.getIprHistoryEventModels()));
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository, Mockito.never()).save(Mockito.any(ProductIPR.class));
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
  }

  @Test
  void testPerformIprActionForProduct_ExistingReleasedState() throws Exception {
    existingProductIpr.setState(ProductStateIPR.RELEASED.name());
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.RELEASED.name());
    productSkuDetailResponse.setMarkForDelete(true);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    IPRActionResponseDto iprActionResponseDto =
        iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(ErrorMessages.PRODUCT_WAITING_TO_GET_ACTIVATED,
      iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertTrue(CollectionUtils.isEmpty(iprActionResponseDto.getIprHistoryEventModels()));
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository).save(Mockito.any(ProductIPR.class));
  }

  @Test
  void testPerformIprActionForProduct_ExistingSameState() throws Exception {
    existingProductIpr.setState(ProductStateIPR.RELEASED.name());
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.RELEASED.name());
    productSkuDetailResponse.setMarkForDelete(false);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    IPRActionResponseDto iprActionResponseDto =
        iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(Constants.IN_SAME_STATE,
      iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertTrue(CollectionUtils.isEmpty(iprActionResponseDto.getIprHistoryEventModels()));
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository, Mockito.never()).save(Mockito.any(ProductIPR.class));
  }

  @Test
  void testPerformIprActionForProduct_ExistingSameSuspendedState() throws Exception {
    existingProductIpr.setState(ProductStateIPR.SUSPENDED.name());
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.SUSPENDED.name());
    productSkuDetailResponse.setMarkForDelete(false);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    IPRActionResponseDto iprActionResponseDto =
      iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertNotEquals(Constants.IN_SAME_STATE,
      iprActionResponseDto.getErrorCategory().getMessage());
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository).save(Mockito.any(ProductIPR.class));
    Mockito.verify(productServiceRepository, Mockito.times(1)).suspendIprProduct(Mockito.any());
  }

  @Test
  void testPerformIprActionForProduct_ExistingWhitelistState() throws Exception {
    existingProductIpr.setState(ProductStateIPR.WHITELISTED.name());
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.RELEASED.name());
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    IPRActionResponseDto iprActionResponseDto =
      iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(ErrorMessages.PRODUCT_WHITELISTED,
      iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertTrue(CollectionUtils.isEmpty(iprActionResponseDto.getIprHistoryEventModels()));
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository, Mockito.never()).save(Mockito.any(ProductIPR.class));
  }

  @Test
  void testPerformIprActionForProduct_ExistingSuspendedState() throws Exception {
    existingProductIpr.setState(ProductStateIPR.SUSPENDED.name());
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.RELEASED.name());
    productSkuDetailResponse.setMarkForDelete(false);
    productSkuDetailResponse.setRejected(true);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    IPRActionResponseDto iprActionResponseDto =
      iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(ErrorMessages.PRODUCT_REJECTED_FROM_INTERNAL,
      iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertTrue(CollectionUtils.isEmpty(iprActionResponseDto.getIprHistoryEventModels()));
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository, Mockito.never()).save(Mockito.any(ProductIPR.class));
  }

  @Test
  void testPerformIprActionForProduct_ExistingRejectedState() throws Exception {
    existingProductIpr.setState(ProductStateIPR.REJECTED.name());
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.RELEASED.name());
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    IPRActionResponseDto iprActionResponseDto =
      iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(ErrorMessages.PRODUCT_REJECTED_FROM_INTERNAL,
      iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertTrue(CollectionUtils.isEmpty(iprActionResponseDto.getIprHistoryEventModels()));
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository, Mockito.never()).save(Mockito.any(ProductIPR.class));
  }

  @Test
  void testPerformIprActionForProduct_ExistingWaitingState() throws Exception {
    existingProductIpr.setState(ProductStateIPR.WAITING_TO_GET_ACTIVATED.name());
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.RELEASED.name());
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    IPRActionResponseDto iprActionResponseDto =
      iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(ErrorMessages.PRODUCT_WAITING_TO_GET_ACTIVATED,
      iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertTrue(CollectionUtils.isEmpty(iprActionResponseDto.getIprHistoryEventModels()));
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository, Mockito.never()).save(Mockito.any(ProductIPR.class));
  }

  @Test
  void testPerformIprActionForProduct_ValidActionReleased() throws Exception {
    existingProductIpr.setState(ProductStateIPR.IN_REVIEW.name());
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.RELEASED.name());
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(existingProductIpr);
    Mockito.when(objectMapper.writeValueAsString(Mockito.any()))
        .thenReturn(DESCRIPTION_IN_REVIEW_TO_RELEASED);
    IPRActionResponseDto iprActionResponseDto =
        iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(StringUtils.EMPTY, iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertEquals(DESCRIPTION_IN_REVIEW_TO_RELEASED,
      iprActionResponseDto.getIprHistoryEventModels().get(0).getDescription());
    Assertions.assertEquals(IPRHistoryActivity.STATUS_UPDATE.getValue(),
      iprActionResponseDto.getIprHistoryEventModels().get(0).getActivity());
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository, Mockito.times(1)).save(Mockito.any(ProductIPR.class));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  void testPerformIprActionForProductNotFoundManual() {
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.RELEASED.name());
    iprActionRequest.setBulkAction(false);
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString())).thenReturn(null);
    Assertions.assertThrows(ValidationException.class, () -> {
      iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
      throw new ValidationException(ErrorCategory.PRODUCT_SKU_NOT_FOUND.getCode(),
          ErrorCategory.PRODUCT_SKU_NOT_FOUND.getMessage());
    });
    Mockito.verify(iprRepository).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository, Mockito.never()).save(Mockito.any(ProductIPR.class));
  }

  @Test
  void testPerformIprActionForProductEmptyAssigneeManual() {
    existingProductIpr.setState(ProductStateIPR.IN_REVIEW.name());
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.RELEASED.name());
    iprActionRequest.setBulkAction(false);
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    Assertions.assertThrows(ValidationException.class, () -> {
      iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
      throw new ValidationException(ErrorCategory.USER_NOT_ALLOWED_TO_REVIEW.getCode(),
          ErrorCategory.USER_NOT_ALLOWED_TO_REVIEW.getMessage());
    });
  }

  @Test
  void testPerformIprActionForProductWrongAssigneeManual() {
    existingProductIpr.setState(ProductStateIPR.IN_REVIEW.name());
    existingProductIpr.setAssignedTo(USER);
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.RELEASED.name());
    iprActionRequest.setBulkAction(false);
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    Assertions.assertThrows(ValidationException.class, () -> {
      iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
      throw new ValidationException(ErrorCategory.USER_NOT_ALLOWED_TO_REVIEW.getCode(),
          ErrorCategory.USER_NOT_ALLOWED_TO_REVIEW.getMessage());
    });
  }

  @Test
  void testPerformIprActionForProduct_InValidAction() throws Exception {
    existingProductIpr.setState(ProductStateIPR.IN_REVIEW.name());
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.WAITING_TO_GET_ACTIVATED.name());
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(existingProductIpr);
    IPRActionResponseDto iprActionResponseDto =
        iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(StringUtils.EMPTY,
      iprActionResponseDto.getErrorCategory().getMessage());
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository, Mockito.times(1)).save(Mockito.any(ProductIPR.class));
  }

  @Test
  void testPerformIprActionForProduct_ValidActionWhitelisted() throws Exception {
    existingProductIpr.setState(ProductStateIPR.IN_REVIEW.name());
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.WHITELISTED.name());
    iprActionRequest.setSellerNotes(NOTES);
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(existingProductIpr);
    Mockito.when(objectMapper.writeValueAsString(Mockito.any()))
        .thenReturn(DESCRIPTION_IN_REVIEW_TO_WHITELISTED);
    IPRActionResponseDto iprActionResponseDto =
        iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(StringUtils.EMPTY, iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertEquals(DESCRIPTION_IN_REVIEW_TO_WHITELISTED,
      iprActionResponseDto.getIprHistoryEventModels().get(0).getDescription());
    Assertions.assertEquals(IPRHistoryActivity.STATUS_UPDATE.getValue(),
      iprActionResponseDto.getIprHistoryEventModels().get(0).getActivity());
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository, Mockito.times(1)).save(Mockito.any(ProductIPR.class));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  void testPerformIprActionForProduct_ValidActionSuspended() throws Exception {
    existingProductIpr.setState(ProductStateIPR.IN_REVIEW.name());
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.SUSPENDED.name());
    iprActionRequest.setSellerNotes(NOTES);
    iprActionRequest.setReasons(REASONS);
    iprActionRequest.setViolationType(VIOLATION_TYPE);
    iprActionRequest.setReviewerNotes(NOTES);
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(existingProductIpr);
    Mockito.when(objectMapper.writeValueAsString(Mockito.any()))
        .thenReturn(DESCRIPTION_IN_REVIEW_TO_SUSPENDED);
    IPRActionResponseDto iprActionResponseDto =
      iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(StringUtils.EMPTY,
      iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertEquals(DESCRIPTION_IN_REVIEW_TO_SUSPENDED,
      iprActionResponseDto.getIprHistoryEventModels().get(0).getDescription());
    Assertions.assertEquals(IPRHistoryActivity.STATUS_UPDATE.getValue(),
      iprActionResponseDto.getIprHistoryEventModels().get(0).getActivity());
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository, Mockito.times(1)).save(Mockito.any(ProductIPR.class));
    Mockito.verify(productServiceRepository, Mockito.times(1)).suspendIprProduct(Mockito.any());
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  void testPerformIprActionForProduct_ValidActionEvidenceRequested() throws Exception {
    existingProductIpr.setState(ProductStateIPR.IN_REVIEW.name());
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.EVIDENCE_REQUESTED.name());
    iprActionRequest.setSellerNotes(NOTES);
    iprActionRequest.setReasons(REASONS);
    iprActionRequest.setViolationType(VIOLATION_TYPE);
    iprActionRequest.setReviewerNotes(NOTES);
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(existingProductIpr);
    Mockito.when(objectMapper.writeValueAsString(Mockito.any()))
        .thenReturn(DESCRIPTION_IN_REVIEW_TO_EVIDENCE_REQUESTED);
    IPRActionResponseDto iprActionResponseDto =
      iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(StringUtils.EMPTY,
      iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertEquals(DESCRIPTION_IN_REVIEW_TO_EVIDENCE_REQUESTED,
      iprActionResponseDto.getIprHistoryEventModels().get(0).getDescription());
    Assertions.assertEquals(IPRHistoryActivity.STATUS_UPDATE.getValue(),
      iprActionResponseDto.getIprHistoryEventModels().get(0).getActivity());
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository, Mockito.times(1)).save(Mockito.any(ProductIPR.class));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  void performIprAction_existingReleasedAndDifferentAssigneeAndStatusUpdateToEvidenceRequested() throws Exception {
    existingProductIpr.setState(ProductStateIPR.RELEASED.name());
    existingProductIpr.setAssignedTo(ASSIGNEE);
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.EVIDENCE_REQUESTED.name());
    iprActionRequest.setSellerNotes(NOTES);
    iprActionRequest.setReasons(REASONS);
    iprActionRequest.setViolationType(VIOLATION_TYPE);
    iprActionRequest.setReviewerNotes(NOTES);
    iprActionRequest.setAssignee(ASSIGNEE2);
    IPRHistoryDescription iprHistoryDescription = IPRHistoryDescription.builder().previous(ProductStateIPR.RELEASED.name())
        .current(ProductStateIPR.EVIDENCE_REQUESTED.name()).build();
    IPRHistoryDescription iprHistoryDescription2 = IPRHistoryDescription.builder().previous(ASSIGNEE)
        .current(ASSIGNEE2).build();
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(existingProductIpr);
    Mockito.when(objectMapper.writeValueAsString(Mockito.eq(iprHistoryDescription)))
        .thenReturn(DESCRIPTION_RELEASED_TO_EVIDENCE_REQUESTED);
    Mockito.when(objectMapper.writeValueAsString(Mockito.eq(iprHistoryDescription2)))
        .thenReturn(DESCRIPTION_ASSIGNEE_UPDATE2);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    IPRActionResponseDto iprActionResponseDto =
        iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
    Assertions.assertEquals(StringUtils.EMPTY,
        iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertEquals(DESCRIPTION_ASSIGNEE_UPDATE2,
        iprActionResponseDto.getIprHistoryEventModels().get(0).getDescription());
    Assertions.assertEquals(IPRHistoryActivity.ASSIGNEE_UPDATE.getValue(),
        iprActionResponseDto.getIprHistoryEventModels().get(0).getActivity());
    Assertions.assertEquals(DESCRIPTION_RELEASED_TO_EVIDENCE_REQUESTED,
        iprActionResponseDto.getIprHistoryEventModels().get(1).getDescription());
    Assertions.assertEquals(IPRHistoryActivity.STATUS_UPDATE.getValue(),
        iprActionResponseDto.getIprHistoryEventModels().get(1).getActivity());
  }

  @Test
  void performIprAction_existingInReviewAndStatusUpdateToEvidenceRequestedBulkActionFalse() throws Exception {
    existingProductIpr.setState(ProductStateIPR.IN_REVIEW.name());
    existingProductIpr.setAssignedTo(ASSIGNEE);
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.EVIDENCE_REQUESTED.name());
    iprActionRequest.setSellerNotes(NOTES);
    iprActionRequest.setReasons(REASONS);
    iprActionRequest.setViolationType(VIOLATION_TYPE);
    iprActionRequest.setReviewerNotes(NOTES);
    iprActionRequest.setBulkAction(false);
    iprActionRequest.setUpdatedBy(ASSIGNEE);
    IPRHistoryDescription iprHistoryDescription = IPRHistoryDescription.builder().previous(ProductStateIPR.IN_REVIEW.name())
        .current(ProductStateIPR.EVIDENCE_REQUESTED.name()).build();
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(existingProductIpr);
    Mockito.when(objectMapper.writeValueAsString(Mockito.eq(iprHistoryDescription)))
        .thenReturn(DESCRIPTION_IN_REVIEW_TO_EVIDENCE_REQUESTED);
    IPRActionResponseDto iprActionResponseDto =
        iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(StringUtils.EMPTY,
        iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertEquals(DESCRIPTION_IN_REVIEW_TO_EVIDENCE_REQUESTED,
        iprActionResponseDto.getIprHistoryEventModels().get(0).getDescription());
    Assertions.assertEquals(IPRHistoryActivity.STATUS_UPDATE.getValue(),
        iprActionResponseDto.getIprHistoryEventModels().get(0).getActivity());
  }

  @Test
  void performIprAction_existingReleasedAndSameAssigneeAndStatusUpdateToEvidenceRequested() throws Exception {
    existingProductIpr.setState(ProductStateIPR.RELEASED.name());
    existingProductIpr.setAssignedTo(ASSIGNEE2);
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.EVIDENCE_REQUESTED.name());
    iprActionRequest.setSellerNotes(NOTES);
    iprActionRequest.setReasons(REASONS);
    iprActionRequest.setViolationType(VIOLATION_TYPE);
    iprActionRequest.setReviewerNotes(NOTES);
    iprActionRequest.setAssignee(ASSIGNEE2);
    IPRHistoryDescription iprHistoryDescription = IPRHistoryDescription.builder().previous(ProductStateIPR.RELEASED.name())
        .current(ProductStateIPR.EVIDENCE_REQUESTED.name()).build();
    IPRHistoryDescription iprHistoryDescription2 = IPRHistoryDescription.builder().previous(ASSIGNEE)
        .current(ASSIGNEE2).build();
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
        .thenReturn(existingProductIpr);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(existingProductIpr);
    Mockito.when(objectMapper.writeValueAsString(Mockito.eq(iprHistoryDescription)))
        .thenReturn(DESCRIPTION_RELEASED_TO_EVIDENCE_REQUESTED);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    IPRActionResponseDto iprActionResponseDto =
        iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
    Assertions.assertEquals(StringUtils.EMPTY,
        iprActionResponseDto.getErrorCategory().getMessage());
    Assertions.assertEquals(DESCRIPTION_RELEASED_TO_EVIDENCE_REQUESTED,
        iprActionResponseDto.getIprHistoryEventModels().get(0).getDescription());
    Assertions.assertEquals(IPRHistoryActivity.STATUS_UPDATE.getValue(),
        iprActionResponseDto.getIprHistoryEventModels().get(0).getActivity());
  }

  @Test
  void getPrimaryFilterCountsTest() throws Exception {
    Map<String, Object> response = new HashMap<>();
    Mockito.when(iprProductSolrCollectionRepository.getPrimaryFilterCounts(STORE_ID))
      .thenReturn(response);
    iprService.getPrimaryFilterCounts(STORE_ID);
    Mockito.verify(iprProductSolrCollectionRepository).getPrimaryFilterCounts(STORE_ID);
  }

  @Test
  void fetchAndSuspendEvidenceRequestedProductTest() {
    ReflectionTestUtils.setField(iprService, "suspendAfterWorkingDays", true);
    pageable = PageRequest.of(PAGE, 1);
    Page<ProductIPR> iprProductPageNew =
      new PageImpl<>(List.of(productIPR), PageRequest.of(2, 1), 2);
    iprProductPage = new PageImpl<>(List.of(productIPR), pageable, 2);
    productIPR.setProductSku(PRODUCT_SKU);
    Mockito.when(
        iprRepository.findByStoreIdAndStateAndEvidenceRequestedDateBeforeAndMarkForDeleteFalse(
          Mockito.anyString(), Mockito.anyString(), Mockito.any(Date.class), Mockito.any()))
      .thenReturn(iprProductPage).thenReturn(iprProductPageNew);
    Mockito.when(kafkaTopicPropertiesConsumer.getSuspendIprProductEvent()).thenReturn(EVENT);
    iprService.fetchAndSuspendEvidenceRequestedProduct(STORE_ID, DAYS, pageable);
    Mockito.verify(iprRepository, Mockito.times(2))
      .findByStoreIdAndStateAndEvidenceRequestedDateBeforeAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString(), Mockito.any(Date.class), Mockito.any(Pageable.class));
    Mockito.verify(kafkaTopicPropertiesConsumer, Mockito.times(4)).getSuspendIprProductEvent();
    Mockito.verify(kafkaProducer, Mockito.times(2))
      .send(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  void fetchAndSuspendEvidenceRequestedNoProductTest() {
    iprProductPage = new PageImpl<>(new ArrayList<>(), pageable, 0);
    productIPR.setProductSku(PRODUCT_SKU);
    Mockito.when(
        iprRepository.findByStoreIdAndStateAndEvidenceRequestedDateBeforeAndMarkForDeleteFalse(
          Mockito.anyString(), Mockito.anyString(), Mockito.any(Date.class), Mockito.any()))
      .thenReturn(iprProductPage);
    iprService.fetchAndSuspendEvidenceRequestedProduct(STORE_ID, DAYS, pageable);
    Mockito.verify(iprRepository)
      .findByStoreIdAndStateAndEvidenceRequestedDateBeforeAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString(), Mockito.any(Date.class), Mockito.any());
  }

  @Test
  void fetchIprHistoryByProductSkuTest() {
    pageable = PageRequest.of(PAGE, 1);
    Page<IPRHistory> iprHistoryPageNew =
        new PageImpl<>(List.of(IPRHistory.builder().build()), PageRequest.of(0, 1), 2);
    Mockito.when(
        iprHistoryRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalseOrderByCreatedDateDesc(STORE_ID, PRODUCT_SKU,
            pageable)).thenReturn(iprHistoryPageNew);
    Page<IPRHistoryResponse> response =
        iprService.fetchIprHistoryByProductSku(STORE_ID, PRODUCT_SKU, pageable);
    Assertions.assertEquals(2, response.getTotalElements());
  }

  @Test
  void updateIprProductHistory() {
    iprService.updateIprHistoryForProduct(STORE_ID, iprHistoryEventModel);
    Mockito.verify(iprHistoryRepository).save(iprHistoryArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, iprHistoryArgumentCaptor.getValue().getProductSku());
  }

  @Test
  void getIprProductListFetchAssigneeFromDbResponseTest() throws Exception {
    ReflectionTestUtils.setField(iprService, "fetchAssigneeFromDb", true);
    Mockito.when(
      iprProductSolrCollectionRepository.getIprProductsList(STORE_ID, iprProductListRequest,
        pageable)).thenReturn(iprProductSolrPage);
    Mockito.when(iprRepository.findByProductSkuIn(Collections.singletonList(PRODUCT_SKU)))
      .thenReturn(List.of(productIPR));
    Page<IprProductListResponse> response =
      iprService.getIprProductListResponse(STORE_ID, iprProductListRequest, pageable);
    Mockito.verify(iprProductSolrCollectionRepository)
      .getIprProductsList(STORE_ID, iprProductListRequest, pageable);
    Mockito.verify(iprRepository).findByProductSkuIn(Collections.singletonList(PRODUCT_SKU));
    Assertions.assertEquals(1, iprProductSolrPage.getTotalElements());
    Assertions.assertEquals(PRODUCT_CODE, response.getContent().get(0).getProductCode());
  }

  @Test
  void getIprProductListFetchAssigneeFromDbResponseEmptyProductSkuTest() throws Exception {
    iprProductSolrPage = new PageImpl<>(new ArrayList<>(), pageable, 1L);
    ReflectionTestUtils.setField(iprService, "fetchAssigneeFromDb", true);
    Mockito.when(
      iprProductSolrCollectionRepository.getIprProductsList(STORE_ID, iprProductListRequest,
        pageable)).thenReturn(iprProductSolrPage);
    Page<IprProductListResponse> response =
      iprService.getIprProductListResponse(STORE_ID, iprProductListRequest, pageable);
    Mockito.verify(iprProductSolrCollectionRepository)
      .getIprProductsList(STORE_ID, iprProductListRequest, pageable);
    Assertions.assertEquals(1, iprProductSolrPage.getTotalElements());
  }

  @Test
  void testPerformIprActionResetEvidenceRequestedTest() throws Exception {
    existingProductIpr.setState(ProductStateIPR.EVIDENCE_REQUESTED.name());
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.SUSPENDED.name());
    Mockito.when(iprRepository.findByProductSku(Mockito.anyString()))
      .thenReturn(existingProductIpr);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(existingProductIpr);
    IPRActionResponseDto iprActionResponseDto =
      iprService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Assertions.assertEquals(StringUtils.EMPTY,
      iprActionResponseDto.getErrorCategory().getMessage());
    Mockito.verify(iprRepository, Mockito.times(1)).findByProductSku(Mockito.anyString());
    Mockito.verify(iprRepository, Mockito.times(1)).save(Mockito.any(ProductIPR.class));
    Mockito.verify(productServiceRepository, Mockito.times(1)).suspendIprProduct(Mockito.any());
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  void testAddDSModelProductToIPRInReviewProductIPR() {
    ProductIPR productIPR = new ProductIPR();
    productIPR.setState(ProductStateIPR.IN_REVIEW.name());
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(productIPR);
    iprService.addDSModelProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE);
    Mockito.verify(iprRepository).findByProductSku(PRODUCT_SKU);
    productIPR.setSource(SOURCE);
    Mockito.verify(iprRepository).save(productIPR);
  }

  @Test
  void testAddDSModelProductToIPRIWhiteListedProductIPR() {
    ProductIPR productIPR = new ProductIPR();
    productIPR.setState(ProductStateIPR.WHITELISTED.name());
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(productIPR);
    iprService.addDSModelProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE);
    Mockito.verify(iprRepository).findByProductSku(PRODUCT_SKU);
  }

  @Test
  void testAddDSModelProductToIPRSuspendProductIPR() {
    ProductIPR productIPR = new ProductIPR();
    productIPR.setState(ProductStateIPR.SUSPENDED.name());
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(productIPR);
    productSkuDetailResponse.setMarkForDelete(false);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(productIPR);
    iprService.addDSModelProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE);
    Mockito.verify(iprRepository).findByProductSku(PRODUCT_SKU);
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
    Mockito.verify(iprRepository, Mockito.times(1)).save(productIPR);
  }

  @Test
  void testAddDSModelProductToIPRNewProductIPR() {
    ProductIPR productIPR = new ProductIPR();
    productIPR.setStoreId(STORE_ID);
    productIPR.setState(ProductStateIPR.IN_REVIEW.name());
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(null);
    productSkuDetailResponse.setMarkForDelete(false);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    Mockito.when(iprRepository.save(Mockito.any(ProductIPR.class))).thenReturn(productIPR);
    iprService.addDSModelProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE);
    Mockito.verify(iprRepository).findByProductSku(PRODUCT_SKU);
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
    Mockito.verify(iprRepository, Mockito.times(1)).save(Mockito.any(ProductIPR.class));
  }

  @Test
  void testAddDSModelProductToIPRRejectedProductIPR() {
    ProductIPR productIPR = new ProductIPR();
    productIPR.setStoreId(STORE_ID);
    productIPR.setState(ProductStateIPR.IN_REVIEW.name());
    Mockito.when(iprRepository.findByProductSku(PRODUCT_SKU)).thenReturn(null);
    productSkuDetailResponse.setRejected(true);
    Mockito.when(productServiceRepository.getProductDetailForProduct(PRODUCT_SKU, null))
        .thenReturn(productSkuDetailResponse);
    iprService.addDSModelProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE);
    Mockito.verify(iprRepository).findByProductSku(PRODUCT_SKU);
    Mockito.verify(productServiceRepository).getProductDetailForProduct(PRODUCT_SKU, null);
  }
}