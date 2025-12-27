package com.gdn.partners.pcu.internal.service.impl;

import static com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName.PRODUCT_QC_APPROVED_TASK_EVENT_NAME;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.gdn.partners.pcu.internal.client.model.response.DistributionProductResponse;
import com.gdn.partners.pcu.internal.properties.KafkaTopicProperties;
import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
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
import org.mockito.junit.MockitoJUnitRunner;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.data.domain.Page;

import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.partners.pcu.internal.client.feign.PDTFeign;
import com.gdn.partners.pcu.internal.web.model.request.RejectProductWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;
import com.gdn.x.campaign.clientsdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.request.RejectProductRequest;
import com.gdn.x.mta.distributiontask.rest.model.constant.WorkflowWebState;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.PDTProductDomainEventModelResponse;
import org.springframework.test.util.ReflectionTestUtils;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class QCTaskServiceImplTest {

  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String STATUS = "status";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String PRODUCT_CODE = "MTA-0000001";
  private static final String PRODUCT_ID = "product_id";
  private static final String REASON = "reason";
  private static final String REJECTED_TYPE = "rejectedType";
  private static final String START_DATE = "startDate";
  private static final String END_DATE = "endDate";
  private static final String BRAND = "brand";
  private static final String DEFAULT_REQUEST_ID = "REQUEST-ID";
  private static final String WORKFLOW_STATE = "PASSED";
  private static final String KEYWORD = "keyword";
  private static final int PAGE = 0;
  private static final int SIZE = 25;

  private ProductListRequest productListRequest;
  private DistributionProductResponse distributionProductResponse;
  private List<DistributionProductResponse> distributionProductResponseList = new ArrayList<>();
  private GdnRestListResponse<DistributionProductResponse> distributionProductResponseGdnRestListResponse;
  private SummaryFilterWebRequest summaryFilterWebRequest;
  private PDTProductDomainEventModelResponse pdtProductDomainEventModelResponse;
  private GdnRestSingleResponse pdtProductDomainEventModelResponseGdnRestSingleResponse;
  private RejectProductWebRequest rejectProductWebRequest;
  private GdnRestListResponse<ProductBusinessPartnerMapperResponse> productBusinessPartnerMapperResponseGdnRestListResponse;

  @Mock
  private PDTFeign pdtFeign;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private QCTaskServiceImpl qcTaskService;

  @Captor
  private ArgumentCaptor<ProductListRequest> productListRequestArgumentCaptor;

  @Captor ArgumentCaptor<RejectProductRequest> rejectProductRequestArgumentCaptor;

  @BeforeEach
  public void init() {
    productListRequest = new ProductListRequest();
    productListRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productListRequest.setProductCode(PRODUCT_CODE);
    productListRequest.setCategoryCode(CATEGORY_CODE);
    productListRequest.setStartDate(START_DATE);
    productListRequest.setEndDate(END_DATE);
    productListRequest.setProductName(PRODUCT_CODE);

    distributionProductResponseGdnRestListResponse = new GdnRestListResponse<>();
    distributionProductResponse = new DistributionProductResponse();
    distributionProductResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    distributionProductResponse.setCategoryCode(CATEGORY_CODE);
    distributionProductResponse.setProductApproved(true);
    distributionProductResponse.setProductCode(PRODUCT_CODE);
    distributionProductResponse.setBrand(BRAND);
    distributionProductResponse.setState(WorkflowWebState.PASSED);
    distributionProductResponseList.add(distributionProductResponse);
    distributionProductResponseGdnRestListResponse =
        new GdnRestListResponse<>(distributionProductResponseList, new PageMetaData(), DEFAULT_REQUEST_ID);

    summaryFilterWebRequest = SummaryFilterWebRequest.builder().build().builder().status(STATUS).keyword(PRODUCT_CODE)
        .categoryCode(CATEGORY_CODE).businessPartnerCode(BUSINESS_PARTNER_CODE).startDate(START_DATE).endDate(END_DATE)
        .build();

    pdtProductDomainEventModelResponse = new PDTProductDomainEventModelResponse();
    PDTProductDomainEventModel pdtProductDomainEventModel = new PDTProductDomainEventModel();
    pdtProductDomainEventModel.setActivated(true);
    pdtProductDomainEventModel.setBrand(BRAND);
    pdtProductDomainEventModel.setReviewPending(false);
    pdtProductDomainEventModel.setMerchantCode(BUSINESS_PARTNER_CODE);
    pdtProductDomainEventModel.setPostLive(true);
    pdtProductDomainEventModelResponse.setPdtProductDomainEventModel(pdtProductDomainEventModel);
    pdtProductDomainEventModelResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(pdtProductDomainEventModelResponse, DEFAULT_REQUEST_ID);

    rejectProductWebRequest =
        RejectProductWebRequest.builder().productId(PRODUCT_ID).rejectedReason(REASON).rejectedType(REJECTED_TYPE)
            .isAssignedToVendor(true).build();
    ProductBusinessPartnerMapperResponse productBusinessPartnerMapperResponse =
        new ProductBusinessPartnerMapperResponse();
    productBusinessPartnerMapperResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productBusinessPartnerMapperResponse.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    productBusinessPartnerMapperResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(productBusinessPartnerMapperResponse), new PageMetaData(),
            DEFAULT_REQUEST_ID);
  }


  @Test
  public void findProductCollectionSummaryByKeywordTest() throws Exception {
    Mockito.when(this.pdtFeign.filterProduct(eq(STATUS), eq(PAGE), eq(SIZE), any(ProductListRequest.class)))
        .thenReturn(distributionProductResponseGdnRestListResponse);
    Page<DistributionProductWebResponse> responsePage =
        qcTaskService.filterQCProductList(summaryFilterWebRequest, PAGE, SIZE);
    Mockito.verify(this.pdtFeign)
        .filterProduct(eq(STATUS), eq(PAGE), eq(SIZE), productListRequestArgumentCaptor.capture());
    Assertions.assertNotNull(responsePage.getContent());
    Assertions.assertEquals(PRODUCT_CODE, responsePage.getContent().get(0).getProductCode());
    Assertions.assertEquals(CATEGORY_CODE, productListRequestArgumentCaptor.getValue().getCategoryCode());
    Assertions.assertEquals(CATEGORY_CODE, responsePage.getContent().get(0).getCategoryCode());
  }

  @Test
  public void retryApproveQcTest() throws Exception {
    Mockito.when(pdtFeign.getPDTDomainModelResponseByCode(eq(PRODUCT_CODE)))
        .thenReturn(pdtProductDomainEventModelResponseGdnRestSingleResponse);
    this.qcTaskService.retryApproveQc(PRODUCT_CODE);
    Mockito.verify(pdtFeign).getPDTDomainModelResponseByCode(eq(PRODUCT_CODE));
    Mockito.verify(this.kafkaProducer, times(2)).send(eq(DomainEventName.PRODUCT_STATUS_UPDATE_TRACKER), eq(PRODUCT_CODE),
        any());
    Mockito.verify(this.kafkaProducer)
        .send(eq(PRODUCT_QC_APPROVED_TASK_EVENT_NAME), eq(PRODUCT_CODE), any());
  }

  @Test
  public void retryApproveQcFlagOnTest() throws Exception {
    ReflectionTestUtils.setField(qcTaskService, "retryFinalQCFlag", true);
    this.qcTaskService.retryApproveQc(PRODUCT_CODE);
    Mockito.verify(this.kafkaProducer)
        .send(eq(kafkaTopicProperties.getRetryFinalQcEventName()), Mockito.anyString(),
            any());
    Mockito.verify(this.kafkaTopicProperties, times(3)).getRetryFinalQcEventName();
  }

  @Test
  public void rejectProductTest() throws Exception {
    Mockito.when(this.pdtFeign.rejectQCProduct(any(RejectProductRequest.class)))
        .thenReturn(new GdnBaseRestResponse(DEFAULT_REQUEST_ID));
    this.qcTaskService.rejectProduct(rejectProductWebRequest);
    Mockito.verify(this.pdtFeign).rejectQCProduct(rejectProductRequestArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_ID, rejectProductRequestArgumentCaptor.getValue().getProductId());
    Assertions.assertEquals(REASON, rejectProductRequestArgumentCaptor.getValue().getRejectedReason());
    Assertions.assertEquals(REJECTED_TYPE, rejectProductRequestArgumentCaptor.getValue().getRejectedType());
  }

  @Test
  public void approveProductTest() throws Exception {
    Mockito.when(pdtFeign.approveQCProduct(eq(PRODUCT_ID))).thenReturn(new GdnBaseRestResponse(DEFAULT_REQUEST_ID));
    this.qcTaskService.approveProduct(PRODUCT_CODE, PRODUCT_ID);
    Mockito.verify(pdtFeign).approveQCProduct(eq(PRODUCT_ID));
    Mockito.verify(this.kafkaProducer)
        .send(eq(DomainEventName.PRODUCT_STATUS_UPDATE_TRACKER), eq(PRODUCT_CODE), any());
  }

  @Test
  public void getBusinessPartnerListWithKeywordTest() throws Exception {
    Mockito.when(
        this.pdtFeign.filterProductBusinessPartnerMapperByWorkFlowState(PAGE, SIZE, true, KEYWORD, WORKFLOW_STATE))
        .thenReturn(productBusinessPartnerMapperResponseGdnRestListResponse);
    Page<BusinessPartnerWebResponse> responsePage =
        qcTaskService.getBusinessPartnerList(WORKFLOW_STATE, KEYWORD, PAGE, SIZE);
    Mockito.verify(this.pdtFeign)
        .filterProductBusinessPartnerMapperByWorkFlowState(PAGE, SIZE, true, KEYWORD, WORKFLOW_STATE);
    Assertions.assertNotNull(responsePage.getContent());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, responsePage.getContent().get(0).getBusinessPartnerCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, responsePage.getContent().get(0).getBusinessPartnerName());
  }

  @Test
  public void getBusinessPartnerListWithoutKeywordTest() throws Exception {
    Mockito.when(this.pdtFeign
        .filterProductBusinessPartnerMapperByWorkFlowState(PAGE, SIZE, false, StringUtils.EMPTY, WORKFLOW_STATE))
        .thenReturn(productBusinessPartnerMapperResponseGdnRestListResponse);
    Page<BusinessPartnerWebResponse> responsePage =
        qcTaskService.getBusinessPartnerList(WORKFLOW_STATE, StringUtils.EMPTY, PAGE, SIZE);
    Mockito.verify(this.pdtFeign)
        .filterProductBusinessPartnerMapperByWorkFlowState(PAGE, SIZE, false, StringUtils.EMPTY, WORKFLOW_STATE);
    Assertions.assertNotNull(responsePage.getContent());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, responsePage.getContent().get(0).getBusinessPartnerCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, responsePage.getContent().get(0).getBusinessPartnerName());
  }

  @AfterEach
  public void tearDown() throws IOException {
    verifyNoMoreInteractions(pdtFeign);
    verifyNoMoreInteractions(kafkaProducer);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }
}
