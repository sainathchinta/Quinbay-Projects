package com.gdn.partners.pbp.service.listener;

import static org.mockito.Mockito.verify;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.partners.pbp.distributiontask.ProductDistributionTaskQCService;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTEditedProductVendorApprovedEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTRevisedProductVendorApprovedEventModel;

public class PDTRevisedProductVendorApprovalKafkaConsumerBeanTest {

  private static final String PRODUCT_CODE = "productCode";
  public static final String IMAGE = "IMAGE";
  private ObjectMapper mapper;

  @InjectMocks
  private PDTRevisedProductVendorApprovalKafkaConsumerBean pdtRevisedProductVendorApprovalKafkaConsumerBean;

  @Mock
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductDistributionTaskQCService productDistributionTaskQCService;

  @Mock
  private ProductService productService;

  private PDTProductDomainEventModel pdtProductDomainEventModel;
  private PDTRevisedProductVendorApprovedEventModel pdtRevisedProductVendorApprovedEventModel;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    pdtProductDomainEventModel = new PDTProductDomainEventModel();
    pdtProductDomainEventModel.setProductCode(PRODUCT_CODE);
    pdtRevisedProductVendorApprovedEventModel = new PDTRevisedProductVendorApprovedEventModel();
    pdtRevisedProductVendorApprovedEventModel.setProductCode(PRODUCT_CODE);
    pdtRevisedProductVendorApprovedEventModel.setApprovalType(IMAGE);
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productDistributionTaskQCService, productService);
    Mockito.verifyNoMoreInteractions(productDistributionTaskRepository, objectMapper);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    pdtRevisedProductVendorApprovedEventModel.setPostLive(true);
    pdtRevisedProductVendorApprovedEventModel.setReviewPending(true);
    pdtRevisedProductVendorApprovedEventModel.setUpdatedBy(IMAGE);
    Mockito.when(productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(pdtProductDomainEventModel);
    String message = mapper.writeValueAsString(pdtRevisedProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTRevisedProductVendorApprovedEventModel.class))
        .thenReturn(pdtRevisedProductVendorApprovedEventModel);
    this.pdtRevisedProductVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTRevisedProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskQCService)
        .processVendorApprovalEventForRevisedProducts(pdtProductDomainEventModel, pdtRevisedProductVendorApprovedEventModel);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(productService).publishProductStatusEventByProductCode(pdtRevisedProductVendorApprovedEventModel
            .getProductCode(), ProductStatus.ACTIVE, StringUtils.EMPTY);
  }

  @Test
  public void onDomainEventConsumedPreLiveTest() throws Exception {
    pdtRevisedProductVendorApprovedEventModel.setPostLive(false);
    pdtRevisedProductVendorApprovedEventModel.setReviewPending(false);
    pdtRevisedProductVendorApprovedEventModel.setUpdatedBy(IMAGE);
    Mockito.when(productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(pdtProductDomainEventModel);
    String message = mapper.writeValueAsString(pdtRevisedProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTRevisedProductVendorApprovedEventModel.class))
        .thenReturn(pdtRevisedProductVendorApprovedEventModel);
    this.pdtRevisedProductVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTRevisedProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskQCService)
        .processVendorApprovalEventForRevisedProducts(pdtProductDomainEventModel, pdtRevisedProductVendorApprovedEventModel);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(productService).publishProductStatusEventByProductCode(pdtRevisedProductVendorApprovedEventModel
        .getProductCode(), ProductStatus.ACTIVE, StringUtils.EMPTY);
  }

  @Test
  public void onDomainEventItemsEmptyConsumed() throws Exception {
    pdtRevisedProductVendorApprovedEventModel.setPostLive(false);
    pdtRevisedProductVendorApprovedEventModel.setReviewPending(true);
    pdtRevisedProductVendorApprovedEventModel.setUpdatedBy(IMAGE);
    Mockito.when(productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(pdtProductDomainEventModel);
    pdtProductDomainEventModel.setProductItems(null);
    String message = mapper.writeValueAsString(pdtRevisedProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTRevisedProductVendorApprovedEventModel.class))
        .thenReturn(pdtRevisedProductVendorApprovedEventModel);
    this.pdtRevisedProductVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTRevisedProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskQCService)
        .processVendorApprovalEventForRevisedProducts(pdtProductDomainEventModel, pdtRevisedProductVendorApprovedEventModel);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
  }

  @Test
  public void onDomainEventConsumedEmptyUpdatedByTest() throws Exception {
    pdtRevisedProductVendorApprovedEventModel.setPostLive(true);
    pdtRevisedProductVendorApprovedEventModel.setReviewPending(false);
    pdtRevisedProductVendorApprovedEventModel.setUpdatedBy(StringUtils.EMPTY);
    Mockito.when(productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(pdtProductDomainEventModel);
    String message = mapper.writeValueAsString(pdtRevisedProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTRevisedProductVendorApprovedEventModel.class))
        .thenReturn(pdtRevisedProductVendorApprovedEventModel);
    this.pdtRevisedProductVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTRevisedProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskQCService)
        .processVendorApprovalEventForRevisedProducts(pdtProductDomainEventModel, pdtRevisedProductVendorApprovedEventModel);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    pdtRevisedProductVendorApprovedEventModel.setUpdatedBy(IMAGE);
    Mockito.when(productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(pdtProductDomainEventModel);
    String message = mapper.writeValueAsString(pdtRevisedProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTRevisedProductVendorApprovedEventModel.class))
        .thenReturn(pdtRevisedProductVendorApprovedEventModel);
    Mockito.doThrow(Exception.class).when(productDistributionTaskQCService)
        .processVendorApprovalEventForRevisedProducts(pdtProductDomainEventModel, pdtRevisedProductVendorApprovedEventModel);
    this.pdtRevisedProductVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTRevisedProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskQCService)
        .processVendorApprovalEventForRevisedProducts(pdtProductDomainEventModel, pdtRevisedProductVendorApprovedEventModel);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
  }

  @Test
  public void listenWithNullObject() throws Exception {
    String message = mapper.writeValueAsString(null);
    Mockito.when(objectMapper.readValue(message, PDTEditedProductVendorApprovedEventModel.class)).thenReturn(null);
    this.pdtRevisedProductVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTRevisedProductVendorApprovedEventModel.class);
  }
}