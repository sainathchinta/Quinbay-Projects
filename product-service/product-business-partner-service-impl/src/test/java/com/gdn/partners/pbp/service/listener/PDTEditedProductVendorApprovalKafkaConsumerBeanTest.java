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
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.partners.pbp.distributiontask.ProductDistributionTaskQCService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTEditedProductVendorApprovedEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;

public class PDTEditedProductVendorApprovalKafkaConsumerBeanTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String USERNAME = "username";
  public static final String IMAGE = "IMAGE";
  private ObjectMapper mapper;

  @InjectMocks
  private PDTEditedProductVendorApprovalKafkaConsumerBean pdtEditedProductVendorApprovalKafkaConsumerBean;

  @Mock
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductDistributionTaskQCService productDistributionTaskQCService;

  @Mock
  private ProductService productService;

  private PDTProductDomainEventModel pdtProductDomainEventModel;
  private PDTEditedProductVendorApprovedEventModel pdtProductVendorApprovedEventModel;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    pdtProductDomainEventModel = new PDTProductDomainEventModel();
    pdtProductDomainEventModel.setProductCode(PRODUCT_CODE);
    pdtProductVendorApprovedEventModel = new PDTEditedProductVendorApprovedEventModel();
    pdtProductVendorApprovedEventModel.setProductCode(PRODUCT_CODE);
    pdtProductVendorApprovedEventModel.setApprovalType(IMAGE);
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productDistributionTaskQCService);
    Mockito.verifyNoMoreInteractions(productDistributionTaskRepository, objectMapper, productService);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    pdtProductVendorApprovedEventModel.setUpdatedBy(IMAGE);
    Mockito.when(productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(pdtProductDomainEventModel);
    String message = mapper.writeValueAsString(pdtProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTEditedProductVendorApprovedEventModel.class))
        .thenReturn(pdtProductVendorApprovedEventModel);
    this.pdtEditedProductVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTEditedProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskQCService)
        .processVendorApprovalEventForEditedProducts(pdtProductDomainEventModel, IMAGE);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
  }

  @Test
  public void onDomainEventItemsEmptyConsumed() throws Exception {
    pdtProductVendorApprovedEventModel.setUpdatedBy(IMAGE);
    Mockito.when(productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(pdtProductDomainEventModel);
    pdtProductDomainEventModel.setProductItems(null);
    String message = mapper.writeValueAsString(pdtProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTEditedProductVendorApprovedEventModel.class))
        .thenReturn(pdtProductVendorApprovedEventModel);
    this.pdtEditedProductVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTEditedProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskQCService)
        .processVendorApprovalEventForEditedProducts(pdtProductDomainEventModel, IMAGE);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
  }

  @Test
  public void onDomainEventConsumedEmptyUpdatedByTest() throws Exception {
    pdtProductVendorApprovedEventModel.setUpdatedBy(StringUtils.EMPTY);
    Mockito.when(productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(pdtProductDomainEventModel);
    String message = mapper.writeValueAsString(pdtProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTEditedProductVendorApprovedEventModel.class))
        .thenReturn(pdtProductVendorApprovedEventModel);
    this.pdtEditedProductVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTEditedProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskQCService)
        .processVendorApprovalEventForEditedProducts(pdtProductDomainEventModel, IMAGE);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    pdtProductVendorApprovedEventModel.setUpdatedBy(IMAGE);
    Mockito.when(productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(pdtProductDomainEventModel);
    String message = mapper.writeValueAsString(pdtProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTEditedProductVendorApprovedEventModel.class))
        .thenReturn(pdtProductVendorApprovedEventModel);
    Mockito.doThrow(Exception.class).when(productDistributionTaskQCService)
        .processVendorApprovalEventForEditedProducts(pdtProductDomainEventModel, IMAGE);
    this.pdtEditedProductVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTEditedProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskQCService)
        .processVendorApprovalEventForEditedProducts(pdtProductDomainEventModel, IMAGE);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(productService).setReviewPendingFlagToTrue(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void listenWithNullObject() throws Exception {
    String message = mapper.writeValueAsString(null);
    Mockito.when(objectMapper.readValue(message, PDTEditedProductVendorApprovedEventModel.class))
        .thenReturn(null);
    this.pdtEditedProductVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTEditedProductVendorApprovedEventModel.class);
  }
}