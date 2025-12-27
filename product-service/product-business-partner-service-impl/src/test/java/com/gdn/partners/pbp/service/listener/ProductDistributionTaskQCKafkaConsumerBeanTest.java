package com.gdn.partners.pbp.service.listener;

import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.ProcessImageDomainEvent;
import com.gdn.mta.domain.event.modal.XGPImageInfoDomainEvent;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.service.partners.pbp.distributiontask.ProductDistributionTaskQCService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;

public class ProductDistributionTaskQCKafkaConsumerBeanTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String STORE_ID = "10001";
  private static final int MAX_RETRIES = 3;
  private static final int RETRY = 1;

  @InjectMocks
  private ProductDistributionTaskQCKafkaConsumerBean productDistributionTaskQCKafkaConsumerBean;

  private XGPImageInfoDomainEvent xgpImageInfoDomainEvent;
  private ProductResponse productResponse;
  private ActivateImageResponse activateImageResponse;

  @Mock
  private ProductService productService;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private ProductWfService productWfService;

  @Mock
  private ProductStatusPublisherService productStatusPublisherService;

  @Mock
  private ProductDistributionTaskQCService productDistributionTaskQCService;

  @Mock
  private ProductLevel1CollectionService productLevel1CollectionService;

  @Mock
  private ObjectMapper objectMapper;

  private PDTProductDomainEventModel pdtProductDomainEventModel;

  private ProductCollection productCollection;
  private ObjectMapper mapper;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    pdtProductDomainEventModel = new PDTProductDomainEventModel();
    pdtProductDomainEventModel.setProductCode(PRODUCT_CODE);
    activateImageResponse = new ActivateImageResponse();
    productCollection = new ProductCollection();
    ReflectionTestUtils.setField(productDistributionTaskQCKafkaConsumerBean, "maxRetryCount", MAX_RETRIES);
    Mockito.when(productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.doNothing().when(this.productDistributionTaskQCService)
        .processProductDistributionTaskQCKafkaConsumer(Mockito.any(), Mockito.eq(0));
    Mockito.when(productService.publishProcessImageRequest(Mockito.any(ProductRequest.class), Mockito.anyString()))
        .thenReturn(new ProcessImageDomainEvent());
    mapper = new ObjectMapper();
  }

  @Test
  public void listenWithNullObject() throws Exception {
    String message = mapper.writeValueAsString(null);
    Mockito.when(objectMapper.readValue(message, PDTProductDomainEventModel.class))
        .thenReturn(null);
    this.productDistributionTaskQCKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTProductDomainEventModel.class);
  }

  @Test
  public void listen() throws Exception {
    String message = mapper.writeValueAsString(pdtProductDomainEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductDomainEventModel.class))
        .thenReturn(pdtProductDomainEventModel);
    this.productDistributionTaskQCKafkaConsumerBean.onDomainEventConsumed(message);
    Mockito.verify(productDistributionTaskQCService).processProductDistributionTaskQCKafkaConsumer(pdtProductDomainEventModel, 0);
    verify(objectMapper).readValue(message, PDTProductDomainEventModel.class);
  }

  @Test
  public void listenWithExceptionWithoutRetry() throws Exception {
    productCollection.setApproveRetryCount(MAX_RETRIES);
    Mockito.doThrow(new RuntimeException()).when(productDistributionTaskQCService)
        .processProductDistributionTaskQCKafkaConsumer(pdtProductDomainEventModel, 0);
    String message = mapper.writeValueAsString(pdtProductDomainEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductDomainEventModel.class))
        .thenReturn(pdtProductDomainEventModel);
    this.productDistributionTaskQCKafkaConsumerBean.onDomainEventConsumed(message);
    Mockito.verify(productDistributionTaskQCService)
        .processProductDistributionTaskQCKafkaConsumer(pdtProductDomainEventModel, 0);
    verify(objectMapper).readValue(message, PDTProductDomainEventModel.class);

  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(productDistributionTaskQCService);
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(productLevel1CollectionService, objectMapper);
  }

}
