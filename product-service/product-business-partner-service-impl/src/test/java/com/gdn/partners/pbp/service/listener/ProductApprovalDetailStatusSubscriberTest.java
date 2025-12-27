package com.gdn.partners.pbp.service.listener;

import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.config.ProductApprovalDetailStatus;
import com.gdn.mta.domain.event.modal.ProductApprovalDetailStatusEvent;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;

public class ProductApprovalDetailStatusSubscriberTest {

  private static final String HASH_CODE = "hashCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final String IMAGE_LOCATION = "imageLocation";
  private static final String STORE_ID = "10001";
  private static final String CLIENT_ID = "clientId";
  private static final String USER_NAME = "userName";
  private static final String REQUEST_ID = "requserId";
  private static final Long TIMESTAMP = System.currentTimeMillis();

  private ProductApprovalDetailStatusEvent productApprovalDetailStatusEvent;
  private ProductResponse productResponse;
  private ActivateImageResponse activateImageResponse;
  private ObjectMapper mapper;

  @InjectMocks
  private ProductApprovalDetailStatusSubscriber productApprovalDetailStatusSubscriber;

  @Mock
  private ProductLevel1CollectionService productLevel1CollectionService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    ProductApprovalDetailStatus productApprovalDetailStatus =
        ProductApprovalDetailStatus.PBP_UpdateProductContent_Response_Received_PCB;
    productApprovalDetailStatusEvent =
        new ProductApprovalDetailStatusEvent(PRODUCT_CODE, productApprovalDetailStatus, TIMESTAMP);
    Mockito.doNothing().when(productLevel1CollectionService)
        .updateProductStatus(Mockito.eq(STORE_ID), Mockito.any(ProductApprovalDetailStatusEvent.class));
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productLevel1CollectionService, objectMapper);
  }

  @Test
  public void listenWithNullObject() throws Exception {
    String message = mapper.writeValueAsString(null);
    Mockito.when(objectMapper.readValue(message, ProductApprovalDetailStatusEvent.class))
        .thenReturn(null);
    this.productApprovalDetailStatusSubscriber.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ProductApprovalDetailStatusEvent.class);
  }

  @Test
  public void listenWithMessage() throws Exception {
    String message = mapper.writeValueAsString(productApprovalDetailStatusEvent);
    Mockito.when(objectMapper.readValue(message, ProductApprovalDetailStatusEvent.class))
        .thenReturn(productApprovalDetailStatusEvent);
    this.productApprovalDetailStatusSubscriber.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ProductApprovalDetailStatusEvent.class);
    Mockito.verify(productLevel1CollectionService).updateProductStatus(STORE_ID, productApprovalDetailStatusEvent);
  }

  @Test
  public void listenWithException() throws Exception {
    Mockito.doThrow(new RuntimeException()).when(productLevel1CollectionService)
        .updateProductStatus(Mockito.eq(STORE_ID), Mockito.any(ProductApprovalDetailStatusEvent.class));
    String message = mapper.writeValueAsString(productApprovalDetailStatusEvent);
    Mockito.when(objectMapper.readValue(message, ProductApprovalDetailStatusEvent.class))
        .thenReturn(productApprovalDetailStatusEvent);
    this.productApprovalDetailStatusSubscriber.onDomainEventConsumed(message);
    Mockito.verify(productLevel1CollectionService).updateProductStatus(STORE_ID, productApprovalDetailStatusEvent);
    verify(objectMapper).readValue(message, ProductApprovalDetailStatusEvent.class);
  }
}
