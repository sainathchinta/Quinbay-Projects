package com.gdn.x.mta.distributiontask.inbound.impl;

import java.io.IOException;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.domain.event.InternalHistoryEventModel;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.rest.model.request.PublishAndSavedProductAndHistoryModel;
import com.gdn.x.mta.distributiontask.service.api.ProductService;

public class PBPAutoApprovalCheckEventListenerTest {

  private static final String PRODUCT_CODE = "productCode";
  private AutoApprovalTypeRequest autoApprovalTypeRequest;
  private AutoApprovalTypeRequest autoApprovalTypeRequest1;
  private AutoApprovalTypeResponse autoApprovalTypeResponse;
  private ObjectMapper mapper;

  @InjectMocks
  private PBPAutoApprovalCheckEventListener pbpAutoApprovalCheckEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductServiceRepository productServiceRepository;

  @Mock
  private ProductService productService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    mapper = new ObjectMapper();
    autoApprovalTypeRequest = new AutoApprovalTypeRequest();
    autoApprovalTypeRequest.setProductCode(PRODUCT_CODE);
    autoApprovalTypeRequest.setReviewType(Constants.CONTENT_AND_IMAGE);
    autoApprovalTypeRequest.setRevised(true);

    autoApprovalTypeRequest1 =
        new AutoApprovalTypeRequest(autoApprovalTypeRequest.getCategoryCode(), autoApprovalTypeRequest.isEdited(),
            autoApprovalTypeRequest.getReviewType(), autoApprovalTypeRequest.isRevised());

    autoApprovalTypeResponse = new AutoApprovalTypeResponse();
    autoApprovalTypeResponse.setAutoApprovalType(Constants.CONTENT_AND_IMAGE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(productServiceRepository);
    Mockito.verifyNoMoreInteractions(productService);
  }

  @Test
   void onDomainEventConsumedSuccessFalse() throws IOException {
    Mockito
        .when(objectMapper.readValue(mapper.writeValueAsString(autoApprovalTypeRequest), AutoApprovalTypeRequest.class))
        .thenReturn(autoApprovalTypeRequest);
    Mockito.when(productServiceRepository.getAutoApprovalType(autoApprovalTypeRequest.getStoreId(),
            autoApprovalTypeRequest.getProductCode(), false, autoApprovalTypeRequest1))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    pbpAutoApprovalCheckEventListener.onDomainEventConsumed(mapper.writeValueAsString(autoApprovalTypeRequest));
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(autoApprovalTypeRequest), AutoApprovalTypeRequest.class);
    Mockito.verify(productServiceRepository)
        .getAutoApprovalType(autoApprovalTypeRequest.getStoreId(), autoApprovalTypeRequest.getProductCode(), false,
            autoApprovalTypeRequest1);
  }

  @Test
   void onDomainEventConsumedValueNull() throws IOException {
    Mockito
        .when(objectMapper.readValue(mapper.writeValueAsString(autoApprovalTypeRequest), AutoApprovalTypeRequest.class))
        .thenReturn(autoApprovalTypeRequest);
    Mockito.when(productServiceRepository.getAutoApprovalType(autoApprovalTypeRequest.getStoreId(),
            autoApprovalTypeRequest.getProductCode(), false, autoApprovalTypeRequest1))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, null));
    pbpAutoApprovalCheckEventListener.onDomainEventConsumed(mapper.writeValueAsString(autoApprovalTypeRequest));
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(autoApprovalTypeRequest), AutoApprovalTypeRequest.class);
    Mockito.verify(productServiceRepository)
        .getAutoApprovalType(autoApprovalTypeRequest.getStoreId(), autoApprovalTypeRequest.getProductCode(), false,
            autoApprovalTypeRequest1);
  }

  @Test
   void onDomainEventConsumedValueNotValid() throws IOException {
    autoApprovalTypeResponse.setAutoApprovalType(PRODUCT_CODE);
    Mockito
        .when(objectMapper.readValue(mapper.writeValueAsString(autoApprovalTypeRequest), AutoApprovalTypeRequest.class))
        .thenReturn(autoApprovalTypeRequest);
    Mockito.when(productServiceRepository.getAutoApprovalType(autoApprovalTypeRequest.getStoreId(),
            autoApprovalTypeRequest.getProductCode(), false, autoApprovalTypeRequest1))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, autoApprovalTypeResponse, null));
    pbpAutoApprovalCheckEventListener.onDomainEventConsumed(mapper.writeValueAsString(autoApprovalTypeRequest));
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(autoApprovalTypeRequest), AutoApprovalTypeRequest.class);
    Mockito.verify(productServiceRepository)
        .getAutoApprovalType(autoApprovalTypeRequest.getStoreId(), autoApprovalTypeRequest.getProductCode(), false,
            autoApprovalTypeRequest1);
  }

  @Test
   void onDomainEventConsumed() throws Exception {
    Mockito
        .when(objectMapper.readValue(mapper.writeValueAsString(autoApprovalTypeRequest), AutoApprovalTypeRequest.class))
        .thenReturn(autoApprovalTypeRequest);
    Mockito.when(productServiceRepository.getAutoApprovalType(autoApprovalTypeRequest.getStoreId(),
            autoApprovalTypeRequest.getProductCode(), false, autoApprovalTypeRequest1))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, autoApprovalTypeResponse, null));
    InternalHistoryEventModel internalHistoryEventModel = new InternalHistoryEventModel();
    Mockito.when(productService.autoApproveProduct(PRODUCT_CODE)).thenReturn(
        PublishAndSavedProductAndHistoryModel.builder().internalHistoryEventModel(internalHistoryEventModel).build());
    pbpAutoApprovalCheckEventListener.onDomainEventConsumed(mapper.writeValueAsString(autoApprovalTypeRequest));
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(autoApprovalTypeRequest), AutoApprovalTypeRequest.class);
    Mockito.verify(productServiceRepository)
        .getAutoApprovalType(autoApprovalTypeRequest.getStoreId(), autoApprovalTypeRequest.getProductCode(), false,
            autoApprovalTypeRequest1);
    Mockito.verify(productService).autoApproveProduct(PRODUCT_CODE);
    Mockito.verify(productService).publishInternalHistoryEventForProduct(internalHistoryEventModel);
  }

  @Test
   void onDomainEventConsumedNullHistory() throws Exception {
    Mockito.when(
            objectMapper.readValue(mapper.writeValueAsString(autoApprovalTypeRequest), AutoApprovalTypeRequest.class))
        .thenReturn(autoApprovalTypeRequest);
    Mockito.when(productServiceRepository.getAutoApprovalType(autoApprovalTypeRequest.getStoreId(),
            autoApprovalTypeRequest.getProductCode(), false, autoApprovalTypeRequest1))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, autoApprovalTypeResponse, null));
    Mockito.when(productService.autoApproveProduct(PRODUCT_CODE))
        .thenReturn(PublishAndSavedProductAndHistoryModel.builder().build());
    pbpAutoApprovalCheckEventListener.onDomainEventConsumed(mapper.writeValueAsString(autoApprovalTypeRequest));
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(autoApprovalTypeRequest), AutoApprovalTypeRequest.class);
    Mockito.verify(productServiceRepository)
        .getAutoApprovalType(autoApprovalTypeRequest.getStoreId(), autoApprovalTypeRequest.getProductCode(), false,
            autoApprovalTypeRequest1);
    Mockito.verify(productService).autoApproveProduct(PRODUCT_CODE);
  }

  @Test
   void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito
        .when(objectMapper.readValue(mapper.writeValueAsString(autoApprovalTypeRequest), AutoApprovalTypeRequest.class))
        .thenReturn(autoApprovalTypeRequest);
    Mockito.when(productServiceRepository.getAutoApprovalType(autoApprovalTypeRequest.getStoreId(),
            autoApprovalTypeRequest.getProductCode(), false, autoApprovalTypeRequest1))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, autoApprovalTypeResponse, null));
    Mockito.doThrow(Exception.class).when(productService).autoApproveProduct(PRODUCT_CODE);
    pbpAutoApprovalCheckEventListener.onDomainEventConsumed(mapper.writeValueAsString(autoApprovalTypeRequest));
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(autoApprovalTypeRequest), AutoApprovalTypeRequest.class);
    Mockito.verify(productServiceRepository)
        .getAutoApprovalType(autoApprovalTypeRequest.getStoreId(), autoApprovalTypeRequest.getProductCode(), false,
            autoApprovalTypeRequest1);
    Mockito.verify(productService).autoApproveProduct(PRODUCT_CODE);
  }
}
