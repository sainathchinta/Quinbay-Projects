package com.gdn.x.mta.distributiontask.inbound.impl;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashMap;

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
import com.gdn.x.mta.distributiontask.domain.event.model.ProductAutoApprovalEventModel;
import com.gdn.x.mta.distributiontask.model.enums.AutoApprovalStatus;
import com.gdn.x.mta.distributiontask.service.api.ProductAutoApprovalService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;


public class AutoApprovalCriteraCheckEventListenerTest {

  @InjectMocks
  private AutoApprovalCriteraCheckEventListener autoApprovalCriteraCheckEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductWrapperService productWrapperService;

  @Mock
  private ProductServiceRepository productServiceRepository;

  @Mock
  private ProductAutoApprovalService productAutoApprovalService;

  private static final String PRODUCT_CODE = "productCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String REVIEW_TYPE = "reviewType";
  private static final String STORE_ID = "10001";
  private static final boolean IS_EDITED = true;
  private static String CONTENT_AND_IMAGE = "CONTENT_AND_IMAGE";
  private static String NA = "NA";
  private static String REQUEST_ID = "request_id";
  private static String ACTIVE = "ACTIVE";
  private static String IN_PROGRESS = "IN_PROGRESS";


  private ProductAutoApprovalEventModel productAutoApprovalEvent;
  private AutoApprovalTypeResponse autoApprovalType;
  private AutoApprovalTypeRequest autoApprovalTypeRequest;
  private ObjectMapper mapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    mapper = new ObjectMapper();
    productAutoApprovalEvent = new ProductAutoApprovalEventModel();
    productAutoApprovalEvent.setCategoryCode(CATEGORY_CODE);
    productAutoApprovalEvent.setEdited(IS_EDITED);
    productAutoApprovalEvent.setProductCode(PRODUCT_CODE);
    productAutoApprovalEvent.setStoreId(STORE_ID);
    productAutoApprovalEvent.setReviewType(REVIEW_TYPE);
    autoApprovalTypeRequest =
        new AutoApprovalTypeRequest(productAutoApprovalEvent.getCategoryCode(),
            productAutoApprovalEvent.getEdited(),
            productAutoApprovalEvent.getReviewType(), false);
    autoApprovalType = new AutoApprovalTypeResponse(CONTENT_AND_IMAGE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(productWrapperService);
    Mockito.verifyNoMoreInteractions(productServiceRepository);
    Mockito.verifyNoMoreInteractions(productAutoApprovalService);
  }

  @Test
   void onDomainEventConsumedEligibleTest() throws Exception {
    String message = mapper.writeValueAsString(productAutoApprovalEvent);
    when(objectMapper.readValue(message, ProductAutoApprovalEventModel.class)).thenReturn(productAutoApprovalEvent);
    when(productServiceRepository.getAutoApprovalType(productAutoApprovalEvent.getStoreId(),
        productAutoApprovalEvent.getProductCode(), false, autoApprovalTypeRequest)).thenReturn(
        new GdnRestSingleResponse(autoApprovalType, REQUEST_ID));
    when(productServiceRepository.getProductStatus(productAutoApprovalEvent.getStoreId(),
        productAutoApprovalEvent.getProductCode())).thenReturn(ACTIVE);
    autoApprovalCriteraCheckEventListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, ProductAutoApprovalEventModel.class);
    verify(productServiceRepository).getAutoApprovalType(STORE_ID, PRODUCT_CODE, false, autoApprovalTypeRequest);
    verify(productServiceRepository).getProductStatus(STORE_ID, PRODUCT_CODE);
    verify(productWrapperService).autoApproveOfPendingProductsAfterEligibilityCheck(STORE_ID, PRODUCT_CODE, Boolean.TRUE,
        autoApprovalType);
  }

  @Test
   void onDomainEventConsumedNoEligibleTest() throws Exception {
    this.autoApprovalType.setAutoApprovalType(NA);
    String message = mapper.writeValueAsString(productAutoApprovalEvent);
    when(objectMapper.readValue(message, ProductAutoApprovalEventModel.class)).thenReturn(productAutoApprovalEvent);
    when(productServiceRepository.getAutoApprovalType(productAutoApprovalEvent.getStoreId(),
        productAutoApprovalEvent.getProductCode(), false, autoApprovalTypeRequest)).thenReturn(
        new GdnRestSingleResponse(autoApprovalType, REQUEST_ID));
    when(productServiceRepository.getProductStatus(productAutoApprovalEvent.getStoreId(),
        productAutoApprovalEvent.getProductCode())).thenReturn(ACTIVE);
    autoApprovalCriteraCheckEventListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, ProductAutoApprovalEventModel.class);
    verify(productServiceRepository).getAutoApprovalType(STORE_ID, PRODUCT_CODE, false, autoApprovalTypeRequest);
    verify(productServiceRepository).getProductStatus(STORE_ID, PRODUCT_CODE);
    verify(productWrapperService).autoApproveOfPendingProductsAfterEligibilityCheck(STORE_ID, PRODUCT_CODE, Boolean.FALSE,
        autoApprovalType);
  }

  @Test
   void onDomainEventConsumedPPBEmptyResponse() throws Exception {
    String message = mapper.writeValueAsString(productAutoApprovalEvent);
    when(objectMapper.readValue(message, ProductAutoApprovalEventModel.class)).thenReturn(productAutoApprovalEvent);
    when(productServiceRepository.getAutoApprovalType(productAutoApprovalEvent.getStoreId(),
        productAutoApprovalEvent.getProductCode(), false, autoApprovalTypeRequest)).thenReturn(
        new GdnRestSingleResponse(null, REQUEST_ID));
    when(productServiceRepository.getProductStatus(productAutoApprovalEvent.getStoreId(),
        productAutoApprovalEvent.getProductCode())).thenReturn(ACTIVE);
    autoApprovalCriteraCheckEventListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, ProductAutoApprovalEventModel.class);
    verify(productServiceRepository).getAutoApprovalType(STORE_ID, PRODUCT_CODE, false, autoApprovalTypeRequest);
    verify(productServiceRepository).getProductStatus(STORE_ID, PRODUCT_CODE);
    verify(productAutoApprovalService).updateProductAutoApprovalDetailsByProductCode(STORE_ID, PRODUCT_CODE, AutoApprovalStatus.FAILED,
        true);
  }

  @Test
   void onDomainEventConsumedPBPSuccessFalseResponse() throws Exception {
    String message = mapper.writeValueAsString(productAutoApprovalEvent);
    when(objectMapper.readValue(message, ProductAutoApprovalEventModel.class)).thenReturn(productAutoApprovalEvent);
    when(productServiceRepository.getAutoApprovalType(productAutoApprovalEvent.getStoreId(),
        productAutoApprovalEvent.getProductCode(), false, autoApprovalTypeRequest)).thenReturn(
        new GdnRestSingleResponse(null,
        null, false, autoApprovalType, REQUEST_ID));
    when(productServiceRepository.getProductStatus(productAutoApprovalEvent.getStoreId(),
        productAutoApprovalEvent.getProductCode())).thenReturn(ACTIVE);
    autoApprovalCriteraCheckEventListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, ProductAutoApprovalEventModel.class);
    verify(productServiceRepository).getAutoApprovalType(STORE_ID, PRODUCT_CODE, false, autoApprovalTypeRequest);
    verify(productServiceRepository).getProductStatus(STORE_ID, PRODUCT_CODE);
    verify(productAutoApprovalService).updateProductAutoApprovalDetailsByProductCode(STORE_ID, PRODUCT_CODE, AutoApprovalStatus.FAILED,
        true);
  }

  @Test
   void onDomainEventConsumedPBPSuccessResponse() throws Exception {
    String message = mapper.writeValueAsString(productAutoApprovalEvent);
    when(objectMapper.readValue(message, ProductAutoApprovalEventModel.class)).thenReturn(productAutoApprovalEvent);
    when(productServiceRepository.getAutoApprovalType(productAutoApprovalEvent.getStoreId(),
        productAutoApprovalEvent.getProductCode(), false, autoApprovalTypeRequest)).thenReturn(new GdnRestSingleResponse(null,
        null, false, autoApprovalType, REQUEST_ID));
    when(productServiceRepository.getProductStatus(productAutoApprovalEvent.getStoreId(),
        productAutoApprovalEvent.getProductCode())).thenReturn(IN_PROGRESS);
    autoApprovalCriteraCheckEventListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, ProductAutoApprovalEventModel.class);
    verify(productServiceRepository).getProductStatus(STORE_ID, PRODUCT_CODE);
    verify(productAutoApprovalService).addProductsToAutoApprovalTable(STORE_ID, Collections.singletonList(PRODUCT_CODE),
        new HashMap<>());
  }

  @Test
   void onDomainEventConsumedPBPSuccessFalseOnlyCategoryChangeResponse() throws Exception {
    productAutoApprovalEvent.setOnlyCategoryUpdate(true);
    String message = mapper.writeValueAsString(productAutoApprovalEvent);
    when(objectMapper.readValue(message, ProductAutoApprovalEventModel.class)).thenReturn(productAutoApprovalEvent);
    when(productServiceRepository.getAutoApprovalType(productAutoApprovalEvent.getStoreId(),
        productAutoApprovalEvent.getProductCode(), true, autoApprovalTypeRequest)).thenReturn(
        new GdnRestSingleResponse(null, null, false, autoApprovalType, REQUEST_ID));
    when(productServiceRepository.getProductStatus(productAutoApprovalEvent.getStoreId(),
        productAutoApprovalEvent.getProductCode())).thenReturn(IN_PROGRESS);
    autoApprovalCriteraCheckEventListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, ProductAutoApprovalEventModel.class);
    verify(productServiceRepository).getAutoApprovalType(STORE_ID, PRODUCT_CODE, true, autoApprovalTypeRequest);
    verify(productServiceRepository).getProductStatus(STORE_ID, PRODUCT_CODE);
    verify(productAutoApprovalService).updateProductAutoApprovalDetailsByProductCode(STORE_ID, PRODUCT_CODE,
        AutoApprovalStatus.FAILED, true);
  }

  @Test
   void onDomainEventConsumedEligibleErrorTest() throws Exception {
    String message = mapper.writeValueAsString(productAutoApprovalEvent);
    Mockito.doThrow(RuntimeException.class).when(objectMapper).readValue(message, ProductAutoApprovalEventModel.class);
    autoApprovalCriteraCheckEventListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, ProductAutoApprovalEventModel.class);
  }

}
