package com.gdn.mta.product.service.domainevent;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.response.InternalProductHistoryEventModel;
import com.gdn.mta.product.service.ProductServiceBean;

public class ProductHistorySubscriberTest {
  private static final String STORE_ID = "STORE_ID";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String USERNAME = "USERNAME";
  private static final String ACTIVITY = "ACTIVITY";
  private static final String NOTES = "NOTES";


  private ObjectMapper mapper;
  private InternalProductHistoryEventModel internalProductHistoryEventModel;

  @InjectMocks
  ProductHistorySubscriber productHistorySubscriber;

  @Mock
  private ProductServiceBean productServiceBean;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    internalProductHistoryEventModel = new InternalProductHistoryEventModel();
    internalProductHistoryEventModel.setStoreId(STORE_ID);
    internalProductHistoryEventModel.setProductCode(PRODUCT_CODE);
    internalProductHistoryEventModel.setUsername(USERNAME);
    internalProductHistoryEventModel.setActivity(ACTIVITY);
    internalProductHistoryEventModel.setNotes(NOTES);
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productServiceBean, objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    String message = mapper.writeValueAsString(internalProductHistoryEventModel);
    Mockito.when(objectMapper.readValue(message, InternalProductHistoryEventModel.class))
        .thenReturn(this.internalProductHistoryEventModel);
    this.productHistorySubscriber.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, InternalProductHistoryEventModel.class);
    Mockito.verify(productServiceBean).saveProductHistory(internalProductHistoryEventModel.getStoreId(),
        internalProductHistoryEventModel.getProductCode(), internalProductHistoryEventModel.getUsername(),
        internalProductHistoryEventModel.getActivity(), internalProductHistoryEventModel.getNotes());
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    String message = mapper.writeValueAsString(internalProductHistoryEventModel);
    Mockito.when(objectMapper.readValue(message, InternalProductHistoryEventModel.class))
        .thenReturn(this.internalProductHistoryEventModel);
    Mockito.doThrow(RuntimeException.class).when(productServiceBean)
        .saveProductHistory(internalProductHistoryEventModel.getStoreId(),
            internalProductHistoryEventModel.getProductCode(), internalProductHistoryEventModel.getUsername(),
            internalProductHistoryEventModel.getActivity(), internalProductHistoryEventModel.getNotes());
    try {
      this.productHistorySubscriber.onDomainEventConsumed(message);
    } finally {
      Mockito.verify(objectMapper).readValue(message, InternalProductHistoryEventModel.class);
      Mockito.verify(productServiceBean).saveProductHistory(internalProductHistoryEventModel.getStoreId(),
          internalProductHistoryEventModel.getProductCode(), internalProductHistoryEventModel.getUsername(),
          internalProductHistoryEventModel.getActivity(), internalProductHistoryEventModel.getNotes());
    }
  }
}
