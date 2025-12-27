package com.gdn.mta.product.service.domainevent;

import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.service.HalalHistoryUpdateService;
import com.gdn.x.product.domain.event.model.HalalHistoryUpdateEventModel;

public class HalalHistoryUpdateEventListenerTest {

  @InjectMocks
  private HalalHistoryUpdateEventListener halalHistoryUpdateEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private HalalHistoryUpdateService halalHistoryUpdateService;

  private HalalHistoryUpdateEventModel halalHistoryUpdateEventModel;
  private static final String PRODUCT_SKU = "productSku";
  private static final String STORE_ID = "storeId";
  private static final String ACTIVITY = "activity";
  private static final String PREVIOUS_VALUE = "previousValue";
  private static final String CURRENT_VALUE = "currentValue";
  private static final String USER_NAME = "userName";
  private String message;

  @BeforeEach
  public void init() throws Exception {
    initMocks(this);
    halalHistoryUpdateEventModel = new HalalHistoryUpdateEventModel();
    halalHistoryUpdateEventModel.setProductSku(PRODUCT_SKU);
    halalHistoryUpdateEventModel.setStoreId(STORE_ID);
    halalHistoryUpdateEventModel.setUserName(USER_NAME);
    halalHistoryUpdateEventModel.setActivity(ACTIVITY);
    halalHistoryUpdateEventModel.setCurrentValue(CURRENT_VALUE);
    halalHistoryUpdateEventModel.setPreviousValue(PREVIOUS_VALUE);

    ObjectMapper mapper = new ObjectMapper();
    message = mapper.writeValueAsString(halalHistoryUpdateEventModel);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(halalHistoryUpdateService);
  }

  @Test
  public void HalalHistoryUpdateListenerTest() throws Exception {
    Mockito.when(objectMapper.readValue(message, HalalHistoryUpdateEventModel.class))
        .thenReturn(halalHistoryUpdateEventModel);
    halalHistoryUpdateEventListener.onDomainEventConsumed(message);
    Mockito.verify(halalHistoryUpdateService).saveHalalHistoryUpdate(halalHistoryUpdateEventModel);
  }

  @Test
  public void HalalHistoryUpdateListenerExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(message, HalalHistoryUpdateEventModel.class))
        .thenThrow(new NullPointerException());
    halalHistoryUpdateEventListener.onDomainEventConsumed(message);
  }
}
