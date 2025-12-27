package com.gdn.x.product.service.event.listener;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.ext.catalog.enums.EventActionType;
import com.gdn.ext.catalog.model.entity.BlibliUpdateChangeEventModel;
import com.gdn.x.product.service.api.ItemService;

public class PristineUpdateEventListenerTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String PRISTINE_MASTER_ID = "pristineMasterId";
  private static final String ITEM_CODE = "ITEM_CODE";
  private static final String MESSAGE = "message";

  private BlibliUpdateChangeEventModel message;
  private BlibliUpdateChangeEventModel message1;

  @InjectMocks
  private PristineUpdateEventListener pristineUpdateEventListener;

  @Mock
  private ItemService itemService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    message =
        new BlibliUpdateChangeEventModel(null, PRISTINE_MASTER_ID, PRODUCT_CODE, 12335, EventActionType.DPC_UPDATE);
    message1 = new BlibliUpdateChangeEventModel(ITEM_CODE, PRISTINE_MASTER_ID, PRODUCT_CODE, 12335,
        EventActionType.MASTER_DPC_UPDATE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest_DPC() throws Exception {
    doNothing().when(itemService).updatePristineDPC(message.getPcbProductItemId(), message.getPristineMasterId(),
        message.getDefaultProductCode());
    Mockito.when(this.objectMapper.readValue(MESSAGE, BlibliUpdateChangeEventModel.class))
      .thenReturn(message);
    this.pristineUpdateEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BlibliUpdateChangeEventModel.class);
    verify(itemService).updatePristineDPC(message.getPcbProductItemId(), message.getPristineMasterId(),
        message.getDefaultProductCode());
  }

  @Test
  public void onDomainEventConsumedTest_MasterDPC() throws Exception {
    doNothing().when(itemService).updatePristineDPC(message.getPcbProductItemId(), message.getPristineMasterId(),
        message.getDefaultProductCode());
    Mockito.when(this.objectMapper.readValue(MESSAGE, BlibliUpdateChangeEventModel.class))
      .thenReturn(message1);
    this.pristineUpdateEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BlibliUpdateChangeEventModel.class);
    verify(itemService).updatePristineDPC(message1.getPcbProductItemId(), message1.getPristineMasterId(),
        message1.getDefaultProductCode());
  }

  @Test
  public void onDomainEventConsumedTest_WrongType() throws Exception {
    message1.setPcbProductItemId(null);
    doNothing().when(itemService).updatePristineDPC(message1.getPcbProductItemId(), message1.getPristineMasterId(),
        message1.getDefaultProductCode());
    Mockito.when(this.objectMapper.readValue(MESSAGE, BlibliUpdateChangeEventModel.class))
      .thenReturn(message1);
    this.pristineUpdateEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BlibliUpdateChangeEventModel.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    doThrow(RuntimeException.class).when(this.itemService).updatePristineDPC(anyString(), anyString(), anyString());
    Mockito.when(this.objectMapper.readValue(MESSAGE, BlibliUpdateChangeEventModel.class))
      .thenReturn(message);
    this.pristineUpdateEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BlibliUpdateChangeEventModel.class);
    verify(itemService).updatePristineDPC(isNull(), anyString(), anyString());
  }
}
