package com.gdn.partners.pbp.service.listener;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3Service;
import com.gdn.x.productcategorybase.domain.event.model.VatUpdateHistoryDomainEventModel;

public class VatUpdateExternalHistoryListenerTest {
  private static final String REQUEST_ID = "requestId";
  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_ITEM_ID = "productItemId";
  private static final String ITEM_CODE = "itemCode";
  private static final String ITEM_NAME = "itemName";
  private static final String UPDATED_BY = "updatedBy";

  private VatUpdateHistoryDomainEventModel vatUpdateHistoryDomainEventModel;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductLevel3Service productLevel3Service;

  @InjectMocks
  private VatUpdateExternalHistoryListener vatUpdateExternalHistoryListener;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    vatUpdateHistoryDomainEventModel =
        new VatUpdateHistoryDomainEventModel(REQUEST_ID, STORE_ID, PRODUCT_ITEM_ID, ITEM_CODE, ITEM_NAME, UPDATED_BY,
            Boolean.FALSE.toString(), Boolean.TRUE.toString());
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(productLevel3Service);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    String message = getJsonObject(vatUpdateHistoryDomainEventModel);
    when(objectMapper.readValue(message, VatUpdateHistoryDomainEventModel.class))
        .thenReturn(vatUpdateHistoryDomainEventModel);
    doNothing().when(productLevel3Service).addVatUpdateExternalHistory(vatUpdateHistoryDomainEventModel);

    vatUpdateExternalHistoryListener.onDomainEventConsumed(message);

    verify(objectMapper).readValue(message, VatUpdateHistoryDomainEventModel.class);
    verify(productLevel3Service).addVatUpdateExternalHistory(vatUpdateHistoryDomainEventModel);
  }

  @Test
  public void onDomainEventConsumedSpanNullTest() throws Exception {
    String message = getJsonObject(vatUpdateHistoryDomainEventModel);
    when(objectMapper.readValue(message, VatUpdateHistoryDomainEventModel.class))
        .thenReturn(vatUpdateHistoryDomainEventModel);
    doNothing().when(productLevel3Service).addVatUpdateExternalHistory(vatUpdateHistoryDomainEventModel);
    vatUpdateExternalHistoryListener.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, VatUpdateHistoryDomainEventModel.class);
    verify(productLevel3Service).addVatUpdateExternalHistory(vatUpdateHistoryDomainEventModel);
  }

  @Test
  public void onDomainEventConsumedMessageNullTest() throws Exception {
    String message = getJsonObject(vatUpdateHistoryDomainEventModel);
    when(objectMapper.readValue(message, VatUpdateHistoryDomainEventModel.class))
        .thenReturn(null);

    vatUpdateExternalHistoryListener.onDomainEventConsumed(message);

    verify(objectMapper).readValue(message, VatUpdateHistoryDomainEventModel.class);
  }

  @Test
  public void onDomainEventConsumedProductItemIdNullTest() throws Exception {
    vatUpdateHistoryDomainEventModel.setProductItemId(null);
    String message = getJsonObject(vatUpdateHistoryDomainEventModel);
    when(objectMapper.readValue(message, VatUpdateHistoryDomainEventModel.class))
        .thenReturn(vatUpdateHistoryDomainEventModel);

    vatUpdateExternalHistoryListener.onDomainEventConsumed(message);

    verify(objectMapper).readValue(message, VatUpdateHistoryDomainEventModel.class);
  }

  @Test
  public void onDomainEventConsumedUpdatedByNullTest() throws Exception {
    vatUpdateHistoryDomainEventModel.setUpdatedBy(null);
    String message = getJsonObject(vatUpdateHistoryDomainEventModel);
    when(objectMapper.readValue(message, VatUpdateHistoryDomainEventModel.class))
        .thenReturn(vatUpdateHistoryDomainEventModel);

    vatUpdateExternalHistoryListener.onDomainEventConsumed(message);

    verify(objectMapper).readValue(message, VatUpdateHistoryDomainEventModel.class);
  }

  private String getJsonObject(VatUpdateHistoryDomainEventModel vatUpdateHistoryDomainEventModel)
      throws JsonProcessingException {
    return new ObjectMapper().writeValueAsString(vatUpdateHistoryDomainEventModel);
  }

  @Test
  public void setMandatoryParametersTest() throws Exception{
    String message = getJsonObject(vatUpdateHistoryDomainEventModel);
    when(objectMapper.readValue(message, VatUpdateHistoryDomainEventModel.class))
        .thenReturn(vatUpdateHistoryDomainEventModel);
    vatUpdateExternalHistoryListener.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, VatUpdateHistoryDomainEventModel.class);
    verify(productLevel3Service).addVatUpdateExternalHistory(vatUpdateHistoryDomainEventModel);
  }

}
