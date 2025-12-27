package com.gdn.x.mta.distributiontask.inbound.impl;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.AddProductToVendorCombinedEventModel;
import com.gdn.mta.domain.event.modal.AddRevisedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.AutoApprovalTypeRequestModel;
import com.gdn.mta.domain.event.modal.PDTDimensionRefreshEventModel;
import com.gdn.mta.domain.event.modal.ScreeningProductApprovalEvent;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.enums.SellerBadgeConstants;
import com.gdn.x.mta.distributiontask.service.api.AddProductToVendorService;

public class AddProductToVendorCombinedEventListenerTest {

  private static final String JSON = "{}";

  @InjectMocks
  private AddProductToVendorCombinedEventListener addProductToVendorCombinedEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private AddProductToVendorService addProductToVendorService;

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @BeforeEach
  public void before() {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, addProductToVendorService);
  }

  @Test
   void onDomainEventConsumedPriority0ScreeningApprovalTest() throws Exception {
    AddProductToVendorCombinedEventModel addProductToVendorCombinedEventModel =
        AddProductToVendorCombinedEventModel.builder()
            .screeningProductApprovalEvent(new ScreeningProductApprovalEvent()).build();
    addProductToVendorCombinedEventModel.getScreeningProductApprovalEvent().setSellerBadge(
      SellerBadgeConstants.GOLD_MERCHANT.getValue());
    Mockito.when(kafkaTopicPropertiesConsumer.getVendorCombinedEventNoPriority())
        .thenReturn(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT);
    Mockito.when(objectMapper.readValue(JSON, AddProductToVendorCombinedEventModel.class))
        .thenReturn(addProductToVendorCombinedEventModel);
    Mockito.doNothing().when(addProductToVendorService)
        .processScreeningApprovalEvent(addProductToVendorCombinedEventModel.getScreeningProductApprovalEvent(), 0);

    addProductToVendorCombinedEventListener.onDomainEventConsumedPriority0(JSON);

    Mockito.verify(objectMapper).readValue(JSON, AddProductToVendorCombinedEventModel.class);
    Mockito.verify(addProductToVendorService)
        .processScreeningApprovalEvent(addProductToVendorCombinedEventModel.getScreeningProductApprovalEvent(), 0);
    Mockito.verify(kafkaTopicPropertiesConsumer).getVendorCombinedEventNoPriority();
  }

  @Test
   void onDomainEventConsumedPriority0EditedTest() throws Exception {
    AddProductToVendorCombinedEventModel addProductToVendorCombinedEventModel =
        AddProductToVendorCombinedEventModel.builder().addEditedProductToPDTEvent(new AddEditedProductToPDTEvent())
            .build();
    Mockito.when(kafkaTopicPropertiesConsumer.getVendorCombinedEventNoPriority())
        .thenReturn(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT);
    Mockito.when(objectMapper.readValue(JSON, AddProductToVendorCombinedEventModel.class))
        .thenReturn(addProductToVendorCombinedEventModel);
    Mockito.doNothing().when(addProductToVendorService)
        .processAddEditedProductEvent(addProductToVendorCombinedEventModel.getAddEditedProductToPDTEvent());

    addProductToVendorCombinedEventListener.onDomainEventConsumedPriority0(JSON);

    Mockito.verify(objectMapper).readValue(JSON, AddProductToVendorCombinedEventModel.class);
    Mockito.verify(addProductToVendorService)
        .processAddEditedProductEvent(addProductToVendorCombinedEventModel.getAddEditedProductToPDTEvent());
    Mockito.verify(kafkaTopicPropertiesConsumer).getVendorCombinedEventNoPriority();
  }

  @Test
   void onDomainEventConsumedPriority0RevisedTest() throws Exception {
    AddProductToVendorCombinedEventModel addProductToVendorCombinedEventModel =
        AddProductToVendorCombinedEventModel.builder().addRevisedProductToPDTEvent(new AddRevisedProductToPDTEvent())
            .build();
    Mockito.when(kafkaTopicPropertiesConsumer.getVendorCombinedEventNoPriority())
        .thenReturn(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT);
    Mockito.when(objectMapper.readValue(JSON, AddProductToVendorCombinedEventModel.class))
        .thenReturn(addProductToVendorCombinedEventModel);
    Mockito.doNothing().when(addProductToVendorService)
        .processAddRevisedProductEvent(addProductToVendorCombinedEventModel.getAddRevisedProductToPDTEvent());

    addProductToVendorCombinedEventListener.onDomainEventConsumedPriority0(JSON);

    Mockito.verify(objectMapper).readValue(JSON, AddProductToVendorCombinedEventModel.class);
    Mockito.verify(addProductToVendorService)
        .processAddRevisedProductEvent(addProductToVendorCombinedEventModel.getAddRevisedProductToPDTEvent());
    Mockito.verify(kafkaTopicPropertiesConsumer).getVendorCombinedEventNoPriority();
  }

  @Test
   void onDomainEventConsumedPriority0DimensionRefreshTest() throws Exception {
    AddProductToVendorCombinedEventModel addProductToVendorCombinedEventModel =
        AddProductToVendorCombinedEventModel.builder().pdtDimensionRefreshEventModel(new PDTDimensionRefreshEventModel())
            .build();
    Mockito.when(kafkaTopicPropertiesConsumer.getVendorCombinedEventNoPriority())
        .thenReturn(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT);
    Mockito.when(objectMapper.readValue(JSON, AddProductToVendorCombinedEventModel.class))
        .thenReturn(addProductToVendorCombinedEventModel);

    addProductToVendorCombinedEventListener.onDomainEventConsumedPriority0(JSON);

    Mockito.verify(objectMapper).readValue(JSON, AddProductToVendorCombinedEventModel.class);
    Mockito.verify(addProductToVendorService)
        .processDimensionsUpdateEvent(addProductToVendorCombinedEventModel.getPdtDimensionRefreshEventModel());
    Mockito.verify(kafkaTopicPropertiesConsumer).getVendorCombinedEventNoPriority();
  }

  @Test
   void onDomainEventConsumedAutoApprovalTest() throws Exception {
    AddProductToVendorCombinedEventModel addProductToVendorCombinedEventModel =
        AddProductToVendorCombinedEventModel.builder().autoApprovalTypeRequestModel(new AutoApprovalTypeRequestModel())
            .build();
    Mockito.when(kafkaTopicPropertiesConsumer.getVendorCombinedEventNoPriority())
        .thenReturn(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT);
    Mockito.when(objectMapper.readValue(JSON, AddProductToVendorCombinedEventModel.class))
        .thenReturn(addProductToVendorCombinedEventModel);

    addProductToVendorCombinedEventListener.onDomainEventConsumedPriority0(JSON);

    Mockito.verify(objectMapper).readValue(JSON, AddProductToVendorCombinedEventModel.class);
    Mockito.verify(addProductToVendorService)
        .processAutoApprovalCheckEvent(addProductToVendorCombinedEventModel.getAutoApprovalTypeRequestModel());
    Mockito.verify(kafkaTopicPropertiesConsumer).getVendorCombinedEventNoPriority();
  }

  @Test
   void onDomainEventConsumedPriority0NoProcessTest() throws Exception {
    AddProductToVendorCombinedEventModel addProductToVendorCombinedEventModel =
        AddProductToVendorCombinedEventModel.builder().build();
    Mockito.when(objectMapper.readValue(JSON, AddProductToVendorCombinedEventModel.class))
        .thenReturn(addProductToVendorCombinedEventModel);

    addProductToVendorCombinedEventListener.onDomainEventConsumedPriority0(JSON);

    Mockito.verify(objectMapper).readValue(JSON, AddProductToVendorCombinedEventModel.class);
  }

  @Test
   void onDomainEventConsumedPriority0ErrorTest() throws Exception {
    Mockito.when(objectMapper.readValue(JSON, AddProductToVendorCombinedEventModel.class))
        .thenThrow(RuntimeException.class);

    addProductToVendorCombinedEventListener.onDomainEventConsumedPriority0(JSON);

    Mockito.verify(objectMapper).readValue(JSON, AddProductToVendorCombinedEventModel.class);
  }

  @Test
   void onDomainEventConsumedPriority1ScreeningApprovalTest() throws Exception {
    AddProductToVendorCombinedEventModel addProductToVendorCombinedEventModel =
        AddProductToVendorCombinedEventModel.builder()
            .screeningProductApprovalEvent(new ScreeningProductApprovalEvent()).build();
    Mockito.when(kafkaTopicPropertiesConsumer.getVendorCombinedEventPriority1())
        .thenReturn(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT_PRIORITY_1);
    Mockito.when(objectMapper.readValue(JSON, AddProductToVendorCombinedEventModel.class))
        .thenReturn(addProductToVendorCombinedEventModel);
    Mockito.doNothing().when(addProductToVendorService)
        .processScreeningApprovalEvent(addProductToVendorCombinedEventModel.getScreeningProductApprovalEvent(), 1);

    addProductToVendorCombinedEventListener.onDomainEventConsumedPriority1(JSON);

    Mockito.verify(objectMapper).readValue(JSON, AddProductToVendorCombinedEventModel.class);
    Mockito.verify(addProductToVendorService)
        .processScreeningApprovalEvent(addProductToVendorCombinedEventModel.getScreeningProductApprovalEvent(), 1);
    Mockito.verify(kafkaTopicPropertiesConsumer).getVendorCombinedEventPriority1();
  }

  @Test
   void onDomainEventConsumedPriority1NoProcessTest() throws Exception {
    AddProductToVendorCombinedEventModel addProductToVendorCombinedEventModel =
        AddProductToVendorCombinedEventModel.builder().build();
    Mockito.when(objectMapper.readValue(JSON, AddProductToVendorCombinedEventModel.class))
        .thenReturn(addProductToVendorCombinedEventModel);
    addProductToVendorCombinedEventListener.onDomainEventConsumedPriority1(JSON);

    Mockito.verify(objectMapper).readValue(JSON, AddProductToVendorCombinedEventModel.class);
   }

  @Test
   void onDomainEventConsumedPriority1ErrorTest() throws Exception {
    Mockito.when(objectMapper.readValue(JSON, AddProductToVendorCombinedEventModel.class))
        .thenThrow(RuntimeException.class);

    addProductToVendorCombinedEventListener.onDomainEventConsumedPriority1(JSON);

    Mockito.verify(objectMapper).readValue(JSON, AddProductToVendorCombinedEventModel.class);
  }

  @Test
   void onDomainEventConsumedPriority2ScreeningApprovalTest() throws Exception {
    AddProductToVendorCombinedEventModel addProductToVendorCombinedEventModel =
        AddProductToVendorCombinedEventModel.builder()
            .screeningProductApprovalEvent(new ScreeningProductApprovalEvent()).build();
    Mockito.when(kafkaTopicPropertiesConsumer.getVendorCombinedEventPriority2())
        .thenReturn(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT_PRIORITY_2);
    Mockito.when(objectMapper.readValue(JSON, AddProductToVendorCombinedEventModel.class))
        .thenReturn(addProductToVendorCombinedEventModel);
    Mockito.doNothing().when(addProductToVendorService)
        .processScreeningApprovalEvent(addProductToVendorCombinedEventModel.getScreeningProductApprovalEvent(), 2);

    addProductToVendorCombinedEventListener.onDomainEventConsumedPriority2(JSON);

    Mockito.verify(objectMapper).readValue(JSON, AddProductToVendorCombinedEventModel.class);
    Mockito.verify(addProductToVendorService)
        .processScreeningApprovalEvent(addProductToVendorCombinedEventModel.getScreeningProductApprovalEvent(), 2);
    Mockito.verify(kafkaTopicPropertiesConsumer).getVendorCombinedEventPriority2();
  }

  @Test
   void onDomainEventConsumedPriority2NoProcessTest() throws Exception {
    AddProductToVendorCombinedEventModel addProductToVendorCombinedEventModel =
        AddProductToVendorCombinedEventModel.builder().build();
    Mockito.when(objectMapper.readValue(JSON, AddProductToVendorCombinedEventModel.class))
        .thenReturn(addProductToVendorCombinedEventModel);
    addProductToVendorCombinedEventListener.onDomainEventConsumedPriority2(JSON);

    Mockito.verify(objectMapper).readValue(JSON, AddProductToVendorCombinedEventModel.class);
  }

  @Test
   void onDomainEventConsumedPriority2ErrorTest() throws Exception {
    Mockito.when(objectMapper.readValue(JSON, AddProductToVendorCombinedEventModel.class))
        .thenThrow(RuntimeException.class);

    addProductToVendorCombinedEventListener.onDomainEventConsumedPriority2(JSON);

    Mockito.verify(objectMapper).readValue(JSON, AddProductToVendorCombinedEventModel.class);
  }

}
