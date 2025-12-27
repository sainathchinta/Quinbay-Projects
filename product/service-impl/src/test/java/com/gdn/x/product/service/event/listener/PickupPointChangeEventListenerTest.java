package com.gdn.x.product.service.event.listener;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.gdn.x.product.domain.event.model.BusinessPartnerFlags;
import com.gdn.x.product.service.api.ChannelService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.businesspartner.domain.event.config.PickupPointDomainEventName;
import com.gdn.x.product.domain.event.enums.PickupPointChangeFields;
import com.gdn.x.product.domain.event.model.PickupPointChange;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.KafkaEventLogger;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.PickupPoint;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.KafkaEventLoggerService;
import com.gdn.x.product.service.api.OfflineItemService;
import com.gdn.x.product.service.api.PickupPointService;

public class PickupPointChangeEventListenerTest {

  @InjectMocks
  private PickupPointChangeEventListener listener;

  @Mock
  private OfflineItemService offlineItemService;

  @Mock
  private PickupPointService pickupPointService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaEventLoggerService kafkaEventLoggerService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ChannelService channelService;

  private PickupPointChange pickupPointChange;
  private List<OfflineItem> offlineItemList;
  private List<ItemPickupPoint> itemPickupPointList;
  private PickupPoint pickupPoint;
  private KafkaEventLogger kafkaEventLogger = KafkaEventLogger.builder().build();

  private static final String STORE_ID = "store-id";
  private static final String USERNAME = "system";
  private static final String BP_CODE = "BP-code";
  private static final String PICKUP_POINT_CODE = "PP-code";
  private static final String MESSAGE = "message";
  private static final long TIMESTAMP = 0L;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    pickupPoint = new PickupPoint();
    pickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    pickupPoint.setCncActivated(true);

    pickupPointChange = new PickupPointChange();
    pickupPointChange.setStoreId(STORE_ID);
    pickupPointChange.setBusinessPartnerCode(BP_CODE);
    pickupPointChange.setCode(PICKUP_POINT_CODE);
    pickupPointChange.setUpdatedBy(USERNAME);
    pickupPointChange.setCncActivated(false);
    pickupPointChange.setTimestamp(TIMESTAMP);

    offlineItemList = new ArrayList<>();
    OfflineItem offlineItem = new OfflineItem();
    offlineItem.setOfferPrice(0.0);
    offlineItem.setListPrice(0.0);
    offlineItem.setNewData(false);
    offlineItemList.add(offlineItem);

    itemPickupPointList = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Set<Price> priceSet = new HashSet<>();
    Price price = new Price();
    price.setListPrice(0.0);
    price.setOfferPrice(0.0);
    priceSet.add(price);
    itemPickupPoint.setPrice(priceSet);
    itemPickupPoint.setNewData(false);
    itemPickupPointList.add(itemPickupPoint);

    when(offlineItemService.findByPickupPointCode(STORE_ID, PICKUP_POINT_CODE))
        .thenReturn(offlineItemList);

    when(itemPickupPointService.findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(STORE_ID, PICKUP_POINT_CODE, true, false))
        .thenReturn(itemPickupPointList);

    when(itemPickupPointService.findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUP_POINT_CODE))
        .thenReturn(itemPickupPointList);

    when(
      this.kafkaEventLoggerService.findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(STORE_ID,
        TIMESTAMP, PICKUP_POINT_CODE, BP_CODE)).thenReturn(null);
    when(this.kafkaEventLoggerService.insertKafkaEventLogger(STORE_ID, TIMESTAMP, PICKUP_POINT_CODE,
      BP_CODE, PickupPointDomainEventName.UPDATE_FIELDS, MESSAGE)).thenReturn(kafkaEventLogger);
    ReflectionTestUtils.setField(listener, "sellerActionTypesToSkipPPUpdate", "");
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.offlineItemService);
    verifyNoMoreInteractions(this.pickupPointService);
    verifyNoMoreInteractions(this.objectMapper);
    verifyNoMoreInteractions(this.kafkaEventLoggerService);
    verifyNoMoreInteractions(this.channelService);
  }

  @Test
  public void pickupPointCncActivatedFalseChanged_success() throws Exception {
    ReflectionTestUtils.setField(listener, "sellerActionTypesToSkipPPUpdate",
      "SELLER_TERMINATION,SELLER_RESIGN");
    Set<PickupPointChangeFields> changeFields = new HashSet<>();
    changeFields.add(PickupPointChangeFields.CNC_ACTIVATED);
    pickupPointChange.setChangedFields(changeFields);
    pickupPoint.setCncActivated(false);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PickupPointChange.class)).thenReturn(pickupPointChange);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PickupPointChange.class);
    verify(itemPickupPointService).findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(STORE_ID,
        PICKUP_POINT_CODE, true, false);
    verify(offlineItemService).updateItemPickupPointForCNCDeactivation(STORE_ID, itemPickupPointList, BP_CODE,
        PICKUP_POINT_CODE);
    verify(offlineItemService).validateAndUpdateProductItemCncActivatedFalse(Mockito.anyString(), Mockito.anyList(),
        Mockito.anyString(), Mockito.anyString());
    verify(this.pickupPointService).upsertPickupPoint(STORE_ID, pickupPoint, USERNAME);
    verify(this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(STORE_ID, TIMESTAMP,
        PICKUP_POINT_CODE, BP_CODE);
    verify(this.kafkaEventLoggerService).insertKafkaEventLogger(STORE_ID, TIMESTAMP, PICKUP_POINT_CODE, BP_CODE,
        PickupPointDomainEventName.UPDATE_FIELDS, MESSAGE);
    verify(kafkaEventLoggerService).updateKafkaEventToFinished(Mockito.isNull());
    verify(offlineItemService).validateAndUpdateProductCncActivatedFalse(STORE_ID, itemPickupPointList,
        pickupPointChange.getUpdatedBy(), Constants.PICKUP_POINT_CHANGE);
  }

  @Test
  public void pickupPointCncActivatedFalseChanged_nonValidType_success() throws Exception {
    ReflectionTestUtils.setField(listener, "sellerActionTypesToSkipPPUpdate",
      "SELLER_TERMINATION,SELLER_RESIGN");
    Set<PickupPointChangeFields> changeFields = new HashSet<>();
    changeFields.add(PickupPointChangeFields.CNC_ACTIVATED);
    pickupPointChange.setChangedFields(changeFields);
    pickupPointChange.setActionTypes(Set.of("INVALID_ACTION"));
    pickupPoint.setCncActivated(false);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PickupPointChange.class)).thenReturn(pickupPointChange);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PickupPointChange.class);
    verify(itemPickupPointService).findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(STORE_ID,
      PICKUP_POINT_CODE, true, false);
    verify(offlineItemService).updateItemPickupPointForCNCDeactivation(STORE_ID, itemPickupPointList, BP_CODE,
      PICKUP_POINT_CODE);
    verify(offlineItemService).validateAndUpdateProductItemCncActivatedFalse(Mockito.anyString(), Mockito.anyList(),
      Mockito.anyString(), Mockito.anyString());
    verify(this.pickupPointService).upsertPickupPoint(STORE_ID, pickupPoint, USERNAME);
    verify(this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(STORE_ID, TIMESTAMP,
      PICKUP_POINT_CODE, BP_CODE);
    verify(this.kafkaEventLoggerService).insertKafkaEventLogger(STORE_ID, TIMESTAMP, PICKUP_POINT_CODE, BP_CODE,
      PickupPointDomainEventName.UPDATE_FIELDS, MESSAGE);
    verify(kafkaEventLoggerService).updateKafkaEventToFinished(Mockito.isNull());
    verify(offlineItemService).validateAndUpdateProductCncActivatedFalse(STORE_ID, itemPickupPointList,
      pickupPointChange.getUpdatedBy(), Constants.PICKUP_POINT_CHANGE);
  }

  @Test
  public void pickupPointCncActivatedFalseChanged_WithResignAction() throws Exception {
    ReflectionTestUtils.setField(listener, "sellerActionTypesToSkipPPUpdate",
      "SELLER_TERMINATION,SELLER_RESIGN");
    Set<PickupPointChangeFields> changeFields = new HashSet<>();
    changeFields.add(PickupPointChangeFields.CNC_ACTIVATED);
    pickupPointChange.setChangedFields(changeFields);
    pickupPointChange.setActionTypes(Set.of("SELLER_RESIGN"));
    pickupPoint.setCncActivated(false);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PickupPointChange.class)).thenReturn(pickupPointChange);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PickupPointChange.class);
  }

  @Test
  public void pickupPointCncActivatedFalseChanged_cncForWarehouseSuccess() throws Exception {
    ReflectionTestUtils.setField(listener, "cncForWarehouseFeatureSwitch",
        Boolean.valueOf("true"));
    when(channelService.getCncChannel()).thenReturn("CNC");
    when(itemPickupPointService.findByStoreIdAndPickupPointCodeAndConfigFlagValuesAndMarkForDeleteAndChannel(
        STORE_ID, PICKUP_POINT_CODE, true, false,"CNC")).thenReturn(itemPickupPointList);
    Set<PickupPointChangeFields> changeFields = new HashSet<>();
    changeFields.add(PickupPointChangeFields.CNC_ACTIVATED);
    pickupPointChange.setChangedFields(changeFields);
    pickupPoint.setCncActivated(false);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PickupPointChange.class)).thenReturn(pickupPointChange);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PickupPointChange.class);
    verify(itemPickupPointService).findByStoreIdAndPickupPointCodeAndConfigFlagValuesAndMarkForDeleteAndChannel(STORE_ID,
        PICKUP_POINT_CODE, true, false,"CNC");
    verify(offlineItemService).updateItemPickupPointForCNCDeactivation(STORE_ID, itemPickupPointList, BP_CODE,
        PICKUP_POINT_CODE);
    verify(channelService).getCncChannel();
    verify(offlineItemService).validateAndUpdateProductItemCncActivatedFalse(Mockito.anyString(), Mockito.anyList(),
        Mockito.anyString(), Mockito.anyString());
    verify(this.pickupPointService).upsertPickupPoint(STORE_ID, pickupPoint, USERNAME);
    verify(this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(STORE_ID, TIMESTAMP,
        PICKUP_POINT_CODE, BP_CODE);
    verify(this.kafkaEventLoggerService).insertKafkaEventLogger(STORE_ID, TIMESTAMP, PICKUP_POINT_CODE, BP_CODE,
        PickupPointDomainEventName.UPDATE_FIELDS, MESSAGE);
    verify(kafkaEventLoggerService).updateKafkaEventToFinished(null);
    verify(offlineItemService).validateAndUpdateProductCncActivatedFalse(STORE_ID, itemPickupPointList,
        pickupPointChange.getUpdatedBy(), Constants.PICKUP_POINT_CHANGE);
  }

  @Test
  public void pickupPointDeliveryActivatedFalseChanged_cncForWarehouseSuccess() throws Exception {
    ReflectionTestUtils.setField(listener, "cncForWarehouseFeatureSwitch", Boolean.valueOf("true"));
    when(channelService.getDefaultChannel()).thenReturn("DEFAULT");
    when(
        itemPickupPointService.findByStoreIdAndPickupPointCodeAndConfigFlagValuesAndMarkForDeleteAndChannel(
            STORE_ID, PICKUP_POINT_CODE, true, false, "DEFAULT")).thenReturn(itemPickupPointList);
    pickupPoint.setCncActivated(false);
    Set<PickupPointChangeFields> changeFields = new HashSet<>();
    changeFields.add(PickupPointChangeFields.DELIVERY_FLAG);
    pickupPointChange.setChangedFields(changeFields);
    pickupPointChange.setFlags(BusinessPartnerFlags.builder().deliveryFlag(false).build());
    Mockito.when(this.objectMapper.readValue(MESSAGE, PickupPointChange.class))
        .thenReturn(pickupPointChange);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PickupPointChange.class);
    verify(
        itemPickupPointService).findByStoreIdAndPickupPointCodeAndConfigFlagValuesAndMarkForDeleteAndChannel(
        STORE_ID, PICKUP_POINT_CODE, true, false, "DEFAULT");
    verify(channelService).getDefaultChannel();
    verify(offlineItemService).updateItemPickupPointForDeliveryDeactivation(STORE_ID,
        itemPickupPointList, BP_CODE, PICKUP_POINT_CODE);
    verify(this.pickupPointService).upsertPickupPoint(STORE_ID, pickupPoint, USERNAME);
    verify(this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(
        STORE_ID, TIMESTAMP, PICKUP_POINT_CODE, BP_CODE);
    verify(this.kafkaEventLoggerService).insertKafkaEventLogger(STORE_ID, TIMESTAMP,
        PICKUP_POINT_CODE, BP_CODE, PickupPointDomainEventName.UPDATE_FIELDS, MESSAGE);
    verify(kafkaEventLoggerService).updateKafkaEventToFinished(null);
  }


  @Test
  public void pickupPointDeliveryActivatedTrueChanged_cncForWarehouseSuccess() throws Exception {
    ReflectionTestUtils.setField(listener, "cncForWarehouseFeatureSwitch", Boolean.valueOf("true"));
    when(
        itemPickupPointService.findByStoreIdAndPickupPointCodeAndConfigFlagValuesAndMarkForDeleteAndChannel(
            STORE_ID, PICKUP_POINT_CODE, true, false,"CNC")).thenReturn(itemPickupPointList);
    pickupPoint.setCncActivated(false);
    Set<PickupPointChangeFields> changeFields = new HashSet<>();
    changeFields.add(PickupPointChangeFields.DELIVERY_FLAG);
    pickupPointChange.setChangedFields(changeFields);
    pickupPointChange.setFlags(BusinessPartnerFlags.builder().deliveryFlag(true).build());
    Mockito.when(this.objectMapper.readValue(MESSAGE, PickupPointChange.class))
        .thenReturn(pickupPointChange);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PickupPointChange.class);
    verify(this.pickupPointService).upsertPickupPoint(STORE_ID, pickupPoint, USERNAME);
    verify(this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(
        STORE_ID, TIMESTAMP, PICKUP_POINT_CODE, BP_CODE);
    verify(this.kafkaEventLoggerService).insertKafkaEventLogger(STORE_ID, TIMESTAMP,
        PICKUP_POINT_CODE, BP_CODE, PickupPointDomainEventName.UPDATE_FIELDS, MESSAGE);
    verify(kafkaEventLoggerService).updateKafkaEventToFinished(null);
  }

  @Test
  public void pickupPointCncActivatedTrueChanged_success() throws Exception {
    Set<PickupPointChangeFields> changeFields = new HashSet<>();
    changeFields.add(PickupPointChangeFields.CNC_ACTIVATED);
    pickupPointChange.setChangedFields(changeFields);
    pickupPointChange.setCncActivated(true);

    Mockito.when(this.objectMapper.readValue(MESSAGE, PickupPointChange.class)).thenReturn(pickupPointChange);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PickupPointChange.class);

    verify(offlineItemService, times(0))
        .updateItemPickupPointForCNCDeactivation(STORE_ID, itemPickupPointList, BP_CODE, PICKUP_POINT_CODE);

    verify(offlineItemService, times(0))
        .updateProductItemCncActivatedTrue(STORE_ID, PICKUP_POINT_CODE, USERNAME, offlineItemList);
    verify(this.pickupPointService).upsertPickupPoint(STORE_ID, pickupPoint, USERNAME);
    verify(this.kafkaEventLoggerService)
        .findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(STORE_ID, TIMESTAMP, PICKUP_POINT_CODE, BP_CODE);
    verify(this.kafkaEventLoggerService).insertKafkaEventLogger(STORE_ID, TIMESTAMP, PICKUP_POINT_CODE, BP_CODE,
        PickupPointDomainEventName.UPDATE_FIELDS, MESSAGE);
    verify(kafkaEventLoggerService).updateKafkaEventToFinished(Mockito.isNull());
    verify(itemPickupPointService, times(0)).findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(STORE_ID, PICKUP_POINT_CODE, true, false);
  }


  @Test
  public void pickupPointCncActivatedTrueChanged_success_deletePickupPointTrue() throws Exception {
    ReflectionTestUtils.setField(listener, "deletePickupPointSwitch", true);
    Set<PickupPointChangeFields> changeFields = new HashSet<>();
    changeFields.add(PickupPointChangeFields.CNC_ACTIVATED);
    pickupPointChange.setChangedFields(changeFields);
    pickupPointChange.setCncActivated(true);

    Mockito.when(this.objectMapper.readValue(MESSAGE, PickupPointChange.class)).thenReturn(pickupPointChange);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PickupPointChange.class);

    verify(offlineItemService, times(0)).updateItemPickupPointForCNCDeactivation(STORE_ID, itemPickupPointList, BP_CODE,
        PICKUP_POINT_CODE);

    verify(offlineItemService, times(0)).updateProductItemCncActivatedTrue(STORE_ID, PICKUP_POINT_CODE, USERNAME,
        offlineItemList);
    verify(this.pickupPointService).upsertPickupPoint(STORE_ID, pickupPoint, USERNAME);
    verify(this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(STORE_ID, TIMESTAMP,
        PICKUP_POINT_CODE, BP_CODE);
    verify(this.kafkaEventLoggerService).insertKafkaEventLogger(STORE_ID, TIMESTAMP, PICKUP_POINT_CODE, BP_CODE,
        PickupPointDomainEventName.UPDATE_FIELDS, MESSAGE);
    verify(kafkaEventLoggerService).updateKafkaEventToFinished(Mockito.isNull());
    verify(itemPickupPointService, times(0)).findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(STORE_ID,
        PICKUP_POINT_CODE, true, false);
  }

  @Test
  public void pickupPointCncActivatedChanged_exception() throws Exception {
    Set<PickupPointChangeFields> changeFields = new HashSet<>();
    changeFields.add(PickupPointChangeFields.CNC_ACTIVATED);
    pickupPointChange.setChangedFields(changeFields);
    pickupPointChange.setCncActivated(false);

    doThrow(new ApplicationRuntimeException()).when(offlineItemService)
        .updateItemPickupPointForCNCDeactivation(STORE_ID, itemPickupPointList, BP_CODE,
            PICKUP_POINT_CODE);

    Mockito.when(this.objectMapper.readValue(MESSAGE, PickupPointChange.class))
      .thenReturn(pickupPointChange);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PickupPointChange.class);

    verify(itemPickupPointService).findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(STORE_ID, PICKUP_POINT_CODE, true, false);

    verify(offlineItemService).updateItemPickupPointForCNCDeactivation(STORE_ID, itemPickupPointList, BP_CODE, PICKUP_POINT_CODE);
    verify(this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(
      STORE_ID, TIMESTAMP, PICKUP_POINT_CODE, BP_CODE);
    verify(this.kafkaEventLoggerService).insertKafkaEventLogger(STORE_ID, TIMESTAMP, PICKUP_POINT_CODE,
      BP_CODE, PickupPointDomainEventName.UPDATE_FIELDS, MESSAGE);
    verify(kafkaEventLoggerService).updateKafkaEventToFinished(Mockito.isNull());
  }

  @Test
  public void pickupPointCncActivatedChangedTest_pickupPointUpdated() throws Exception {
    Set<PickupPointChangeFields> changeFields = new HashSet<>();
    changeFields.add(PickupPointChangeFields.EXTERNAL_PICKUP_POINT_CODE);
    pickupPointChange.setChangedFields(changeFields);
    offlineItemList.get(0).setMarkForDelete(true);

    pickupPointChange.setCncActivated(Boolean.TRUE);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PickupPointChange.class))
      .thenReturn(pickupPointChange);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PickupPointChange.class);

    verify(itemPickupPointService).findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUP_POINT_CODE);
    verify(offlineItemService).deleteByPickupPointCode_ItemPickupPoint(STORE_ID, PICKUP_POINT_CODE);
    verify(offlineItemService).validateAndUpdateProductItemCncActivatedFalse(STORE_ID, offlineItemList, USERNAME,
        Constants.PICKUP_POINT_CHANGE);
    verify(this.pickupPointService).upsertPickupPoint(STORE_ID, pickupPoint, USERNAME);
    verify(this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(
      STORE_ID, TIMESTAMP, PICKUP_POINT_CODE, BP_CODE);
    verify(this.kafkaEventLoggerService).insertKafkaEventLogger(STORE_ID, TIMESTAMP, PICKUP_POINT_CODE,
      BP_CODE, PickupPointDomainEventName.UPDATE_FIELDS, MESSAGE);
    verify(kafkaEventLoggerService).updateKafkaEventToFinished(Mockito.isNull());
    verify(offlineItemService).validateAndUpdateProductCncActivatedFalse(STORE_ID, itemPickupPointList,
        pickupPointChange.getUpdatedBy(), Constants.PICKUP_POINT_CHANGE);
  }

  @Test
  public void pickupPointCncActivatedChangedTest_archivedTrue() throws Exception {
    Set<PickupPointChangeFields> changeFields = new HashSet<>();
    changeFields.add(PickupPointChangeFields.ARCHIVED);
    pickupPointChange.setChangedFields(changeFields);
    pickupPointChange.setArchived(Boolean.TRUE);
    pickupPointChange.setCncActivated(Boolean.TRUE);
    offlineItemList.get(0).setMarkForDelete(true);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PickupPointChange.class))
      .thenReturn(pickupPointChange);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PickupPointChange.class);

    verify(itemPickupPointService).findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUP_POINT_CODE);
    verify(offlineItemService).deleteByPickupPointCode_ItemPickupPoint(STORE_ID, PICKUP_POINT_CODE);
    verify(this.offlineItemService).validateAndUpdateProductItemCncActivatedFalse(STORE_ID, offlineItemList, USERNAME,
        Constants.PICKUP_POINT_CHANGE);
    verify(this.pickupPointService).upsertPickupPoint(STORE_ID, pickupPoint, USERNAME);
    verify(this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(
      STORE_ID, TIMESTAMP, PICKUP_POINT_CODE, BP_CODE);
    verify(this.kafkaEventLoggerService).insertKafkaEventLogger(STORE_ID, TIMESTAMP, PICKUP_POINT_CODE,
      BP_CODE, PickupPointDomainEventName.UPDATE_FIELDS, MESSAGE);
    verify(kafkaEventLoggerService).updateKafkaEventToFinished(Mockito.isNull());
    verify(offlineItemService).validateAndUpdateProductCncActivatedFalse(STORE_ID, itemPickupPointList,
        pickupPointChange.getUpdatedBy(), Constants.PICKUP_POINT_CHANGE);
  }

  @Test
  public void pickupPointCncActivatedChangedTest_archievedFalse() throws Exception {
    Set<PickupPointChangeFields> changeFields = new HashSet<>();
    changeFields.add(PickupPointChangeFields.ARCHIVED);
    pickupPointChange.setChangedFields(changeFields);
    pickupPointChange.setArchived(Boolean.FALSE);

    pickupPointChange.setCncActivated(Boolean.TRUE);
    Mockito.when(this.objectMapper.readValue(MESSAGE, PickupPointChange.class))
      .thenReturn(pickupPointChange);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PickupPointChange.class);
    verify(this.pickupPointService)
        .upsertPickupPoint(STORE_ID, pickupPoint, USERNAME);
    verify(this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(
      STORE_ID, TIMESTAMP, PICKUP_POINT_CODE, BP_CODE);
    verify(this.kafkaEventLoggerService).insertKafkaEventLogger(STORE_ID, TIMESTAMP, PICKUP_POINT_CODE,
      BP_CODE, PickupPointDomainEventName.UPDATE_FIELDS, MESSAGE);
    verify(kafkaEventLoggerService).updateKafkaEventToFinished(Mockito.isNull());
  }

  @Test
  public void pickupPointCncActivated_reconsumptionTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, PickupPointChange.class))
      .thenReturn(pickupPointChange);
    when(
      this.kafkaEventLoggerService.findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(STORE_ID,
        TIMESTAMP, PICKUP_POINT_CODE, BP_CODE)).thenReturn(kafkaEventLogger);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, PickupPointChange.class);
    verify(this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(
      STORE_ID, TIMESTAMP, PICKUP_POINT_CODE, BP_CODE);
  }
}
