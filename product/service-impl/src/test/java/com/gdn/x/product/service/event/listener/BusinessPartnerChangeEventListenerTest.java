package com.gdn.x.product.service.event.listener;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.businesspartner.domain.event.config.BusinessPartnerDomainEventName;
import com.gdn.x.businesspartner.domain.event.enums.CompanyChangeFields;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerChange;
import com.gdn.x.businesspartner.domain.event.model.CompanyVO;
import com.gdn.x.product.model.entity.KafkaEventLogger;
import com.gdn.x.product.service.api.ItemPickupPointWrapperService;
import com.gdn.x.product.service.api.KafkaEventLoggerService;
import com.gdn.x.product.service.api.OfflineItemService;
import org.springframework.test.util.ReflectionTestUtils;

public class BusinessPartnerChangeEventListenerTest {

    @InjectMocks
    private BusinessPartnerChangeEventListener listener;

    @Mock
    private OfflineItemService service;

    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private ItemPickupPointWrapperService itemPickupPointWrapperService;

    @Mock
    private KafkaEventLoggerService kafkaEventLoggerService;

    private BusinessPartnerChange businessPartnerChange;

    private static final String STORE_ID = "store-id";
    private static final String BP_CODE = "BP-code";
    private static final String USER_NAME = "username";
    private static final String MESSAGE = "message";

    private static final long TIMESTAMP = 0L;

    private KafkaEventLogger kafkaEventLogger = KafkaEventLogger.builder().build();

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        businessPartnerChange = new BusinessPartnerChange();
        businessPartnerChange.setStoreId(STORE_ID);
        businessPartnerChange.setBusinessPartnerCode(BP_CODE);
        businessPartnerChange.setUpdatedBy(USER_NAME);
        CompanyVO companyVO = new CompanyVO();
        companyVO.setCncActivated(false);
        businessPartnerChange.setCompany(companyVO);
        businessPartnerChange.setTimestamp(TIMESTAMP);
    }

    @AfterEach
    public void tearDown() throws Exception {
        Mockito.verifyNoMoreInteractions(this.service);
        Mockito.verifyNoMoreInteractions(this.objectMapper);
        Mockito.verifyNoMoreInteractions(this.itemPickupPointWrapperService);
        verifyNoMoreInteractions(this.kafkaEventLoggerService);
    }

    @Test
    public void onDomainEventConsumed_NullTest() throws Exception {
        businessPartnerChange.setBusinessPartnerCode(null);
        businessPartnerChange.setStoreId(null);
        Mockito.when(this.objectMapper.readValue(MESSAGE, BusinessPartnerChange.class))
          .thenReturn(businessPartnerChange);
        Mockito.when(this.kafkaEventLoggerService.findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(null, TIMESTAMP,
            null, BusinessPartnerDomainEventName.UPDATE_FIELDS)).thenReturn(null);
        Mockito.when(this.kafkaEventLoggerService.insertKafkaEventLogger(null, TIMESTAMP, null,
            BusinessPartnerDomainEventName.UPDATE_FIELDS, BusinessPartnerDomainEventName.UPDATE_FIELDS,
            MESSAGE)).thenReturn(kafkaEventLogger);
        listener.onDomainEventConsumed(MESSAGE);
        Mockito.verify(this.objectMapper).readValue(MESSAGE, BusinessPartnerChange.class);
        verify(this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(null, TIMESTAMP,
            null,BusinessPartnerDomainEventName.UPDATE_FIELDS);
        verify(this.kafkaEventLoggerService).insertKafkaEventLogger(null, TIMESTAMP,null,
            BusinessPartnerDomainEventName.UPDATE_FIELDS,BusinessPartnerDomainEventName.UPDATE_FIELDS, MESSAGE);
        verify(kafkaEventLoggerService).updateKafkaEventToFinished(Mockito.isNull());
    }

    @Test
    public void onDomainEventConsumedCncActivatedChanged_exception() throws Exception {
        Set<String> changeFields = new HashSet<>();
        changeFields.add(CompanyChangeFields.CNC_ACTIVATED.name());
        businessPartnerChange.getCompany().setChangedFields(changeFields);

        Mockito.doThrow(new ApplicationRuntimeException()).when(service)
                .deleteOfflineItemByMerchantCode(STORE_ID, BP_CODE, USER_NAME);

        Mockito.when(this.objectMapper.readValue(MESSAGE, BusinessPartnerChange.class))
          .thenReturn(businessPartnerChange);
        Mockito.when(this.kafkaEventLoggerService.findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(STORE_ID, TIMESTAMP,
            BP_CODE, BusinessPartnerDomainEventName.UPDATE_FIELDS)).thenReturn(null);
        Mockito.when(this.kafkaEventLoggerService.insertKafkaEventLogger(STORE_ID, TIMESTAMP, BP_CODE,
            BusinessPartnerDomainEventName.UPDATE_FIELDS, BusinessPartnerDomainEventName.UPDATE_FIELDS,
            MESSAGE)).thenReturn(kafkaEventLogger);
        listener.onDomainEventConsumed(MESSAGE);
        Mockito.verify(this.objectMapper).readValue(MESSAGE, BusinessPartnerChange.class);
        verify(this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(STORE_ID, TIMESTAMP,
            BP_CODE,BusinessPartnerDomainEventName.UPDATE_FIELDS);
        verify(this.kafkaEventLoggerService).insertKafkaEventLogger(STORE_ID, TIMESTAMP,BP_CODE,
            BusinessPartnerDomainEventName.UPDATE_FIELDS,BusinessPartnerDomainEventName.UPDATE_FIELDS, MESSAGE);
        verify(kafkaEventLoggerService).updateKafkaEventToFinished(Mockito.isNull());
    }

    @Test
    public void onDomainEventConsumedCncActivatedChanged_success() throws Exception {
        Set<String> changeFields = new HashSet<>();
        changeFields.add(CompanyChangeFields.CNC_ACTIVATED.name());
        businessPartnerChange.getCompany().setChangedFields(changeFields);

        businessPartnerChange.getCompany().setCncActivated(Boolean.TRUE);
        Mockito.when(this.objectMapper.readValue(MESSAGE, BusinessPartnerChange.class))
          .thenReturn(businessPartnerChange);
        Mockito.when(this.kafkaEventLoggerService.findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(STORE_ID, TIMESTAMP,
            BP_CODE, BusinessPartnerDomainEventName.UPDATE_FIELDS)).thenReturn(null);
        Mockito.when(this.kafkaEventLoggerService.insertKafkaEventLogger(STORE_ID, TIMESTAMP, BP_CODE,
            BusinessPartnerDomainEventName.UPDATE_FIELDS, BusinessPartnerDomainEventName.UPDATE_FIELDS,
            MESSAGE)).thenReturn(kafkaEventLogger);
        listener.onDomainEventConsumed(MESSAGE);
        Mockito.verify(this.objectMapper).readValue(MESSAGE, BusinessPartnerChange.class);
        verify(this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(STORE_ID, TIMESTAMP,
            BP_CODE,BusinessPartnerDomainEventName.UPDATE_FIELDS);
        verify(this.kafkaEventLoggerService).insertKafkaEventLogger(STORE_ID, TIMESTAMP,BP_CODE,
            BusinessPartnerDomainEventName.UPDATE_FIELDS,BusinessPartnerDomainEventName.UPDATE_FIELDS, MESSAGE);
        verify(kafkaEventLoggerService).updateKafkaEventToFinished(Mockito.isNull());
    }

    @Test
    public void onDomainEventConsumedCncActivatedChanged_success_cncForWarehouseTrue()
        throws Exception {
        ReflectionTestUtils.setField(listener, "cncForWarehouseFeatureSwitch",
            Boolean.valueOf("true"));
        Set<String> changeFields = new HashSet<>();
        changeFields.add(CompanyChangeFields.CNC_ACTIVATED.name());
        businessPartnerChange.getCompany().setChangedFields(changeFields);

        businessPartnerChange.getCompany().setCncActivated(Boolean.FALSE);
        Mockito.when(this.objectMapper.readValue(MESSAGE, BusinessPartnerChange.class))
            .thenReturn(businessPartnerChange);
        Mockito.when(
                this.kafkaEventLoggerService.findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(
                    STORE_ID, TIMESTAMP, BP_CODE, BusinessPartnerDomainEventName.UPDATE_FIELDS))
            .thenReturn(null);
        Mockito.when(
                this.kafkaEventLoggerService.insertKafkaEventLogger(STORE_ID, TIMESTAMP, BP_CODE,
                    BusinessPartnerDomainEventName.UPDATE_FIELDS,
                    BusinessPartnerDomainEventName.UPDATE_FIELDS, MESSAGE))
            .thenReturn(kafkaEventLogger);
        listener.onDomainEventConsumed(MESSAGE);
        Mockito.verify(this.objectMapper).readValue(MESSAGE, BusinessPartnerChange.class);
        verify(
            this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(
            STORE_ID, TIMESTAMP, BP_CODE, BusinessPartnerDomainEventName.UPDATE_FIELDS);
        verify(this.kafkaEventLoggerService).insertKafkaEventLogger(STORE_ID, TIMESTAMP, BP_CODE,
            BusinessPartnerDomainEventName.UPDATE_FIELDS,
            BusinessPartnerDomainEventName.UPDATE_FIELDS, MESSAGE);
        verify(kafkaEventLoggerService).updateKafkaEventToFinished(any());
    }

    @Test
    public void onDomainEventConsumedCncDeactivatedChanged_success() throws Exception {
        Set<String> changeFields = new HashSet<>();
        changeFields.add(CompanyChangeFields.CNC_ACTIVATED.name());
        businessPartnerChange.getCompany().setChangedFields(changeFields);

        businessPartnerChange.getCompany().setCncActivated(Boolean.FALSE);
        Mockito.when(this.objectMapper.readValue(MESSAGE, BusinessPartnerChange.class))
          .thenReturn(businessPartnerChange);
        Mockito.when(this.kafkaEventLoggerService.findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(STORE_ID, TIMESTAMP,
            BP_CODE, BusinessPartnerDomainEventName.UPDATE_FIELDS)).thenReturn(null);
        Mockito.when(this.kafkaEventLoggerService.insertKafkaEventLogger(STORE_ID, TIMESTAMP, BP_CODE,
            BusinessPartnerDomainEventName.UPDATE_FIELDS, BusinessPartnerDomainEventName.UPDATE_FIELDS,
            MESSAGE)).thenReturn(kafkaEventLogger);
        listener.onDomainEventConsumed(MESSAGE);
        Mockito.verify(this.objectMapper).readValue(MESSAGE, BusinessPartnerChange.class);
        verify(this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(STORE_ID, TIMESTAMP,
            BP_CODE,BusinessPartnerDomainEventName.UPDATE_FIELDS);
        verify(this.kafkaEventLoggerService).insertKafkaEventLogger(STORE_ID, TIMESTAMP,BP_CODE,
            BusinessPartnerDomainEventName.UPDATE_FIELDS,BusinessPartnerDomainEventName.UPDATE_FIELDS, MESSAGE);
        verify(kafkaEventLoggerService).updateKafkaEventToFinished(Mockito.isNull());
    }

    @Test
    public void onDomainEventConsumedCncDeactivatedChanged_NonNullKafkaLog() throws Exception {
        Set<String> changeFields = new HashSet<>();
        changeFields.add(CompanyChangeFields.CNC_ACTIVATED.name());
        businessPartnerChange.getCompany().setChangedFields(changeFields);
        businessPartnerChange.getCompany().setCncActivated(Boolean.FALSE);
        Mockito.when(this.objectMapper.readValue(MESSAGE, BusinessPartnerChange.class))
            .thenReturn(businessPartnerChange);
        Mockito.when(this.kafkaEventLoggerService.findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(STORE_ID, TIMESTAMP,
            BP_CODE, BusinessPartnerDomainEventName.UPDATE_FIELDS)).thenReturn(kafkaEventLogger);
        Mockito.when(this.kafkaEventLoggerService.insertKafkaEventLogger(STORE_ID, TIMESTAMP, BP_CODE,
            BusinessPartnerDomainEventName.UPDATE_FIELDS, BusinessPartnerDomainEventName.UPDATE_FIELDS,
            MESSAGE)).thenReturn(kafkaEventLogger);
        listener.onDomainEventConsumed(MESSAGE);
        Mockito.verify(this.objectMapper).readValue(MESSAGE, BusinessPartnerChange.class);
        verify(this.kafkaEventLoggerService).findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(STORE_ID, TIMESTAMP,
            BP_CODE,BusinessPartnerDomainEventName.UPDATE_FIELDS);
    }
}
