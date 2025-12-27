package com.gdn.mta.bulk.service;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.entity.SystemParameterConfigHistory;
import com.gdn.mta.bulk.repository.SystemParameterConfigHistoryRepository;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

public class SystemParameterConfigHistoryServiceBeanTest {

  private static final String DESCRIPTION = "description";
  private static final String VALUE = "value";
  private static final String VALUE_UPDATED = "valueUpdated";
  private static final String VARIABLE = "variable";
  private static final String STORE_ID = "store_id";
  private static final String DELETED_BY = "deleted_by";

  @InjectMocks
  private SystemParameterConfigHistoryServiceBean systemParameterConfigHistoryServiceBean;

  @Mock
  private SystemParameterConfigHistoryRepository systemParameterConfigHistoryRepository;

  @Captor
  private ArgumentCaptor<SystemParameterConfigHistory> systemParameterConfigHistoryArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(systemParameterConfigHistoryRepository);
  }

  @Test
  public void saveSystemParameterHistoryDelete() {
    SystemParameterConfig parameter = new SystemParameterConfig(VARIABLE, VALUE, DESCRIPTION);
    parameter.setStoreId(SystemParameterConfigHistoryServiceBeanTest.STORE_ID);
    this.systemParameterConfigHistoryServiceBean.saveHistoryDelete(parameter, DELETED_BY);
    verify(this.systemParameterConfigHistoryRepository).save(systemParameterConfigHistoryArgumentCaptor.capture());
    Assertions.assertEquals(VARIABLE, systemParameterConfigHistoryArgumentCaptor.getValue().getVariable());
    Assertions.assertEquals(VALUE, systemParameterConfigHistoryArgumentCaptor.getValue().getOldValue());
    Assertions.assertEquals(null, systemParameterConfigHistoryArgumentCaptor.getValue().getNewValue());
    Assertions.assertEquals(DELETED_BY, systemParameterConfigHistoryArgumentCaptor.getValue().getCreatedBy());
  }

  @Test
  public void saveSystemParameterHistoryUpdate() {
    SystemParameterConfig parameterUpdated = new SystemParameterConfig(VARIABLE, VALUE_UPDATED, DESCRIPTION);
    parameterUpdated.setStoreId(SystemParameterConfigHistoryServiceBeanTest.STORE_ID);
    this.systemParameterConfigHistoryServiceBean.saveHistoryUpdate(parameterUpdated, VALUE);
    verify(this.systemParameterConfigHistoryRepository).save(systemParameterConfigHistoryArgumentCaptor.capture());
    Assertions.assertEquals(VARIABLE, systemParameterConfigHistoryArgumentCaptor.getValue().getVariable());
    Assertions.assertEquals(VALUE, systemParameterConfigHistoryArgumentCaptor.getValue().getOldValue());
    Assertions.assertEquals(VALUE_UPDATED, systemParameterConfigHistoryArgumentCaptor.getValue().getNewValue());
  }

}
