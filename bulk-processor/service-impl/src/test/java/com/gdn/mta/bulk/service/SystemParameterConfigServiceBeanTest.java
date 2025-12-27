package com.gdn.mta.bulk.service;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.SystemParameterConfigRequest;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.repository.SystemParameterConfigRepository;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import java.util.Collections;
import java.util.Map;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

public class SystemParameterConfigServiceBeanTest {

  private static final String BLANK = "";
  private static final String DESCRIPTION = "description";
  private static final String VALUE = "value";
  private static final String VALUE_2 = "value_2";
  private static final String VARIABLE = "variable";
  private static final String VARIABLE_NOT_FOUND = "variable_not_found";
  private static final String STORE_ID = "store_id";
  private static final String USERNAME = "username";

  @InjectMocks
  private SystemParameterConfigServiceBean systemParameterConfigServiceBean;

  @Mock
  private SystemParameterConfigRepository systemParameterConfigRepository;

  @Mock
  private SystemParameterConfigHistoryService systemParameterConfigHistoryService;

  @Captor
  private ArgumentCaptor<SystemParameterConfig> systemParameterConfigArgumentCaptor;

  private SystemParameterConfig systemParameterConfig;
  private SystemParameterConfig systemParameterConfigUpdated;
  private SystemParameterConfigRequest systemParameterWithNotFoundVariable;
  private SystemParameterConfigRequest systemParameterWithUpdateValue;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    systemParameterConfig = new SystemParameterConfig(VARIABLE, VALUE, DESCRIPTION);
    systemParameterConfig.setStoreId(STORE_ID);
    systemParameterWithNotFoundVariable = new SystemParameterConfigRequest(VARIABLE_NOT_FOUND, VALUE, DESCRIPTION);
    systemParameterWithUpdateValue = new SystemParameterConfigRequest(VARIABLE, VALUE_2, DESCRIPTION);
    systemParameterConfigUpdated = new SystemParameterConfig(VARIABLE, VALUE_2, DESCRIPTION);
    when(systemParameterConfigRepository.findByStoreIdAndVariable(STORE_ID, VARIABLE)).thenReturn(systemParameterConfig);
    when(systemParameterConfigRepository.findByStoreIdAndVariable(STORE_ID, VARIABLE_NOT_FOUND)).thenReturn(null);
    when(systemParameterConfigRepository.deleteByStoreIdAndVariable(STORE_ID, VARIABLE)).thenReturn(systemParameterConfig);
    when(systemParameterConfigRepository.save(systemParameterConfig)).thenReturn(systemParameterConfig);
    when(systemParameterConfigRepository.save(systemParameterConfigUpdated)).thenReturn(systemParameterConfigUpdated);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(systemParameterConfigRepository);
    verifyNoMoreInteractions(systemParameterConfigHistoryService);
  }

  @Test
  public void deleteWithBlankStoreId() {
    Assertions.assertThrows(RuntimeException.class,
        () -> systemParameterConfigServiceBean.delete(BLANK, USERNAME, VARIABLE));
  }

  @Test
  public void deleteWithBlankVariable() {
    Assertions.assertThrows(RuntimeException.class,
        () -> systemParameterConfigServiceBean.delete(STORE_ID, USERNAME, BLANK));
  }

  @Test
  public void deleteWithCorrectParameter() {
    systemParameterConfigServiceBean.delete(STORE_ID, USERNAME, VARIABLE);
    verify(systemParameterConfigRepository).deleteByStoreIdAndVariable(STORE_ID, VARIABLE);
    verify(systemParameterConfigHistoryService).saveHistoryDelete(systemParameterConfig, USERNAME);
  }

  @Test
  public void deleteWithNotFoundVariable() {
    boolean success = true;
    try {
      systemParameterConfigServiceBean.delete(STORE_ID, USERNAME, VARIABLE_NOT_FOUND);
    } catch (ApplicationRuntimeException e) {
      success = false;
    }
    verify(systemParameterConfigRepository).deleteByStoreIdAndVariable(STORE_ID, VARIABLE_NOT_FOUND);
    Assertions.assertFalse(success);
  }

  @Test
  public void findWithCorrectStoreIdAndVariable() {
    systemParameterConfigServiceBean.findValueByStoreIdAndVariable(STORE_ID, VARIABLE);
    verify(systemParameterConfigRepository).findByStoreIdAndVariable(STORE_ID, VARIABLE);
  }

  @Test
  public void findWithNotFoundVariable() {
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> systemParameterConfigServiceBean.findValueByStoreIdAndVariable(STORE_ID, VARIABLE_NOT_FOUND));
    } finally {
      verify(systemParameterConfigRepository).findByStoreIdAndVariable(STORE_ID, VARIABLE_NOT_FOUND);
    }
  }

  @Test
  public void insertWithBlankDescription() throws Exception {
    SystemParameterConfigRequest parameter = new SystemParameterConfigRequest(VARIABLE, VALUE, BLANK);
    Assertions.assertThrows(RuntimeException.class,
        () -> systemParameterConfigServiceBean.insert(STORE_ID, USERNAME, parameter));
  }

  @Test
  public void insertWithBlankValue() throws Exception {
    SystemParameterConfigRequest parameter = new SystemParameterConfigRequest(VARIABLE, BLANK, DESCRIPTION);
    Assertions.assertThrows(RuntimeException.class,
        () -> systemParameterConfigServiceBean.insert(STORE_ID, USERNAME, parameter));
  }

  @Test
  public void insertWithBlankStoreId() throws Exception {
    SystemParameterConfigRequest parameter = new SystemParameterConfigRequest(VARIABLE, VALUE, DESCRIPTION);
    Assertions.assertThrows(RuntimeException.class,
        () -> systemParameterConfigServiceBean.insert(BLANK, USERNAME, parameter));
  }

  @Test
  public void insertWithBlankVariable() throws Exception {
    SystemParameterConfigRequest parameter = new SystemParameterConfigRequest(BLANK, VALUE, DESCRIPTION);
    Assertions.assertThrows(RuntimeException.class,
        () -> systemParameterConfigServiceBean.insert(STORE_ID, USERNAME, parameter));
  }

  @Test
  public void insertWithCorrectParameter() throws Exception {
    Mockito.when(systemParameterConfigRepository.findByStoreIdAndVariable(STORE_ID, VARIABLE)).thenReturn(null);
    SystemParameterConfigRequest parameter = new SystemParameterConfigRequest(VARIABLE, VALUE, DESCRIPTION);
    systemParameterConfigServiceBean.insert(STORE_ID, USERNAME, parameter);
    verify(systemParameterConfigRepository).save(systemParameterConfigArgumentCaptor.capture());
    verify(systemParameterConfigHistoryService).saveHistoryUpdate(systemParameterConfigArgumentCaptor.capture(),
        eq(StringUtils.EMPTY));
    verify(systemParameterConfigRepository).findByStoreIdAndVariable(STORE_ID, VARIABLE);
    Assertions.assertEquals(VARIABLE, systemParameterConfigArgumentCaptor.getAllValues().get(0).getVariable());
    Assertions.assertEquals(VALUE, systemParameterConfigArgumentCaptor.getAllValues().get(0).getValue());
    Assertions.assertEquals(DESCRIPTION, systemParameterConfigArgumentCaptor.getAllValues().get(0).getDescription());
  }

  @Test
  public void insertExistingValueTest() throws Exception {
    SystemParameterConfigRequest parameter = new SystemParameterConfigRequest(VARIABLE, VALUE, DESCRIPTION);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> systemParameterConfigServiceBean.insert(STORE_ID, USERNAME, parameter));
    }finally {
      verify(systemParameterConfigRepository).findByStoreIdAndVariable(STORE_ID, VARIABLE);
    }
  }

  @Test
  public void updateWithBlankValue() {
    SystemParameterConfigRequest parameter = new SystemParameterConfigRequest(VARIABLE, BLANK, DESCRIPTION);
    Assertions.assertThrows(RuntimeException.class,
        () -> systemParameterConfigServiceBean.update(STORE_ID, USERNAME, parameter));
  }

  @Test
  public void updateWithBlankStoreId() {
    SystemParameterConfigRequest parameter = new SystemParameterConfigRequest(VARIABLE, VALUE, DESCRIPTION);
    Assertions.assertThrows(RuntimeException.class,
        () -> systemParameterConfigServiceBean.update(BLANK, USERNAME, parameter));
  }


  @Test
  public void updateWithBlankVariable() {
    SystemParameterConfigRequest parameter =
        new SystemParameterConfigRequest(BLANK, VALUE, DESCRIPTION);
    Assertions.assertThrows(RuntimeException.class,
        () -> systemParameterConfigServiceBean.update(STORE_ID, USERNAME, parameter));
  }

  @Test
  public void updateWithCorrectParameter() {
    systemParameterConfigServiceBean.update(STORE_ID, USERNAME, systemParameterWithUpdateValue);
    verify(systemParameterConfigRepository).findByStoreIdAndVariable(STORE_ID, VARIABLE);
    verify(systemParameterConfigRepository).save(Mockito.any(SystemParameterConfig.class));
    verify(systemParameterConfigHistoryService).saveHistoryUpdate(Mockito.any(SystemParameterConfig.class), Mockito.eq(VALUE));
    Assertions.assertEquals(VALUE_2, systemParameterConfigUpdated.getValue());
    Assertions.assertEquals(VARIABLE, systemParameterConfigUpdated.getVariable());
  }

  @Test
  public void updateWithNotFoundVariable() {
    boolean success = true;
    try {
      systemParameterConfigServiceBean.update(STORE_ID, USERNAME, systemParameterWithNotFoundVariable);
    } catch (ApplicationRuntimeException e) {
      success = false;
    }
    Assertions.assertFalse(success);
    verify(systemParameterConfigRepository).findByStoreIdAndVariable(STORE_ID, VARIABLE_NOT_FOUND);
  }

  @Test
  public void findValueByStoreIdAndVariablesTest() {
    Mockito.when(this.systemParameterConfigRepository.findByStoreIdAndVariableIn(eq(STORE_ID),
      eq(Collections.singletonList(VARIABLE)))).thenReturn(Collections.singletonList(systemParameterConfig));
    Map<String, SystemParameterConfig> valueByStoreIdAndVariables =
      systemParameterConfigServiceBean.findValueByStoreIdAndVariables(STORE_ID,
        Collections.singletonList(VARIABLE));
    Mockito.verify(this.systemParameterConfigRepository).findByStoreIdAndVariableIn(eq(STORE_ID),
      eq(Collections.singletonList(VARIABLE)));
    Assertions.assertEquals(systemParameterConfig, valueByStoreIdAndVariables.get(VARIABLE));
  }

  @Test
  public void findValueByStoreIdAndVariables_emptyStoreIdTest() {
    Assertions.assertThrows(RuntimeException.class,
        () -> systemParameterConfigServiceBean.findValueByStoreIdAndVariables(StringUtils.EMPTY,
            Collections.singletonList(VARIABLE)));
  }

  @Test
  public void findValueByStoreIdAndVariables_emptyVariableTest() {
    Assertions.assertThrows(RuntimeException.class,
        () -> systemParameterConfigServiceBean.findValueByStoreIdAndVariables(STORE_ID,
            Collections.emptyList()));
  }

  @Test
  public void findValueByStoreIdAndVariables_emptyResultTest() {
    Mockito.when(this.systemParameterConfigRepository.findByStoreIdAndVariableIn(eq(STORE_ID),
      eq(Collections.singletonList(VARIABLE)))).thenReturn(Collections.emptyList());
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> systemParameterConfigServiceBean.findValueByStoreIdAndVariables(STORE_ID,
              Collections.singletonList(VARIABLE)));
    } finally {
      Mockito.verify(this.systemParameterConfigRepository)
        .findByStoreIdAndVariableIn(eq(STORE_ID), eq(Collections.singletonList(VARIABLE)));
    }
  }
}
