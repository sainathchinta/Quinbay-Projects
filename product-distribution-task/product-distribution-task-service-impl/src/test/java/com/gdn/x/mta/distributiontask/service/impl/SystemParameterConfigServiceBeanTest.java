package com.gdn.x.mta.distributiontask.service.impl;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.dao.api.SystemParameterConfigRepository;
import com.gdn.x.mta.distributiontask.model.SystemParameterConfig;
import com.gdn.x.mta.distributiontask.request.SystemParameterConfigRequest;
import com.gdn.x.mta.distributiontask.service.api.SystemParameterConfigHistoryService;
import joptsimple.internal.Strings;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;
import java.util.Map;

@ExtendWith(MockitoExtension.class)
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

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @Mock
  private KafkaPublisher kafkaPublisher;

  @Mock
  private ApplicationContext applicationContext;


  private SystemParameterConfig systemParameterConfig;
  private SystemParameterConfig systemParameterConfigUpdated;
  private SystemParameterConfigRequest systemParameterWithNotFoundVariable;
  private SystemParameterConfigRequest systemParameterWithUpdateValue;

  @BeforeEach
  public void setUp() throws Exception {
    systemParameterConfig = new SystemParameterConfig(VARIABLE, VALUE, DESCRIPTION);
    systemParameterConfig.setStoreId(STORE_ID);
    systemParameterWithNotFoundVariable = new SystemParameterConfigRequest(VARIABLE_NOT_FOUND, VALUE, DESCRIPTION);
    systemParameterWithUpdateValue = new SystemParameterConfigRequest(VARIABLE, VALUE_2, DESCRIPTION);
    systemParameterConfigUpdated = new SystemParameterConfig(VARIABLE, VALUE_2, DESCRIPTION);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(systemParameterConfigRepository);
    verifyNoMoreInteractions(systemParameterConfigHistoryService);
  }

  @Test
   void deleteWithBlankStoreId() {
    Assertions.assertThrows(Exception.class,
      () -> systemParameterConfigServiceBean.delete(BLANK, USERNAME, VARIABLE));
  }

  @Test
   void deleteWithBlankVariable() {
    Assertions.assertThrows(Exception.class,
      () -> systemParameterConfigServiceBean.delete(STORE_ID, USERNAME, BLANK));
  }

  @Test
   void deleteWithCorrectParameter() {
    when(systemParameterConfigRepository.findByStoreIdAndVariable(STORE_ID, VARIABLE)).thenReturn(
        systemParameterConfig);
    systemParameterConfigServiceBean.delete(STORE_ID, USERNAME, VARIABLE);
    verify(systemParameterConfigRepository).findByStoreIdAndVariable(STORE_ID, VARIABLE);
    verify(systemParameterConfigHistoryService).saveHistoryDelete(systemParameterConfig, USERNAME);
    verify(systemParameterConfigRepository).deleteById(systemParameterConfig.getId());
  }

  @Test
  void deleteWithCorrectParameterAndRedisEnabledTest() {
    ReflectionTestUtils.setField(systemParameterConfigServiceBean, "redisCacheEnabled", true);
    when(systemParameterConfigRepository.findByStoreIdAndVariable(STORE_ID, VARIABLE)).thenReturn(
        systemParameterConfig);
    Mockito.when(applicationContext.getBean(SystemParameterConfigServiceBean.class))
        .thenReturn(systemParameterConfigServiceBean);
    systemParameterConfigServiceBean.delete(STORE_ID, USERNAME, VARIABLE);
    verify(systemParameterConfigRepository).findByStoreIdAndVariable(STORE_ID, VARIABLE);
    verify(systemParameterConfigHistoryService).saveHistoryDelete(systemParameterConfig, USERNAME);
    verify(systemParameterConfigRepository).deleteById(systemParameterConfig.getId());
  }

  @Test
   void deleteWithNotFoundVariable() {
    boolean success = true;
    try {
      systemParameterConfigServiceBean.delete(STORE_ID, USERNAME, VARIABLE_NOT_FOUND);
    } catch (ApplicationRuntimeException e) {
      success = false;
    }
    verify(systemParameterConfigRepository).findByStoreIdAndVariable(STORE_ID, VARIABLE_NOT_FOUND);
    MatcherAssert.assertThat(success, equalTo(false));
  }

  @Test
   void findWithCorrectStoreIdAndVariable() {
    when(systemParameterConfigRepository.findByStoreIdAndVariable(STORE_ID, VARIABLE)).thenReturn(
        systemParameterConfig);
    systemParameterConfigServiceBean.findValueByStoreIdAndVariable(STORE_ID, VARIABLE);
    verify(systemParameterConfigRepository).findByStoreIdAndVariable(STORE_ID, VARIABLE);
  }

  @Test
   void findWithNotFoundVariable() {
    Assertions.assertThrows(Exception.class,
      () -> systemParameterConfigServiceBean.findValueByStoreIdAndVariable(STORE_ID,
        VARIABLE_NOT_FOUND));
    verify(systemParameterConfigRepository).findByStoreIdAndVariable(STORE_ID, VARIABLE_NOT_FOUND);
  }

  @Test
   void insertWithBlankDescription() {
    SystemParameterConfigRequest parameter = new SystemParameterConfigRequest(VARIABLE, VALUE, BLANK);
    Assertions.assertThrows(Exception.class,
      () -> systemParameterConfigServiceBean.insert(STORE_ID, USERNAME, parameter));
  }

  @Test
   void insertWithBlankValue() {
    SystemParameterConfigRequest parameter = new SystemParameterConfigRequest(VARIABLE, BLANK, DESCRIPTION);
    Assertions.assertThrows(Exception.class,
      () -> systemParameterConfigServiceBean.insert(STORE_ID, USERNAME, parameter));
  }

  @Test
   void insertWithBlankStoreId() {
    SystemParameterConfigRequest parameter = new SystemParameterConfigRequest(VARIABLE, VALUE, DESCRIPTION);
    Assertions.assertThrows(Exception.class,
      () -> systemParameterConfigServiceBean.insert(BLANK, USERNAME, parameter));
  }

  @Test
   void insertWithBlankVariable() {
    SystemParameterConfigRequest parameter = new SystemParameterConfigRequest(BLANK, VALUE, DESCRIPTION);
    Assertions.assertThrows(Exception.class,
      () -> systemParameterConfigServiceBean.insert(STORE_ID, USERNAME, parameter));
  }

  @Test
   void insertWithCorrectParameter() {
    SystemParameterConfigRequest parameter = new SystemParameterConfigRequest(VARIABLE, VALUE, DESCRIPTION);
    systemParameterConfigServiceBean.insert(STORE_ID, USERNAME, parameter);
    verify(systemParameterConfigRepository).save(systemParameterConfigArgumentCaptor.capture());
    verify(systemParameterConfigHistoryService).saveHistoryUpdate(systemParameterConfigArgumentCaptor.capture(),
        Mockito.eq(Strings.EMPTY));
    Assertions.assertEquals(VARIABLE,
        systemParameterConfigArgumentCaptor.getAllValues().get(0).getVariable());
    Assertions.assertEquals(VALUE,
        systemParameterConfigArgumentCaptor.getAllValues().get(0).getValue());
    Assertions.assertEquals(DESCRIPTION,
        systemParameterConfigArgumentCaptor.getAllValues().get(0).getDescription());
  }

  @Test
  void insertWithCorrectParameterRedisEnabledOn() {
    ReflectionTestUtils.setField(systemParameterConfigServiceBean, "redisCacheEnabled", true);
    Mockito.when(applicationContext.getBean(SystemParameterConfigServiceBean.class))
        .thenReturn(systemParameterConfigServiceBean);
    SystemParameterConfigRequest parameter = new SystemParameterConfigRequest(VARIABLE, VALUE, DESCRIPTION);
    systemParameterConfigServiceBean.insert(STORE_ID, USERNAME, parameter);
    verify(systemParameterConfigRepository).save(systemParameterConfigArgumentCaptor.capture());
    verify(systemParameterConfigHistoryService).saveHistoryUpdate(systemParameterConfigArgumentCaptor.capture(),
        Mockito.eq(Strings.EMPTY));
    Assertions.assertEquals(VARIABLE,
        systemParameterConfigArgumentCaptor.getAllValues().get(0).getVariable());
    Assertions.assertEquals(VALUE,
        systemParameterConfigArgumentCaptor.getAllValues().get(0).getValue());
    Assertions.assertEquals(DESCRIPTION,
        systemParameterConfigArgumentCaptor.getAllValues().get(0).getDescription());
  }

  @Test
   void updateWithBlankValue() {
    SystemParameterConfigRequest parameter = new SystemParameterConfigRequest(VARIABLE, BLANK, DESCRIPTION);
    Assertions.assertThrows(Exception.class,
      () -> systemParameterConfigServiceBean.update(STORE_ID, USERNAME, parameter));
  }

  @Test
   void updateWithBlankStoreId() {
    SystemParameterConfigRequest parameter = new SystemParameterConfigRequest(VARIABLE, VALUE, DESCRIPTION);
    Assertions.assertThrows(Exception.class,
      () -> systemParameterConfigServiceBean.update(BLANK, USERNAME, parameter));
  }


  @Test
   void updateWithBlankVariable() {
    SystemParameterConfigRequest parameter =
        new SystemParameterConfigRequest(BLANK, VALUE, DESCRIPTION);
    Assertions.assertThrows(Exception.class,
      () -> systemParameterConfigServiceBean.update(STORE_ID, USERNAME, parameter));
  }

  @Test
   void updateWithCorrectParameter() {
    when(systemParameterConfigRepository.findByStoreIdAndVariable(STORE_ID, VARIABLE)).thenReturn(systemParameterConfig);
        when(systemParameterConfigRepository.save(systemParameterConfig)).thenReturn(systemParameterConfig);
    systemParameterConfigServiceBean.update(STORE_ID, USERNAME, systemParameterWithUpdateValue);
    verify(systemParameterConfigRepository).findByStoreIdAndVariable(STORE_ID, VARIABLE);
    verify(systemParameterConfigRepository).save(systemParameterConfigUpdated);
    verify(systemParameterConfigHistoryService).saveHistoryUpdate(systemParameterConfigUpdated, VALUE);
    Assertions.assertEquals(VALUE_2, systemParameterConfigUpdated.getValue());
    Assertions.assertEquals(VARIABLE, systemParameterConfigUpdated.getVariable());
  }

  @Test
  void updateWithCorrectParameterForRedisSwitchOn() {
    ReflectionTestUtils.setField(systemParameterConfigServiceBean, "redisCacheEnabled", true);
    Mockito.when(applicationContext.getBean(SystemParameterConfigServiceBean.class))
        .thenReturn(systemParameterConfigServiceBean);
    when(systemParameterConfigRepository.findByStoreIdAndVariable(STORE_ID, VARIABLE)).thenReturn(
        systemParameterConfig);
    when(systemParameterConfigRepository.save(systemParameterConfig)).thenReturn(
        systemParameterConfig);
    systemParameterConfigServiceBean.update(STORE_ID, USERNAME, systemParameterWithUpdateValue);
    verify(systemParameterConfigRepository).findByStoreIdAndVariable(STORE_ID, VARIABLE);
    verify(systemParameterConfigRepository).save(systemParameterConfigUpdated);
    verify(systemParameterConfigHistoryService).saveHistoryUpdate(systemParameterConfigUpdated,
        VALUE);
    Assertions.assertEquals(VALUE_2, systemParameterConfigUpdated.getValue());
    Assertions.assertEquals(VARIABLE, systemParameterConfigUpdated.getVariable());
  }


  @Test
   void updateWithNotFoundVariable() {
    boolean success = true;
    try {
      systemParameterConfigServiceBean.update(STORE_ID, USERNAME, systemParameterWithNotFoundVariable);
    } catch (ApplicationRuntimeException e) {
      success = false;
    }
    MatcherAssert.assertThat(success, equalTo(false));
    verify(systemParameterConfigRepository).findByStoreIdAndVariable(STORE_ID, VARIABLE_NOT_FOUND);
  }

  @Test
  public void findSwitchValuesTest() {
    Mockito.when(systemParameterConfigRepository.findByStoreIdAndMarkForDeleteFalse(STORE_ID))
      .thenReturn(Collections.singletonList(systemParameterConfig));
    Map<String, String> fetchedValue = systemParameterConfigServiceBean.findSwitchValues(STORE_ID);
    Mockito.verify(systemParameterConfigRepository).findByStoreIdAndMarkForDeleteFalse(STORE_ID);
    Assertions.assertEquals(VALUE, fetchedValue.get(VARIABLE));
  }
}
