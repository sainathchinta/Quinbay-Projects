package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.common.util.BeanUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.entity.SystemParameter;
import com.gdn.x.productcategorybase.repository.SystemParameterRepository;

public class SystemParameterServiceImplTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DESCRIPTION = "desc";
  private static final String VARIABLE = "variable";
  private static final String VALUE = "value";
  private static final String VALUE_1 = "value1";
  private static final String ID = "id";
  private static final String VARIABLE_1 = "variable1";
  private static final String VARIABLE_2 = "variable2";
  private List<String> SWITCH_VARIALBLES_LIST = new ArrayList<>();
  private SystemParameter systemParameter;
  private SystemParameter systemParameter1;


  @InjectMocks
  private SystemParameterServiceImpl systemParameterService;

  @Mock
  private SystemParameterRepository systemParameterRepository;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    systemParameter = new SystemParameter();
    systemParameter.setDescription(DESCRIPTION);
    systemParameter.setValue(VALUE);
    systemParameter.setVariable(VARIABLE);
    systemParameter.setStoreId(DEFAULT_STORE_ID);
    systemParameter.setId(ID);

    systemParameter1 = new SystemParameter();
    BeanUtils.copyProperties(systemParameter, systemParameter1);
    systemParameter1.setValue(VALUE_1);

    SWITCH_VARIALBLES_LIST.add(VARIABLE_1);
    SWITCH_VARIALBLES_LIST.add(VARIABLE_2);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(systemParameterRepository);
  }

  @Test
  public void insertTest() {
    systemParameterService.insert(systemParameter);
    Mockito.verify(systemParameterRepository).save(systemParameter);
  }

  @Test
  public void insertStoreIdBlankTest() {
    systemParameter.setStoreId(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> systemParameterService.insert(systemParameter));
  }

  @Test
  public void insertVariableBlankTest() {
    systemParameter.setVariable(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> systemParameterService.insert(systemParameter));
  }

  @Test
  public void insertDescriptionBlankTest() {
    systemParameter.setDescription(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> systemParameterService.insert(systemParameter));
  }

  @Test
  public void updateTest() {
    Mockito.when(systemParameterRepository.findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE))
        .thenReturn(systemParameter);
    systemParameterService.update(systemParameter1);
    Mockito.verify(systemParameterRepository).findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE);
    Mockito.verify(systemParameterRepository).save(systemParameter1);
  }

  @Test
  public void updateExceptionTest() {
    Mockito.when(systemParameterRepository.findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> systemParameterService.update(systemParameter1));
    } finally {
      Mockito.verify(systemParameterRepository).findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE);
    }
  }

  @Test
  public void deleteTest() {
    Mockito.when(systemParameterRepository.findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE))
        .thenReturn(systemParameter);
    systemParameterService.delete(DEFAULT_STORE_ID, VARIABLE);
    Mockito.verify(systemParameterRepository).findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE);
    Mockito.verify(systemParameterRepository).deleteById(ID);
  }

  @Test
  public void findByStoreIdAndVariableTest() {
    Mockito.when(systemParameterRepository.findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE))
        .thenReturn(systemParameter);
    SystemParameter response = systemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE);
    Mockito.verify(systemParameterRepository).findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE);
    Assertions.assertEquals(VALUE, response.getValue());
    Assertions.assertEquals(DESCRIPTION, response.getDescription());
    Assertions.assertEquals(VARIABLE, response.getVariable());
  }

  @Test
  public void findByStoreIdAndVariableNotFoundTest() {
    Mockito.when(systemParameterRepository.findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE))
        .thenThrow(ApplicationRuntimeException.class);
    try {
      this.systemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE);
    } catch (Exception e) {
      Mockito.verify(systemParameterRepository).findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE);
    }
  }
}
