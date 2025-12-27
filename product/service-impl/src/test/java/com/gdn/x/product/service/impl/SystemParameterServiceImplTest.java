package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.dao.api.SystemParameterRepository;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.SystemParameterHistoryService;

;

public class SystemParameterServiceImplTest {

  private static final String BLANK = "";

  private static final String DESCRIPTION = "description";

  private static final String VALUE = "value";

  private static final String VALUE_2 = "value_2";

  private static final String VARIABLE = "variable";

  private static final String VARIABLE_NOT_FOUND = "variable_not_found";

  private static final String STORE_ID = "store_id";

  @InjectMocks
  private SystemParameterServiceImpl paramServiceImpl;

  @Mock
  private SystemParameterRepository paramRepo;

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  @Mock
  private SystemParameterHistoryService paramHistoryService;

  private SystemParameter systemParameter;

  private SystemParameter emptySysParameter;

  private SystemParameter systemParameterWithNotFoundVariable;

  private SystemParameter systemParameterWithUpdateValue;

  private List<SystemParameter> systemParameters;

  @Test
  public void deleteWithBlankStoreId() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.paramServiceImpl.delete(SystemParameterServiceImplTest.BLANK,
        SystemParameterServiceImplTest.VARIABLE));
  }

  @Test
  public void deleteWithBlankVariable() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.paramServiceImpl.delete(SystemParameterServiceImplTest.STORE_ID,
        SystemParameterServiceImplTest.BLANK));
  }

  @Test
  public void deleteWithCorrectParameter() {
    this.paramServiceImpl.delete(SystemParameterServiceImplTest.STORE_ID, SystemParameterServiceImplTest.VARIABLE);
    verify(this.paramRepo).deleteByStoreIdAndVariable(SystemParameterServiceImplTest.STORE_ID,
        SystemParameterServiceImplTest.VARIABLE);
    verify(this.paramHistoryService).saveHistoryDelete(this.systemParameter);
    verify(this.cacheEvictHelperService).flushRedisDBByJedisConnectionFactory(
        Mockito.isNull());
  }

  @Test
  public void deleteWithNotFoundVariable() {
    boolean success = true;
    try {
      this.paramServiceImpl.delete(SystemParameterServiceImplTest.STORE_ID,
          SystemParameterServiceImplTest.VARIABLE_NOT_FOUND);
    } catch (ApplicationRuntimeException e) {
      success = false;
    }
    verify(this.paramRepo).deleteByStoreIdAndVariable(SystemParameterServiceImplTest.STORE_ID,
        SystemParameterServiceImplTest.VARIABLE_NOT_FOUND);
    assertEquals(success, (false));
  }

  @Test
  public void findAllWithBlankStoreId() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.paramServiceImpl.findAll(SystemParameterServiceImplTest.BLANK));
  }

  @Test
  public void findAllWithCorrectStoreId() {
    this.paramServiceImpl.findAll(SystemParameterServiceImplTest.STORE_ID);
    verify(this.paramRepo).findAllByStoreId(SystemParameterServiceImplTest.STORE_ID);
  }

  @Test
  public void findWithCorrectStoreIdAndVariable() {
    this.paramServiceImpl.findValueByStoreIdAndVariable(SystemParameterServiceImplTest.STORE_ID,
        SystemParameterServiceImplTest.VARIABLE);
    verify(this.paramRepo).findByStoreIdAndVariable(SystemParameterServiceImplTest.STORE_ID,
        SystemParameterServiceImplTest.VARIABLE);
  }

  @Test
  public void findWithNotFoundVariable() {
    try {
      this.paramServiceImpl.findValueByStoreIdAndVariable(SystemParameterServiceImplTest.STORE_ID,
          SystemParameterServiceImplTest.VARIABLE_NOT_FOUND);
    } catch (Exception e) {
      verify(this.paramRepo).findByStoreIdAndVariable(SystemParameterServiceImplTest.STORE_ID,
          SystemParameterServiceImplTest.VARIABLE_NOT_FOUND);
    }
  }

  @Test
  public void insertWithBlankDescription() {
    SystemParameter parameter =
        new SystemParameter(SystemParameterServiceImplTest.STORE_ID,
            SystemParameterServiceImplTest.VARIABLE, SystemParameterServiceImplTest.VALUE,
            SystemParameterServiceImplTest.BLANK);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.paramServiceImpl.insert(parameter));
  }

  @Test
  public void insertWithBlankStoreId() {
    SystemParameter parameter =
        new SystemParameter(SystemParameterServiceImplTest.BLANK,
            SystemParameterServiceImplTest.VARIABLE, SystemParameterServiceImplTest.VALUE,
            SystemParameterServiceImplTest.DESCRIPTION);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.paramServiceImpl.insert(parameter));
  }

  @Test
  public void insertWithBlankValue() {
    SystemParameter parameter =
        new SystemParameter(SystemParameterServiceImplTest.STORE_ID,
            SystemParameterServiceImplTest.VARIABLE, SystemParameterServiceImplTest.BLANK,
            SystemParameterServiceImplTest.DESCRIPTION);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.paramServiceImpl.insert(parameter));
  }

  @Test
  public void insertWithBlankVariable() {
    SystemParameter parameter =
        new SystemParameter(SystemParameterServiceImplTest.STORE_ID,
            SystemParameterServiceImplTest.BLANK, SystemParameterServiceImplTest.VALUE,
            SystemParameterServiceImplTest.DESCRIPTION);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.paramServiceImpl.insert(parameter));
  }

  @Test
  public void insertWithCorrectParameter() {
    SystemParameter parameter =
        new SystemParameter(SystemParameterServiceImplTest.STORE_ID, SystemParameterServiceImplTest.VARIABLE,
            SystemParameterServiceImplTest.VALUE, SystemParameterServiceImplTest.DESCRIPTION);
    this.paramServiceImpl.insert(parameter);
    verify(this.paramRepo).save(parameter);
    verify(this.cacheEvictHelperService).flushRedisDBByJedisConnectionFactory(
        Mockito.isNull());
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.systemParameter =
        new SystemParameter(SystemParameterServiceImplTest.STORE_ID,
            SystemParameterServiceImplTest.VARIABLE, SystemParameterServiceImplTest.VALUE,
            SystemParameterServiceImplTest.DESCRIPTION);
    this.systemParameterWithNotFoundVariable =
        new SystemParameter(SystemParameterServiceImplTest.STORE_ID,
            SystemParameterServiceImplTest.VARIABLE_NOT_FOUND,
            SystemParameterServiceImplTest.VALUE, SystemParameterServiceImplTest.DESCRIPTION);
    this.systemParameterWithUpdateValue =
        new SystemParameter(SystemParameterServiceImplTest.STORE_ID,
            SystemParameterServiceImplTest.VARIABLE, SystemParameterServiceImplTest.VALUE_2,
            SystemParameterServiceImplTest.DESCRIPTION);
    ArrayList<SystemParameter> emptySystemParameter = new ArrayList<SystemParameter>();
    this.emptySysParameter = new SystemParameter();
    this.systemParameters = new ArrayList<SystemParameter>();
    this.systemParameters.add(this.systemParameter);

    when(
        this.paramRepo.findByStoreIdAndVariable(SystemParameterServiceImplTest.STORE_ID,
            SystemParameterServiceImplTest.VARIABLE)).thenReturn(this.systemParameter);
    when(
        this.paramRepo.findByStoreIdAndVariable(SystemParameterServiceImplTest.STORE_ID,
            SystemParameterServiceImplTest.VARIABLE_NOT_FOUND)).thenReturn(null);
    when(
        this.paramRepo.deleteByStoreIdAndVariable(SystemParameterServiceImplTest.STORE_ID,
            SystemParameterServiceImplTest.VARIABLE_NOT_FOUND)).thenReturn(emptySystemParameter);
    when(
        this.paramRepo.deleteByStoreIdAndVariable(SystemParameterServiceImplTest.STORE_ID,
            SystemParameterServiceImplTest.VARIABLE)).thenReturn(this.systemParameters);
    when(this.paramRepo.findAndUpdate(this.systemParameterWithNotFoundVariable)).thenReturn(null);
    when(this.paramRepo.findAndUpdate(this.systemParameterWithUpdateValue)).thenReturn(
        this.systemParameter);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.paramRepo);
    verifyNoMoreInteractions(this.paramHistoryService);
    verifyNoMoreInteractions(this.cacheEvictHelperService);
  }

  @Test
  public void updateWithBlankDescription() {
    SystemParameter parameter =
        new SystemParameter(SystemParameterServiceImplTest.STORE_ID,
            SystemParameterServiceImplTest.VARIABLE, SystemParameterServiceImplTest.VALUE,
            SystemParameterServiceImplTest.BLANK);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.paramServiceImpl.update(parameter));
  }

  @Test
  public void updateWithBlankStoreId() {
    SystemParameter parameter =
        new SystemParameter(SystemParameterServiceImplTest.BLANK,
            SystemParameterServiceImplTest.VARIABLE, SystemParameterServiceImplTest.VALUE,
            SystemParameterServiceImplTest.DESCRIPTION);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.paramServiceImpl.update(parameter));
  }

  @Test
  public void updateWithBlankValue() {
    SystemParameter parameter =
        new SystemParameter(SystemParameterServiceImplTest.STORE_ID,
            SystemParameterServiceImplTest.VARIABLE, SystemParameterServiceImplTest.BLANK,
            SystemParameterServiceImplTest.DESCRIPTION);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.paramServiceImpl.update(parameter));
  }


  @Test
  public void updateWithBlankVariable() {
    SystemParameter parameter =
        new SystemParameter(SystemParameterServiceImplTest.STORE_ID,
            SystemParameterServiceImplTest.BLANK, SystemParameterServiceImplTest.VALUE,
            SystemParameterServiceImplTest.DESCRIPTION);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.paramServiceImpl.update(parameter));
  }

  @Test
  public void updateWithCorrectParameter() {
    this.paramServiceImpl.update(this.systemParameterWithUpdateValue);
    verify(this.paramRepo).findAndUpdate(this.systemParameterWithUpdateValue);
    verify(this.paramHistoryService).saveHistoryUpdate(this.systemParameter);
    verify(this.cacheEvictHelperService).flushRedisDBByJedisConnectionFactory(
        Mockito.isNull());
  }

  @Test
  public void updateWithNotFoundVariable() {
    boolean success = true;
    try {
      this.paramServiceImpl.update(this.systemParameterWithNotFoundVariable);
    } catch (ApplicationRuntimeException e) {
      success = false;
    }
    assertEquals(success, false);
    verify(this.paramRepo).findAndUpdate(this.systemParameterWithNotFoundVariable);
  }

}
