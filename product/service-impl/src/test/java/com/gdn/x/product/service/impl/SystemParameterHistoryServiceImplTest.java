package com.gdn.x.product.service.impl;

import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.Date;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.x.product.dao.api.SystemParameterHistoryRepository;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.entity.SystemParameterHistory;

public class SystemParameterHistoryServiceImplTest {

  private static final String DESCRIPTION = "description";

  private static final String VALUE = "value";

  private static final String VARIABLE = "variable";

  private static final String STORE_ID = "store_id";

  @InjectMocks
  private SystemParameterHistoryServiceImpl paramHistoryServiceImpl;

  @Mock
  private SystemParameterHistoryRepository paramHistoryRepo;

  @Test
  public void saveSystemParameterHistoryDelete() {
    SystemParameter parameter = new SystemParameter(STORE_ID, VARIABLE, VALUE, DESCRIPTION);
    this.paramHistoryServiceImpl.saveHistoryDelete(parameter);
    SystemParameterHistory history =
        new SystemParameterHistory(STORE_ID, VARIABLE, VALUE, DESCRIPTION);
    verify(this.paramHistoryRepo).save(history);
  }

  @Test
  public void saveSystemParameterHistoryUpdate() {
    SystemParameter parameter = new SystemParameter(STORE_ID, VARIABLE, VALUE, DESCRIPTION);
    parameter.setId("id");
    parameter.setCreatedDate(new Date());
    parameter.setCreatedBy("user");

    this.paramHistoryServiceImpl.saveHistoryUpdate(parameter);
    SystemParameterHistory history = new SystemParameterHistory(parameter);
    assertNull(history.getId());
    verify(this.paramHistoryRepo).save(history);
  }


  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(paramHistoryRepo);
  }

}
