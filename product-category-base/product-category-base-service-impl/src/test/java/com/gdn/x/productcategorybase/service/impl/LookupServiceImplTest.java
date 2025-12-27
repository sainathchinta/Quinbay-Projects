package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.entity.Lookup;
import com.gdn.x.productcategorybase.repository.LookupRepository;

public class LookupServiceImplTest {

  private static final String LOOKUP_GROUP = "lookupGroup";

  @Mock
  private LookupRepository lookupRepository;

  @InjectMocks
  private LookupServiceImpl lookupService;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getLookupByLookupGroupTest() throws Exception {
    Mockito.when(this.lookupRepository.findByLookupGroupAndMarkForDeleteFalse(LOOKUP_GROUP))
        .thenReturn(new ArrayList<>());
    List<Lookup> response = this.lookupService.getLookupByLookupGroup(LOOKUP_GROUP);
    Mockito.verify(this.lookupRepository).findByLookupGroupAndMarkForDeleteFalse(LOOKUP_GROUP);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getLookupByLookupGroupExceptionTest() throws Exception {
    try {
      this.lookupService.getLookupByLookupGroup(StringUtils.EMPTY);
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(ErrorMessage.LOOKUP_GROUP_MUST_NOT_BE_BLANK.getMessage()));
    }
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.lookupRepository);
  }
}
