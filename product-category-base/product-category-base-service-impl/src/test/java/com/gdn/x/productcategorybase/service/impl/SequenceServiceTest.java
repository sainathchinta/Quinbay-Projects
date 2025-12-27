package com.gdn.x.productcategorybase.service.impl;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.x.productcategorybase.repository.sequence.SequenceRepository;

public class SequenceServiceTest {

  private static final String CODE_STRING = "0001";
  private static final Long CODE_LONG = 1L;

  @Mock
  private SequenceRepository sequenceRepository;

  @InjectMocks
  private SequenceServiceBean sequenceServiceBean;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.sequenceRepository);
  }

  @Test
  public void findByCodeTest() {
    Mockito.when(this.sequenceRepository.findByCode(CODE_STRING)).thenReturn(CODE_LONG);
    Long response = sequenceServiceBean.findByCode(CODE_STRING);
    Mockito.verify(this.sequenceRepository).findByCode(CODE_STRING);
    Assertions.assertEquals(CODE_LONG, response);
  }
}
