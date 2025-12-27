package com.gdn.mta.product.service;

import com.gdn.mta.product.repository.SequenceRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class SequenceServiceImplTest {

  private static final String KEY = "key";
  private static final Long COUNTER = Long.valueOf(10);

  @Mock
  private SequenceRepository sequenceRepository;

  @InjectMocks
  private SequenceServiceImpl sequenceService;

  @BeforeEach
  public void init() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(sequenceRepository);
  }

  @Test
  public void findCounterByKeyTest() {
    Mockito.when(this.sequenceRepository.findByCode(KEY)).thenReturn(COUNTER);
    Long response = sequenceService.findCounterByKey(KEY);
    Mockito.verify(this.sequenceRepository).findByCode(KEY);
    Assertions.assertEquals(COUNTER, response);
  }
}