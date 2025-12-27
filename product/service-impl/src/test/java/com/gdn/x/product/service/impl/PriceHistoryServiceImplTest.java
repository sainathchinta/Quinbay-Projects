package com.gdn.x.product.service.impl;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.x.product.dao.api.PriceHistoryRepository;
import com.gdn.x.product.model.entity.PriceHistory;

public class PriceHistoryServiceImplTest {

  public static final String REQUEST_ID = "request-id";

  @InjectMocks
  private PriceHistoryServiceImpl priceHistoryServiceImpl;

  @Mock
  private PriceHistoryRepository priceHistoryRepository;

  private PriceHistory priceHistory;

  @Test
  public void savePriceHistoryTest() {
    PriceHistory result = this.priceHistoryServiceImpl.savePriceHistory(this.priceHistory);

    verify(this.priceHistoryRepository).save(this.priceHistory);

    Assertions.assertEquals(result, this.priceHistory);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    this.priceHistory = new PriceHistory();

    when(this.priceHistoryRepository.save(this.priceHistory)).thenReturn(this.priceHistory);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.priceHistoryRepository);
  }
}
