package com.gdn.x.mta.distributiontask.service.impl;

import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.x.mta.distributiontask.dao.api.AutoQcConfigChangeRepository;
import com.gdn.x.mta.distributiontask.model.AutoQcConfigChange;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class AutoQcConfigChangeServiceTest {
  private String STORE_ID = "10001";
  private final String STATUS = "PENDING";
  private static final String SELLER_CODE = "code";
  private static final String C1_CODE = "c1Code";
  private int LIMIT = 10;

  @InjectMocks
  private AutoQcConfigChangeServiceImpl autoQcConfigChangeService;

  @Mock
  private AutoQcConfigChangeRepository autoQcConfigChangeRepository;

  @BeforeEach
  public void setUp() {
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(autoQcConfigChangeRepository);
  }

  @Test
   void fetchAutoQcConfigChangesByStatusTest() {
    Mockito.when(autoQcConfigChangeRepository.findByStoreIdAndStatus(STORE_ID, STATUS, LIMIT))
        .thenReturn(List.of(new AutoQcConfigChange()));
    autoQcConfigChangeService.fetchAutoQcConfigChangesByStatus(STORE_ID, STATUS, LIMIT);
    Mockito.verify(autoQcConfigChangeRepository).findByStoreIdAndStatus(STORE_ID, STATUS, LIMIT);
  }

  @Test
   void saveAutoQcConfigChangeTest() {
    AutoQcConfigChange autoQcConfigChange = new AutoQcConfigChange();
    Mockito.when(autoQcConfigChangeRepository.save(autoQcConfigChange)).thenReturn(autoQcConfigChange);
    autoQcConfigChangeService.saveAutoQcConfigChange(autoQcConfigChange);
    Mockito.verify(autoQcConfigChangeRepository).save(autoQcConfigChange);
  }

  @Test
   void saveAutoQcConfigChangesTest() {
    AutoQcConfigChange autoQcConfigChange = new AutoQcConfigChange();
    Mockito.when(autoQcConfigChangeRepository.saveAll(List.of(autoQcConfigChange)))
        .thenReturn(List.of(autoQcConfigChange));
    autoQcConfigChangeService.saveAutoQcConfigChanges(List.of(autoQcConfigChange));
    Mockito.verify(autoQcConfigChangeRepository).saveAll(List.of(autoQcConfigChange));
  }

  @Test
   void findBySellerCodeAndC1CategoryCodeAndStatusTest() {
    autoQcConfigChangeService.findBySellerCodeAndC1CategoryCodeAndStatus(SELLER_CODE, C1_CODE, STATUS);
    Mockito.verify(autoQcConfigChangeRepository)
        .findBySellerCodeAndC1CategoryCodeAndStatus(SELLER_CODE, C1_CODE, STATUS);
  }

}
