package com.gdn.x.product.service.impl;

import static org.mockito.MockitoAnnotations.openMocks;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.x.product.dao.api.BusinessPartnerPromoRepository;
import com.gdn.x.product.model.entity.BusinessPartnerPromo;


public class BusinessPartnerPromoServiceImplTest {

  public static final String WHOLESALE = "WHOLESALE";
  public static final String STORE_ID = "10001";
  public static final String BP_CODE = "BP_CODE";

  @InjectMocks
  private BusinessPartnerPromoServiceImpl businessPartnerPromoService;

  @Mock
  private BusinessPartnerPromoRepository businessPartnerPromoRepository;

  @Captor
  private ArgumentCaptor<BusinessPartnerPromo> businessPartnerPromoArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    Mockito.when(businessPartnerPromoRepository.findByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(new BusinessPartnerPromo());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(businessPartnerPromoRepository);
  }

  @Test
  public void testUpsertBusinessPartnerPromo() {
    Mockito.when(businessPartnerPromoRepository.findByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(new BusinessPartnerPromo(BP_CODE, new HashSet<>(Arrays.asList(BP_CODE))));
    businessPartnerPromoService.upsertBusinessPartnerPromo(STORE_ID, true, WHOLESALE, BP_CODE);
    Mockito.verify(businessPartnerPromoRepository).findByBusinessPartnerCode(BP_CODE);
    Mockito.verify(businessPartnerPromoRepository).save(businessPartnerPromoArgumentCaptor.capture());
   Assertions.assertEquals(businessPartnerPromoArgumentCaptor.getValue().getActivePromoBundlings().size(), 2);
  }

  @Test
  public void testUpsertBusinessPartnerNullPromo() {
    Mockito.when(businessPartnerPromoRepository.findByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(null);
    businessPartnerPromoService.upsertBusinessPartnerPromo(STORE_ID, true, WHOLESALE, BP_CODE);
    Mockito.verify(businessPartnerPromoRepository).findByBusinessPartnerCode(BP_CODE);
    Mockito.verify(businessPartnerPromoRepository).save(businessPartnerPromoArgumentCaptor.capture());
   Assertions.assertEquals(businessPartnerPromoArgumentCaptor.getValue().getActivePromoBundlings().size(), 1);
  }

  @Test
  public void findByBusinessPartnerCodeTest() {
    Mockito.when(this.businessPartnerPromoRepository.findByBusinessPartnerCode(BP_CODE))
        .thenReturn(new BusinessPartnerPromo());
    this.businessPartnerPromoService.findByBusinessPartnerCode(BP_CODE);
    Mockito.verify(this.businessPartnerPromoRepository).findByBusinessPartnerCode(BP_CODE);
  }

  @Test
  public void testUpsertBusinessPartnerDeactivatePromo() {
    Mockito.when(businessPartnerPromoRepository.findByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(new BusinessPartnerPromo(BP_CODE, new HashSet<>(Arrays.asList(BP_CODE, WHOLESALE))));
    businessPartnerPromoService.upsertBusinessPartnerPromo(STORE_ID, false, WHOLESALE, BP_CODE);
    Mockito.verify(businessPartnerPromoRepository).findByBusinessPartnerCode(BP_CODE);
    Mockito.verify(businessPartnerPromoRepository).save(businessPartnerPromoArgumentCaptor.capture());
   Assertions.assertEquals(businessPartnerPromoArgumentCaptor.getValue().getActivePromoBundlings().size(), 1);
  }

  @Test
  public void testUpsertBusinessPartnerNullDeactivatePromo() {
    Mockito.when(businessPartnerPromoRepository.findByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(null);
    businessPartnerPromoService.upsertBusinessPartnerPromo(STORE_ID, false, WHOLESALE, BP_CODE);
    Mockito.verify(businessPartnerPromoRepository).findByBusinessPartnerCode(BP_CODE);
    Mockito.verify(businessPartnerPromoRepository).save(businessPartnerPromoArgumentCaptor.capture());
   Assertions.assertEquals(businessPartnerPromoArgumentCaptor.getValue().getActivePromoBundlings().size(), 0);
  }

  @Test
  public void findByStoreIdAndBusinessPartnerListTest() {
    Mockito.when(this.businessPartnerPromoRepository.findByStoreIdAndBusinessPartnerCodeIn(STORE_ID,
        Collections.singletonList(BP_CODE)))
      .thenReturn(Collections.singletonList(new BusinessPartnerPromo()));
    this.businessPartnerPromoService.findByStoreIdAndBusinessPartnerList(STORE_ID,
      Collections.singletonList(BP_CODE));
    Mockito.verify(this.businessPartnerPromoRepository).findByStoreIdAndBusinessPartnerCodeIn(STORE_ID,
      Collections.singletonList(BP_CODE));
  }

  @Test
  public void findByStoreIdAndBusinessPartnerList_emptyInputTest() {
    List<BusinessPartnerPromo> response =
      this.businessPartnerPromoService.findByStoreIdAndBusinessPartnerList(STORE_ID,
        Collections.emptyList());
   Assertions.assertTrue(CollectionUtils.isEmpty(response));
  }
}