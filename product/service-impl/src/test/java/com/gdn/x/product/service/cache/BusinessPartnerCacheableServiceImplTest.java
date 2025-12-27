package com.gdn.x.product.service.cache;

import static org.mockito.MockitoAnnotations.openMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.x.product.dao.api.BusinessPartnerRepository;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.BusinessPartner;

;

public class BusinessPartnerCacheableServiceImplTest {

  private static final String BP_CODE = "bp-1";

  private BusinessPartner businessPartner;

  @InjectMocks
  private BusinessPartnerCacheableServiceImpl businessPartnerCacheableService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    businessPartner = new BusinessPartner();
    businessPartner.setBusinessPartnerCode(BP_CODE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(businessPartnerRepository);
  }

  @Test
  public void findByStoreIdAndBusinessPartnerCode() {
    Mockito.when(businessPartnerRepository.findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BP_CODE))
        .thenReturn(businessPartner);
    BusinessPartner response =
        businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BP_CODE);
    Mockito.verify(businessPartnerRepository).findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BP_CODE);
    Assertions.assertEquals(BP_CODE, response.getBusinessPartnerCode());
  }
}