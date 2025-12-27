package com.gdn.mta.product.service;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;

public class ProductInventoryServiceBeanTest {

  @InjectMocks
  private ProductInventoryServiceBean service;

  private static final String BP_CODE = "TEST";

  private ProfileResponse defaultBusinessPartner;

  public ProfileResponse createDefaultBusinessPartner() {
    ProfileResponse profile = new ProfileResponse();
    profile.setBusinessPartnerCode(BP_CODE);
    CompanyDTO company = new CompanyDTO();
    profile.setCompany(company);
    return profile;
  }

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
    defaultBusinessPartner = createDefaultBusinessPartner();
  }

  @Test
  public void testIsSyncedToLevel1InventoryReturnTrue() throws Exception {
    defaultBusinessPartner.getCompany().setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI);
    Assertions.assertTrue(service.isSyncedToLevel1Inventory(defaultBusinessPartner));
  }

  @Test
  public void testIsSyncedToLevel1InventoryReturnFalse() throws Exception {
    defaultBusinessPartner.getCompany().setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    Assertions.assertFalse(service.isSyncedToLevel1Inventory(defaultBusinessPartner));
  }
}
