package com.gdn.x.product.service.impl;

import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerChange;
import com.gdn.x.businesspartner.domain.event.model.CompanyVO;
import com.gdn.x.product.dao.api.BusinessPartnerRepository;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.service.api.BusinessPartnerCacheableService;

public class BusinessPartnerServiceImplTest {

  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String BUSINESS_PARTNER_CODE1 = "bpCode1";
  private static final String BUSINESS_PARTNER_TYPE = "bpType";
  private static final String BUSINESS_PARTNER_NAME = "bpName";
  private static final String LINKED_BUSINESS_PARTNER = "linkedBp";
  private static final String ALIAS_BUSINESS_PARTNER = "alias";
  private static final String INVENTORY_TYPE = "inventoryType";
  private static final String STATUS = "status";
  private static final String TYPE = "type";
  private static final String MERCHANT_DELIVERY_TYPE = "merchantDeliveryType";

  private BusinessPartnerChange businessPartnerChange;
  private CompanyVO companyVO;
  private BusinessPartner businessPartner;
  private BusinessPartner businessPartner1;
  private SimpleListStringRequest businessPartnerCodes;

  @InjectMocks
  private BusinessPartnerServiceImpl businessPartnerService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private BusinessPartnerCacheableService businessPartnerCacheableService;

  @Captor
  private ArgumentCaptor<BusinessPartner> businessPartnerArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    businessPartnerChange = new BusinessPartnerChange();
    companyVO = new CompanyVO();
    businessPartnerChange.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    businessPartnerChange.setBusinessPartnerType(BUSINESS_PARTNER_TYPE);
    businessPartnerChange.setAllCategory(true);
    businessPartnerChange.setMerchantStatus(STATUS);
    businessPartnerChange.setStoreId(Constants.STORE_ID);
    companyVO.setBusinessPartnerAlias(ALIAS_BUSINESS_PARTNER);
    companyVO.setLinkedPartnerStore(LINKED_BUSINESS_PARTNER);
    companyVO.setInternationalFlag(true);
    companyVO.setUmkmFlag(true);
    companyVO.setOfflineToOnlineFlag(true);
    companyVO.setCncActivated(true);
    companyVO.setName(BUSINESS_PARTNER_NAME);
    companyVO.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    companyVO.setInventoryFulfillment(INVENTORY_TYPE);
    companyVO.setMerchantType(TYPE);
    companyVO.setSupplierFlag(true);
    companyVO.setCustomerFlag(true);
    companyVO.setMerchantFlag(true);
    companyVO.setMerchantDeliveryType(MERCHANT_DELIVERY_TYPE);
    businessPartnerChange.setCompany(companyVO);

    businessPartner = new BusinessPartner();
    businessPartner.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    businessPartner.setBusinessPartnerType(BUSINESS_PARTNER_TYPE);
    businessPartner.setAllCategory(true);
    businessPartner.setMerchantStatus(STATUS);
    businessPartner.setBusinessPartnerAlias(ALIAS_BUSINESS_PARTNER);
    businessPartner.setLinkedPartnerStore(LINKED_BUSINESS_PARTNER);
    businessPartner.setInternationalFlag(true);
    businessPartner.setUmkmFlag(true);
    businessPartner.setOfflineToOnlineFlag(true);
    businessPartner.setCncActivated(true);
    businessPartner.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    businessPartner.setInventoryFulfillment(INVENTORY_TYPE);
    businessPartner.setMerchantType(TYPE);
    businessPartner.setSupplierFlag(true);
    businessPartner.setCustomerFlag(true);
    businessPartner.setMerchantFlag(true);
    businessPartner.setMerchantDeliveryType(MERCHANT_DELIVERY_TYPE);

    businessPartner1 = new BusinessPartner();
    businessPartner1.setBusinessPartnerCode(BUSINESS_PARTNER_CODE1);
    businessPartner1.setBusinessPartnerType(BUSINESS_PARTNER_TYPE);
    businessPartner1.setAllCategory(true);

    businessPartnerCodes = new SimpleListStringRequest();
    businessPartnerCodes.setValue(Arrays.asList(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_CODE1));
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(businessPartnerRepository);
    Mockito.verifyNoMoreInteractions(businessPartnerCacheableService);
  }

  @Test
  public void updateBusinessPartner() {
    Mockito.when(
        businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(businessPartner);
    businessPartnerService.upsertBusinessPartner(businessPartnerChange);
    Mockito.verify(businessPartnerCacheableService).findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerRepository).save(businessPartnerArgumentCaptor.capture());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, businessPartnerArgumentCaptor.getValue().getBusinessPartnerCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, businessPartnerArgumentCaptor.getValue().getBusinessPartnerName());
  }

  @Test
  public void insertBusinessPartner() {
    Mockito.when(
        businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(null);
    businessPartnerService.upsertBusinessPartner(businessPartnerChange);
    Mockito.verify(businessPartnerCacheableService).findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerRepository).save(businessPartnerArgumentCaptor.capture());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, businessPartnerArgumentCaptor.getValue().getBusinessPartnerCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, businessPartnerArgumentCaptor.getValue().getBusinessPartnerName());
  }

  @Test
  public void getBusinessPartnerByBusinessPartnerCode() {
    Mockito.when(
        businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(businessPartner);
    BusinessPartner response =
        businessPartnerService.getBusinessPartnerByBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerCacheableService)
        .findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, response.getBusinessPartnerName());
  }

  @Test
  public void isBusinessPartnerUmkmMerchantBpNotPresentTest() {
    boolean response = businessPartnerService.isBusinessPartnerUmkmMerchant(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerCacheableService)
        .findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(response);
  }

  @Test
  public void isBusinessPartnerUmkmMerchantTest() {
    Mockito.when(
        businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(businessPartner);
    boolean response = businessPartnerService.isBusinessPartnerUmkmMerchant(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerCacheableService)
        .findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Assertions.assertTrue(response);
  }

  @Test
  public void findByStoreIdAndBusinessPartnerCodesTest() {
    Mockito.when(
        businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(businessPartner);
    Mockito.when(
        businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE1))
        .thenReturn(businessPartner1);
    List<BusinessPartner> response =
        businessPartnerService.findByStoreIdAndBusinessPartnerCodes(Constants.STORE_ID, businessPartnerCodes);
    Mockito.verify(businessPartnerCacheableService)
        .findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerCacheableService)
        .findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE1);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, response.get(0).getBusinessPartnerName());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE1, response.get(1).getBusinessPartnerCode());
    Assertions.assertEquals(BUSINESS_PARTNER_TYPE, response.get(1).getBusinessPartnerType());
  }

  @Test
  public void findByStoreIdAndBusinessPartnerCodesExceptionTest() {
    businessPartnerCodes = new SimpleListStringRequest();
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> businessPartnerService.findByStoreIdAndBusinessPartnerCodes(Constants.STORE_ID, businessPartnerCodes));
  }

  @Test
  public void isBusinessPartnerB2bMerchantNullTest() {
    Mockito.when(
            businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(null);
    boolean response = businessPartnerService.isBusinessPartnerB2bMerchant(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerCacheableService)
        .findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(response);
  }

  @Test
  public void isBusinessPartnerB2bMerchantNonNullTest() {
    Mockito.when(
            businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(new BusinessPartner());
    boolean response = businessPartnerService.isBusinessPartnerB2bMerchant(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerCacheableService)
        .findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(response);
  }

  @Test
  public void isBusinessPartnerB2bMerchantNonNullSalesChannelTest() {
    businessPartner.setSalesChannel(Arrays.asList(Constants.B2C_SELLER_CHANNEL));
    Mockito.when(
            businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(businessPartner);
    boolean response = businessPartnerService.isBusinessPartnerB2bMerchant(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerCacheableService)
        .findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(response);
  }

  @Test
  public void isBusinessPartnerB2bMerchantTest() {
    businessPartner.setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    Mockito.when(
            businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(businessPartner);
    boolean response = businessPartnerService.isBusinessPartnerB2bMerchant(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerCacheableService)
        .findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Assertions.assertTrue(response);
  }

  @Test
  public void isBusinessPartnerUmkmAndB2bTest() {
    businessPartner.setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    Mockito.when(
            businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(businessPartner);
    Pair<Boolean, Boolean> response =
        businessPartnerService.isBusinessPartnerUmkmAndB2b(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerCacheableService)
        .findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Assertions.assertTrue(response.getRight());
    Assertions.assertTrue(response.getLeft());
  }

  @Test
  public void isBusinessPartnerUmkmAndB2bNullTest() {
    businessPartner.setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    Mockito.when(
            businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(null);
    Pair<Boolean, Boolean> response =
        businessPartnerService.isBusinessPartnerUmkmAndB2b(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerCacheableService)
        .findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(response.getRight());
    Assertions.assertFalse(response.getLeft());
  }

  @Test
  public void isBusinessPartnerUmkmAndB2bEmptyTest() {
    businessPartner.setSalesChannel(new ArrayList<>());
    Mockito.when(
            businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(businessPartner);
    Pair<Boolean, Boolean> response =
        businessPartnerService.isBusinessPartnerUmkmAndB2b(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerCacheableService)
        .findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(response.getRight());
    Assertions.assertTrue(response.getLeft());
  }

  @Test
  public void isBusinessPartnerUmkmAndB2bFailTest() {
    businessPartner.setSalesChannel(Arrays.asList(Constants.B2C_SELLER_CHANNEL));
    Mockito.when(
            businessPartnerCacheableService.findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(businessPartner);
    Pair<Boolean, Boolean> response =
        businessPartnerService.isBusinessPartnerUmkmAndB2b(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerCacheableService)
        .findByStoreIdAndBusinessPartnerCode(Constants.STORE_ID, BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(response.getRight());
    Assertions.assertTrue(response.getLeft());
  }
}