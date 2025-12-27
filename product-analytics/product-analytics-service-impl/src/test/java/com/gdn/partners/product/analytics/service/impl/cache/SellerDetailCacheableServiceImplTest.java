package com.gdn.partners.product.analytics.service.impl.cache;

import static org.mockito.MockitoAnnotations.openMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.partners.product.analytics.entity.SellerQCDetail;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.model.enums.SellerTypes;
import com.gdn.partners.product.analytics.properties.ApplicationProperties;
import com.gdn.partners.product.analytics.repository.SellerQCDetailRepository;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;

public class SellerDetailCacheableServiceImplTest {

  private static final String MERCHANT_CODE = "merchant_code";
  private static final String GOOD_SELLER_LIST = "isOfficialStore,isWhitelistSeller,isWhitelistCompanyOfficer";
  private static final String DIAMOND_MERCHANT = "Diamond Merchant";
  private SellerQCDetail sellerQCDetail;

  @InjectMocks
  private SellerDetailCacheableServiceImpl sellerDetailCacheableService;

  @Mock
  private SellerQCDetailRepository sellerQCDetailRepository;

  @Mock
  private ApplicationProperties applicationProperties;

  @BeforeEach
  public void setUp() {
    openMocks(this);
    Mockito.verifyNoMoreInteractions(sellerQCDetailRepository);
    Mockito.verifyNoMoreInteractions(applicationProperties);

    sellerQCDetail = new SellerQCDetail();
  }

  @AfterEach
  public void tearDown() {
  }

  @Test
  public void findCacheablesByMerchantCodeNullTest() throws NoSuchFieldException, IllegalAccessException {
    sellerDetailCacheableService.findCacheablesByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void findCacheablesByMerchantCodeNotValidTest() throws NoSuchFieldException, IllegalAccessException {
    Mockito.when(applicationProperties.getGoodSellerList()).thenReturn(GOOD_SELLER_LIST);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    SellerDetailResponse sellerDetailResponse =
        sellerDetailCacheableService.findCacheablesByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(applicationProperties).getGoodSellerList();
    Assertions.assertFalse(sellerDetailResponse.isGoodSeller());
    Assertions.assertEquals(SellerTypes.NOT_VALID.name(), sellerDetailResponse.getSellerType());
    Assertions.assertEquals(MERCHANT_CODE, sellerDetailResponse.getBusinessPartnerCode());
  }

  @Test
  public void findCacheablesByMerchantCodeOfficialFalseTest() throws NoSuchFieldException, IllegalAccessException {
    sellerQCDetail.setIsOfficialStore(false);
    Mockito.when(applicationProperties.getGoodSellerList()).thenReturn(GOOD_SELLER_LIST);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    SellerDetailResponse sellerDetailResponse =
        sellerDetailCacheableService.findCacheablesByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(applicationProperties).getGoodSellerList();
    Assertions.assertFalse(sellerDetailResponse.isGoodSeller());
    Assertions.assertEquals(SellerTypes.NOT_VALID.name(), sellerDetailResponse.getSellerType());
    Assertions.assertEquals(MERCHANT_CODE, sellerDetailResponse.getBusinessPartnerCode());
  }

  @Test
  public void findCacheablesByMerchantCodeOfficialTrueTest() throws NoSuchFieldException, IllegalAccessException {
    sellerQCDetail.setIsOfficialStore(true);
    sellerQCDetail.setSellerBadge(DIAMOND_MERCHANT);
    Mockito.when(applicationProperties.getGoodSellerList()).thenReturn(GOOD_SELLER_LIST);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    SellerDetailResponse sellerDetailResponse =
        sellerDetailCacheableService.findCacheablesByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(applicationProperties).getGoodSellerList();
    Assertions.assertTrue(sellerDetailResponse.isGoodSeller());
    Assertions.assertEquals(SellerTypes.OFFICIAL_SELLERS.name(), sellerDetailResponse.getSellerType());
    Assertions.assertEquals(MERCHANT_CODE, sellerDetailResponse.getBusinessPartnerCode());
    Assertions.assertEquals(Constants.OFFICIAL_STORES,sellerDetailResponse.getSellerBadge());
  }

  @Test
  public void findCacheablesByMerchantCodeWhiteListTrueTest() throws NoSuchFieldException, IllegalAccessException {
    sellerQCDetail.setIsOfficialStore(false);
    sellerQCDetail.setIsWhitelistSeller(true);
    Mockito.when(applicationProperties.getGoodSellerList()).thenReturn(GOOD_SELLER_LIST);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    SellerDetailResponse sellerDetailResponse =
        sellerDetailCacheableService.findCacheablesByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(applicationProperties).getGoodSellerList();
    Assertions.assertTrue(sellerDetailResponse.isGoodSeller());
    Assertions.assertEquals(SellerTypes.WHITELISTED_SELLERS.name(), sellerDetailResponse.getSellerType());
    Assertions.assertEquals(MERCHANT_CODE, sellerDetailResponse.getBusinessPartnerCode());
  }

  @Test
  public void findCacheablesByMerchantCodeWhiteListFalseTest() throws NoSuchFieldException, IllegalAccessException {
    sellerQCDetail.setIsOfficialStore(false);
    sellerQCDetail.setIsWhitelistSeller(false);
    Mockito.when(applicationProperties.getGoodSellerList()).thenReturn(GOOD_SELLER_LIST);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    SellerDetailResponse sellerDetailResponse =
        sellerDetailCacheableService.findCacheablesByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(applicationProperties).getGoodSellerList();
    Assertions.assertFalse(sellerDetailResponse.isGoodSeller());
    Assertions.assertEquals(SellerTypes.NOT_VALID.name(), sellerDetailResponse.getSellerType());
    Assertions.assertEquals(MERCHANT_CODE, sellerDetailResponse.getBusinessPartnerCode());
  }

  @Test
  public void findCacheablesByMerchantCodeCommanyOfficersFalseTest()
      throws NoSuchFieldException, IllegalAccessException {
    sellerQCDetail.setIsOfficialStore(false);
    sellerQCDetail.setIsWhitelistSeller(false);
    sellerQCDetail.setIsWhitelistCompanyOfficer(false);
    Mockito.when(applicationProperties.getGoodSellerList()).thenReturn(GOOD_SELLER_LIST);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    SellerDetailResponse sellerDetailResponse =
        sellerDetailCacheableService.findCacheablesByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(applicationProperties).getGoodSellerList();
    Assertions.assertFalse(sellerDetailResponse.isGoodSeller());
    Assertions.assertEquals(SellerTypes.NOT_VALID.name(), sellerDetailResponse.getSellerType());
    Assertions.assertEquals(MERCHANT_CODE, sellerDetailResponse.getBusinessPartnerCode());
  }

  @Test
  public void findCacheablesByMerchantCodeCompanyOfficersTrueTest()
      throws NoSuchFieldException, IllegalAccessException {
    sellerQCDetail.setIsOfficialStore(false);
    sellerQCDetail.setIsWhitelistSeller(false);
    sellerQCDetail.setIsWhitelistCompanyOfficer(true);
    Mockito.when(applicationProperties.getGoodSellerList()).thenReturn(GOOD_SELLER_LIST);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    SellerDetailResponse sellerDetailResponse =
        sellerDetailCacheableService.findCacheablesByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(applicationProperties).getGoodSellerList();
    Assertions.assertTrue(sellerDetailResponse.isGoodSeller());
    Assertions.assertEquals(SellerTypes.COMPANY_OFFICER_SELLERS.name(), sellerDetailResponse.getSellerType());
    Assertions.assertEquals(MERCHANT_CODE, sellerDetailResponse.getBusinessPartnerCode());
    Assertions.assertEquals(Constants.NO_BADGE_SELLERS, sellerDetailResponse.getSellerBadge());
  }

  @Test
  public void findCacheablesByMerchantCodeDiamondTest() throws NoSuchFieldException, IllegalAccessException {
    sellerQCDetail.setIsOfficialStore(false);
    sellerQCDetail.setIsWhitelistSeller(false);
    sellerQCDetail.setIsWhitelistCompanyOfficer(false);
    sellerQCDetail.setSellerBadge(Constants.DIAMOND_OR_GOLD_SELLERS_LIST.get(0));
    Mockito.when(applicationProperties.getGoodSellerList()).thenReturn(GOOD_SELLER_LIST);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    SellerDetailResponse sellerDetailResponse =
        sellerDetailCacheableService.findCacheablesByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(applicationProperties).getGoodSellerList();
    Assertions.assertFalse(sellerDetailResponse.isGoodSeller());
    Assertions.assertEquals(SellerTypes.DIAMOND_OR_GOLD_SELLERS.name(), sellerDetailResponse.getSellerType());
    Assertions.assertEquals(MERCHANT_CODE, sellerDetailResponse.getBusinessPartnerCode());
  }

  @Test
  public void findCacheablesByMerchantCodeGoldTest() throws NoSuchFieldException, IllegalAccessException {
    sellerQCDetail.setIsOfficialStore(false);
    sellerQCDetail.setIsWhitelistSeller(false);
    sellerQCDetail.setIsWhitelistCompanyOfficer(false);
    sellerQCDetail.setSellerBadge(Constants.DIAMOND_OR_GOLD_SELLERS_LIST.get(1));
    Mockito.when(applicationProperties.getGoodSellerList()).thenReturn(GOOD_SELLER_LIST);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    SellerDetailResponse sellerDetailResponse =
        sellerDetailCacheableService.findCacheablesByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(applicationProperties).getGoodSellerList();
    Assertions.assertFalse(sellerDetailResponse.isGoodSeller());
    Assertions.assertEquals(SellerTypes.DIAMOND_OR_GOLD_SELLERS.name(), sellerDetailResponse.getSellerType());
    Assertions.assertEquals(MERCHANT_CODE, sellerDetailResponse.getBusinessPartnerCode());
  }

  @Test
  public void findCacheablesByMerchantCodeBronzeTest() throws NoSuchFieldException, IllegalAccessException {
    sellerQCDetail.setIsOfficialStore(false);
    sellerQCDetail.setIsWhitelistSeller(false);
    sellerQCDetail.setIsWhitelistCompanyOfficer(false);
    sellerQCDetail.setSellerBadge(Constants.BRONZE_OR_SILVER_SELLERS.get(0));
    Mockito.when(applicationProperties.getGoodSellerList()).thenReturn(GOOD_SELLER_LIST);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    SellerDetailResponse sellerDetailResponse =
        sellerDetailCacheableService.findCacheablesByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(applicationProperties).getGoodSellerList();
    Assertions.assertFalse(sellerDetailResponse.isGoodSeller());
    Assertions.assertEquals(SellerTypes.BRONZE_OR_SILVER_SELLERS.name(), sellerDetailResponse.getSellerType());
    Assertions.assertEquals(MERCHANT_CODE, sellerDetailResponse.getBusinessPartnerCode());
  }

  @Test
  public void findCacheablesByMerchantCodeSilverTest() throws NoSuchFieldException, IllegalAccessException {
    sellerQCDetail.setIsOfficialStore(false);
    sellerQCDetail.setIsWhitelistSeller(false);
    sellerQCDetail.setIsWhitelistCompanyOfficer(false);
    sellerQCDetail.setSellerBadge(Constants.BRONZE_OR_SILVER_SELLERS.get(1));
    Mockito.when(applicationProperties.getGoodSellerList()).thenReturn(GOOD_SELLER_LIST);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    SellerDetailResponse sellerDetailResponse =
        sellerDetailCacheableService.findCacheablesByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(applicationProperties).getGoodSellerList();
    Assertions.assertFalse(sellerDetailResponse.isGoodSeller());
    Assertions.assertEquals(SellerTypes.BRONZE_OR_SILVER_SELLERS.name(), sellerDetailResponse.getSellerType());
    Assertions.assertEquals(MERCHANT_CODE, sellerDetailResponse.getBusinessPartnerCode());
  }

  @Test
  public void findCacheablesByMerchantCodeNoBadgeTest() throws NoSuchFieldException, IllegalAccessException {
    sellerQCDetail.setIsOfficialStore(false);
    sellerQCDetail.setIsWhitelistSeller(false);
    sellerQCDetail.setIsWhitelistCompanyOfficer(false);
    sellerQCDetail.setSellerBadge(Constants.NO_BADGE_SELLERS);
    Mockito.when(applicationProperties.getGoodSellerList()).thenReturn(GOOD_SELLER_LIST);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    SellerDetailResponse sellerDetailResponse =
        sellerDetailCacheableService.findCacheablesByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(applicationProperties).getGoodSellerList();
    Assertions.assertFalse(sellerDetailResponse.isGoodSeller());
    Assertions.assertEquals(SellerTypes.NO_BADGE_SELLERS.name(), sellerDetailResponse.getSellerType());
    Assertions.assertEquals(MERCHANT_CODE, sellerDetailResponse.getBusinessPartnerCode());
    Assertions.assertEquals(Constants.NO_BADGE_SELLERS, sellerDetailResponse.getSellerBadge());
  }

  @Test
  public void findCacheablesByMerchantCodeOfficialSellerAndBadgeTest()
      throws NoSuchFieldException, IllegalAccessException {
    sellerQCDetail.setIsOfficialStore(true);
    sellerQCDetail.setIsWhitelistSeller(false);
    sellerQCDetail.setIsWhitelistCompanyOfficer(false);
    sellerQCDetail.setSellerBadge(Constants.NO_BADGE_SELLERS);
    Mockito.when(applicationProperties.getGoodSellerList()).thenReturn(GOOD_SELLER_LIST);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    SellerDetailResponse sellerDetailResponse =
        sellerDetailCacheableService.findCacheablesByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(applicationProperties).getGoodSellerList();
    Assertions.assertTrue(sellerDetailResponse.isGoodSeller());
    Assertions.assertEquals(SellerTypes.OFFICIAL_SELLERS.name(), sellerDetailResponse.getSellerType());
    Assertions.assertEquals(MERCHANT_CODE, sellerDetailResponse.getBusinessPartnerCode());
    Assertions.assertEquals(Constants.OFFICIAL_STORES, sellerDetailResponse.getSellerBadge());
  }

  @Test
  public void findCacheablesByMerchantCodeOfficialTrueTestWithNoBadge() throws NoSuchFieldException,
    IllegalAccessException {
    sellerQCDetail.setIsOfficialStore(true);
    sellerQCDetail.setSellerBadge(null);
    Mockito.when(applicationProperties.getGoodSellerList()).thenReturn(GOOD_SELLER_LIST);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    SellerDetailResponse sellerDetailResponse =
      sellerDetailCacheableService.findCacheablesByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(applicationProperties).getGoodSellerList();
    Assertions.assertTrue(sellerDetailResponse.isGoodSeller());
    Assertions.assertEquals(SellerTypes.OFFICIAL_SELLERS.name(), sellerDetailResponse.getSellerType());
    Assertions.assertEquals(MERCHANT_CODE, sellerDetailResponse.getBusinessPartnerCode());
    Assertions.assertEquals(Constants.OFFICIAL_STORES,sellerDetailResponse.getSellerBadge());
  }

  @Test
  public void findCacheablesByMerchantCodeOfficialFalseTestDiamond() throws NoSuchFieldException,
    IllegalAccessException {
    sellerQCDetail.setIsOfficialStore(false);
    sellerQCDetail.setSellerBadge(DIAMOND_MERCHANT);
    Mockito.when(applicationProperties.getGoodSellerList()).thenReturn(GOOD_SELLER_LIST);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    SellerDetailResponse sellerDetailResponse =
      sellerDetailCacheableService.findCacheablesByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(applicationProperties).getGoodSellerList();
    Assertions.assertEquals(MERCHANT_CODE, sellerDetailResponse.getBusinessPartnerCode());
    Assertions.assertEquals(DIAMOND_MERCHANT,sellerDetailResponse.getSellerBadge());
  }


  @Test
  public void evictCacheByMerchantCodeTest() {
    sellerDetailCacheableService.evictCacheByMerchantCode(MERCHANT_CODE);
  }
}