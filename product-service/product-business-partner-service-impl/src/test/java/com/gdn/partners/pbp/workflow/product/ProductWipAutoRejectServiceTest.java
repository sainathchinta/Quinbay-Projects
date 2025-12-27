package com.gdn.partners.pbp.workflow.product;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.ProductEventAlertNotificationModel;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.service.EmailNotificationService;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Wip;
import com.gdn.x.businesspartner.dto.BusinessPartnerCodesRequest;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;

public class ProductWipAutoRejectServiceTest {

  private static final String PRODUCT_CODE1 = "MTA-12345";
  private static final String PRODUCT_CODE2 = "MTA-54321";
  private static final String PRODUCT_ID1 = "productID1";
  private static final String PRODUCT_ID2 = "productID2";
  private static final String PRODUCT_SKU = "productSku";
  private static final String STORE_ID = "10001";
  private static final String NOTES = "scheduler auto reject product wip need correction expired";
  private static final String MAX_DAYS = "3";
  private static final String BUSINESS_PARTNER_CODE1 = "businessPartnerCode1";
  private static final String BUSINESS_PARTNER_CODE2 = "businessPartnerCode2";
  private static final String PRODUCT_AUTO_REJECT = "productAutoReject";

  @Captor
  ArgumentCaptor<BusinessPartnerCodesRequest> businessPartnerCodesRequestArgumentCaptor;

  @Captor
  ArgumentCaptor<ProductEventAlertNotificationModel> productEventAlertNotificationModelArgumentCaptor;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private ProductWfService productWfService;

  @Mock
  private ApplicationProperties applicationProperties;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private EmailNotificationService emailNotificationService;

  @InjectMocks
  private ProductWipAutoRejectServiceBean productWipAutoRejectServiceBean;

  private List<ProductCollection> productCollections = new ArrayList<>();

  private ProductCollection productCollection1;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(productWipAutoRejectServiceBean, "businessPartnerFetchSize", 100);
    productCollections = generateProductCollections();
    List<ProductLevel3Wip> productLevel3Wips = generateProductLevel3Wips();
    List<ProfileResponse> profileResponses = generateProfileResponses();
    Mockito.when(applicationProperties.getProductRejectMaxDays()).thenReturn(MAX_DAYS);
    Mockito.when(productCollectionRepository
        .findByStoreIdAndStateNeedCorrectionMoreThanMaxDays(Mockito.anyString(), Mockito.any()))
        .thenReturn(productCollections);
    Mockito.when(businessPartnerRepository.filterDetailsByBusinessPartnerCodeList(Mockito.any()))
        .thenReturn(profileResponses);
    Mockito.when(productCollectionRepository
        .findByProductCodeInAndStoreIdAndMarkForDeleteTrue(Mockito.anyList(), Mockito.eq(STORE_ID)))
        .thenReturn(Arrays.asList(productCollection1));
    Mockito.doNothing().when(this.productWfService).delete(Mockito.anyList(), Mockito.anyString());
  }

  private List<ProductLevel3Wip> generateProductLevel3Wips() {
    ProductLevel3Wip productLevel3Wip = new ProductLevel3Wip();
    productLevel3Wip.setProductSku(PRODUCT_SKU);
    productLevel3Wip.setProductLevel1Id(PRODUCT_ID1);
    List<ProductLevel3Wip> productLevel3Wips = new ArrayList<>();
    productLevel3Wips.add(productLevel3Wip);
    return productLevel3Wips;
  }

  private List<ProfileResponse> generateProfileResponses() {
    List<ProfileResponse> profileResponses = new ArrayList<>();
    ProfileResponse profileResponse1 = new ProfileResponse();
    CompanyDTO companyDTO1 = new CompanyDTO();
    companyDTO1.setOfflineToOnlineFlag(false);
    profileResponse1.setBusinessPartnerCode(BUSINESS_PARTNER_CODE1);
    profileResponse1.setCompany(companyDTO1);
    ProfileResponse profileResponse2 = new ProfileResponse();
    CompanyDTO companyDTO2 = new CompanyDTO();
    companyDTO2.setOfflineToOnlineFlag(true);
    profileResponse2.setBusinessPartnerCode(BUSINESS_PARTNER_CODE2);
    profileResponse2.setCompany(companyDTO2);
    profileResponses.add(profileResponse1);
    profileResponses.add(profileResponse2);
    return profileResponses;
  }

  private List<ProductCollection> generateProductCollections() {
    productCollection1 = new ProductCollection();
    productCollection1.setBusinessPartnerCode(BUSINESS_PARTNER_CODE1);
    productCollection1.setProductCode(PRODUCT_CODE1);
    productCollection1.setProductId(PRODUCT_ID1);
    ProductCollection productCollection2 = new ProductCollection();
    productCollection2.setBusinessPartnerCode(BUSINESS_PARTNER_CODE2);
    productCollection2.setProductCode(PRODUCT_CODE2);
    productCollection2.setProductId(PRODUCT_ID2);
    productCollections.add(productCollection1);
    productCollections.add(productCollection2);
    return productCollections;
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productCollectionRepository);
    Mockito.verifyNoMoreInteractions(this.productWfService);
    Mockito.verifyNoMoreInteractions(this.applicationProperties);
    Mockito.verifyNoMoreInteractions(this.businessPartnerRepository);
    Mockito.verifyNoMoreInteractions(emailNotificationService);
  }

  @Test
  public void autoRejectProductWipNeedCorrectionExpiredTest() throws Exception {
    this.productWipAutoRejectServiceBean.autoRejectProductWipNeedCorrectionExpired(STORE_ID);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndStateNeedCorrectionMoreThanMaxDays(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(this.applicationProperties).getProductRejectMaxDays();
    Mockito.verify(this.businessPartnerRepository).filterDetailsByBusinessPartnerCodeList(Mockito.any());
    Mockito.verify(this.productWfService).delete(Mockito.eq(Arrays.asList(PRODUCT_CODE1)), Mockito.eq(NOTES));
    Mockito.verify(this.productCollectionRepository)
        .findByProductCodeInAndStoreIdAndMarkForDeleteTrue(Arrays.asList(PRODUCT_CODE1), STORE_ID);
    Mockito.verify(emailNotificationService).sendEmailToBusinessPartnerForDeletedProducts(
        Mockito.eq(STORE_ID), Mockito.eq(BUSINESS_PARTNER_CODE1), Mockito.eq(Arrays.asList(productCollection1)));
  }

  @Test
  public void autoRejectProductWipNeedCorrectionExpiredNotFoundTest() throws Exception {
    Mockito.when(productCollectionRepository
        .findByStoreIdAndStateNeedCorrectionMoreThanMaxDays(Mockito.anyString(), Mockito.any()))
        .thenReturn(new ArrayList<ProductCollection>());
    this.productWipAutoRejectServiceBean.autoRejectProductWipNeedCorrectionExpired(STORE_ID);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndStateNeedCorrectionMoreThanMaxDays(Mockito.eq(STORE_ID), Mockito.any(Date.class));
    Mockito.verify(this.applicationProperties).getProductRejectMaxDays();
  }

  @Test
  public void autoRejectProductWipNeedCorrectionExpiredExceptionTest() throws Exception {
    Mockito.doThrow(new Exception()).when(productWfService)
        .delete(Mockito.eq(Arrays.asList(PRODUCT_CODE1)), Mockito.eq(NOTES));
    this.productWipAutoRejectServiceBean.autoRejectProductWipNeedCorrectionExpired(STORE_ID);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndStateNeedCorrectionMoreThanMaxDays(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(this.applicationProperties).getProductRejectMaxDays();
    Mockito.verify(this.businessPartnerRepository).filterDetailsByBusinessPartnerCodeList(
        businessPartnerCodesRequestArgumentCaptor.capture());
    BusinessPartnerCodesRequest businessPartnerCodesRequest =
        businessPartnerCodesRequestArgumentCaptor.getValue();
    Assertions.assertEquals(BUSINESS_PARTNER_CODE1,businessPartnerCodesRequest.getBusinessPartnerCodes().get(0));
    Assertions.assertEquals(BUSINESS_PARTNER_CODE2,businessPartnerCodesRequest.getBusinessPartnerCodes().get(1));
    Mockito.verify(this.productWfService).delete(Mockito.eq(Arrays.asList(PRODUCT_CODE1)), Mockito.eq(NOTES));
  }

  @Test
  public void autoRejectProductWipNeedCorrectionExpiredSendEmailExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(emailNotificationService)
        .sendEmailToBusinessPartnerForDeletedProducts(Mockito.eq(STORE_ID), Mockito.anyString(),
            Mockito.anyList());
    this.productWipAutoRejectServiceBean.autoRejectProductWipNeedCorrectionExpired(STORE_ID);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndStateNeedCorrectionMoreThanMaxDays(Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(this.applicationProperties).getProductRejectMaxDays();
    Mockito.verify(this.businessPartnerRepository).filterDetailsByBusinessPartnerCodeList(Mockito.any());
    Mockito.verify(this.productWfService).delete(Mockito.eq(Arrays.asList(PRODUCT_CODE1)), Mockito.eq(NOTES));
    Mockito.verify(this.productCollectionRepository)
        .findByProductCodeInAndStoreIdAndMarkForDeleteTrue(Arrays.asList(PRODUCT_CODE1), STORE_ID);
    Mockito.verify(emailNotificationService).sendEmailToBusinessPartnerForDeletedProducts(
        Mockito.eq(STORE_ID), Mockito.eq(BUSINESS_PARTNER_CODE1), Mockito.eq(Arrays.asList(productCollection1)));
  }


}
