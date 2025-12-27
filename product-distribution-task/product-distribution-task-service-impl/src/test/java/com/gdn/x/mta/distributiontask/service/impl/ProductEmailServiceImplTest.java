package com.gdn.x.mta.distributiontask.service.impl;

import com.gdn.x.mta.distributiontask.dao.api.ProductEmailsRepository;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductEmailDomainEvent;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductEmailEventModel;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.ProductEmails;
import com.gdn.x.mta.distributiontask.service.impl.publisher.ProductEmailEventPublisherImpl;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
class ProductEmailServiceImplTest {

  private static final String STORE_ID = "STORE_ID";
  private static final String EMAIL_TYPE = "email-type";
  private static final String BUSINESS_PARTNER_CODE = "business-partner-code";
  private static final String PRODUCT_SKU = "product-sku";
  @InjectMocks
  private ProductEmailServiceImpl productEmailService;
  @Mock
  private ProductEmailsRepository productEmailsRepository;
  @Mock
  private ProductEmailEventPublisherImpl productEmailEventPublisher;
  private ProductEmails productEmails;

  @BeforeEach
  public void setUp() {
    productEmails = new ProductEmails();
    productEmails.setProductSku("Product-Sku");
    productEmails.setNotes("Notes");
    productEmails.setProductName("Product-Name");
    ReflectionTestUtils.setField(productEmailService, "productSkuMailBatchSize", 10);
  }

  @AfterEach
  public void tearDown() {

    Mockito.verifyNoMoreInteractions(productEmailsRepository);
    Mockito.verifyNoMoreInteractions(productEmailEventPublisher);
  }

  @Test
  void sendProductMailEventsToBusinessPartnersForSuspensionTest() {
    Mockito.when(
      productEmailsRepository.findBusinessPartnerCodesAndProductEmailTypeAndStatus(STORE_ID,
        EMAIL_TYPE, Constants.PENDING)).thenReturn(List.of(BUSINESS_PARTNER_CODE));
    Mockito.when(
        productEmailsRepository.findByStoreIdAndBusinessPartnerCodeAndProductEmailTypeAndStatus(
          STORE_ID, BUSINESS_PARTNER_CODE, EMAIL_TYPE, Constants.PENDING))
      .thenReturn(List.of(productEmails));
    productEmailService.sendProductMailEventsToBusinessPartnersForSuspension(STORE_ID, EMAIL_TYPE);
    Mockito.verify(productEmailsRepository)
      .findBusinessPartnerCodesAndProductEmailTypeAndStatus(STORE_ID, EMAIL_TYPE,
        Constants.PENDING);
    Mockito.verify(productEmailsRepository)
      .findByStoreIdAndBusinessPartnerCodeAndProductEmailTypeAndStatus(STORE_ID,
        BUSINESS_PARTNER_CODE, EMAIL_TYPE, Constants.PENDING);
    Mockito.verify(productEmailsRepository).save(Mockito.any(ProductEmails.class));
    Mockito.verify(productEmailEventPublisher)
      .publishProductMailDomainEventForIprEvidenceRequestedProduct(
        Mockito.any(ProductEmailDomainEvent.class));
  }

  @Test
  void sendProductMailEventsNoDistinctBpCodeTest() {
    Mockito.when(
      productEmailsRepository.findBusinessPartnerCodesAndProductEmailTypeAndStatus(STORE_ID,
        EMAIL_TYPE, Constants.PENDING)).thenReturn(new ArrayList<>());
    productEmailService.sendProductMailEventsToBusinessPartnersForSuspension(STORE_ID, EMAIL_TYPE);
    Mockito.verify(productEmailsRepository)
      .findBusinessPartnerCodesAndProductEmailTypeAndStatus(STORE_ID, EMAIL_TYPE,
        Constants.PENDING);
  }

  @Test
  void sendProductMailEventsNoPendingTest() {
    Mockito.when(
      productEmailsRepository.findBusinessPartnerCodesAndProductEmailTypeAndStatus(STORE_ID,
        EMAIL_TYPE, Constants.PENDING)).thenReturn(List.of(BUSINESS_PARTNER_CODE));
    Mockito.when(
        productEmailsRepository.findByStoreIdAndBusinessPartnerCodeAndProductEmailTypeAndStatus(
          STORE_ID, BUSINESS_PARTNER_CODE, EMAIL_TYPE, Constants.PENDING))
      .thenReturn(new ArrayList<>());
    productEmailService.sendProductMailEventsToBusinessPartnersForSuspension(STORE_ID, EMAIL_TYPE);
    Mockito.verify(productEmailsRepository)
      .findBusinessPartnerCodesAndProductEmailTypeAndStatus(STORE_ID, EMAIL_TYPE,
        Constants.PENDING);
    Mockito.verify(productEmailsRepository)
      .findByStoreIdAndBusinessPartnerCodeAndProductEmailTypeAndStatus(STORE_ID,
        BUSINESS_PARTNER_CODE, EMAIL_TYPE, Constants.PENDING);
  }

  @Test
  void addProductToEmailProcessResetTrueTest() {
    ProductEmailEventModel eventModel = new ProductEmailEventModel();
    eventModel.setResetStatus(true);
    eventModel.setProductSku(PRODUCT_SKU);
    eventModel.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productEmailService.addProductToEmailProcess(eventModel);
    Mockito.verify(productEmailsRepository).deleteMailRecord(PRODUCT_SKU, BUSINESS_PARTNER_CODE);
  }

  @Test
  void addProductToEmailProcessTest() {
    ProductEmailEventModel eventModel = new ProductEmailEventModel();
    eventModel.setProductSku(PRODUCT_SKU);
    eventModel.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productEmailService.addProductToEmailProcess(eventModel);
    Mockito.verify(productEmailsRepository).save(Mockito.any(ProductEmails.class));
  }
}
