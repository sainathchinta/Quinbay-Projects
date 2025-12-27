package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductMigration;
import com.gdn.mta.product.enums.MigrationStatus;
import com.gdn.mta.product.repository.ProductMigrationRepository;

public class ProductMigrationServiceTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PRODUCT_SKU1 = "productSku1";
  private static final String BUSINESS_PARTNER_ID = "merchantCode";
  private static final String MERCHANT_TYPE = "CC";
  private static final String MERCHANT_STATUS = "ACTIVE";
  private static final String MERCHANT_NAME = "merchantName";
  private static final String ID = "ID";
  private static final int LIMIT = 10;

  private ProductMigration productMigration;

  @InjectMocks
  private ProductMigrationServiceImpl productMigrationService;

  @Mock
  private ProductMigrationRepository productMigrationRepository;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);

    productMigration = new ProductMigration();
    productMigration.setGdnProductSku(PRODUCT_SKU);
    productMigration.setMigratedProductCode(PRODUCT_CODE);
    productMigration.setBusinessPartnerId(BUSINESS_PARTNER_ID);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productMigrationRepository);
  }

  @Test
  public void rollbackMigrationChangesTest() throws Exception {
    Mockito.when(productMigrationRepository.findByGdnProductSkuIn(Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(productMigration));
    List<ProductMigration> productMigrationList = productMigrationService.getProductMigrationByProductSkus(Arrays.asList(PRODUCT_SKU));
    Mockito.verify(productMigrationRepository).findByGdnProductSkuIn(Arrays.asList(PRODUCT_SKU));
    Assertions.assertEquals(PRODUCT_SKU, productMigrationList.get(0).getGdnProductSku());
  }

  @Test
  public void getProductMigrationByProductSkusWithStatusTest() throws Exception {
    Mockito.when(productMigrationRepository.findByGdnProductSkuInAndMigrationStatus(Arrays.asList(PRODUCT_SKU),
        MigrationStatus.PENDING.getMigrationStatus())).thenReturn(Arrays.asList(productMigration));
    List<ProductMigration> productMigrationList = productMigrationService
        .getProductMigrationByProductSkusWithStatus(Arrays.asList(PRODUCT_SKU),
            MigrationStatus.PENDING.getMigrationStatus());
    Mockito.verify(productMigrationRepository).findByGdnProductSkuInAndMigrationStatus(Arrays.asList(PRODUCT_SKU),
        MigrationStatus.PENDING.getMigrationStatus());
    Assertions.assertEquals(PRODUCT_SKU, productMigrationList.get(0).getGdnProductSku());
  }

  @Test
  public void findByProductCodesTest() throws Exception {
    Mockito.when(productMigrationRepository.findByProductCodeIn(Arrays.asList(PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productMigration));
    productMigrationService.findByProductCodes(Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productMigrationRepository).findByProductCodeIn(Arrays.asList(PRODUCT_CODE));
  }

  @Test
  public void findByProductCodesEmptyArrayTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productMigrationService.findByProductCodes(new ArrayList<>());
    });
  }

  @Test
  public void findByProductCodesWithFailedStatusTest() throws Exception {
    Mockito.when(productMigrationRepository.findByProductCodeInAndMigrationStatus(Arrays.asList(PRODUCT_CODE),
        MigrationStatus.FAILED.getMigrationStatus())).thenReturn(Arrays.asList(productMigration));
    productMigrationService
        .findByProductCodesWithStatus(Arrays.asList(PRODUCT_CODE), MigrationStatus.FAILED.getMigrationStatus());
    Mockito.verify(productMigrationRepository).findByProductCodeInAndMigrationStatus(Arrays.asList(PRODUCT_CODE),
        MigrationStatus.FAILED.getMigrationStatus());
  }

  @Test
  public void findByProductCodesWithFailedStatusEmptyArrayTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productMigrationService
          .findByProductCodesWithStatus(new ArrayList<>(), MigrationStatus.FAILED.getMigrationStatus());
    });
  }

  @Test
  public void updateBusinessPartnerDetailsForProductCodesExceptionTest() throws Exception {
    Mockito.doNothing().when(productMigrationRepository)
        .updateBusinessPartnerDetails(BUSINESS_PARTNER_ID, MERCHANT_TYPE, MERCHANT_STATUS, MERCHANT_NAME,
            Arrays.asList(PRODUCT_SKU, PRODUCT_SKU1));
    productMigrationService
        .updateBusinessPartnerDetails(BUSINESS_PARTNER_ID, MERCHANT_TYPE, MERCHANT_STATUS, MERCHANT_NAME,
            Arrays.asList(PRODUCT_SKU, PRODUCT_SKU1));
    Mockito.verify(productMigrationRepository)
        .updateBusinessPartnerDetails(BUSINESS_PARTNER_ID, MERCHANT_TYPE, MERCHANT_STATUS, MERCHANT_NAME,
            Arrays.asList(PRODUCT_SKU, PRODUCT_SKU1));
  }

  @Test
  public void updateBusinessPartnerDetailsForProductCodes() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productMigrationService
          .updateBusinessPartnerDetails(BUSINESS_PARTNER_ID, MERCHANT_TYPE, MERCHANT_STATUS, MERCHANT_NAME,
              new ArrayList<>());
    });
  }

  @Test
  public void saveTest() throws Exception {
    Mockito.when(productMigrationRepository.save(productMigration)).thenReturn(productMigration);
    productMigrationService.save(productMigration);
    Mockito.verify(productMigrationRepository).save(productMigration);
  }

  @Test
  public void saveUpdateCaseTest() throws Exception {
    productMigration.setId(ID);
    Mockito.when(productMigrationRepository.findById(ID)).thenReturn(Optional.of(productMigration));
    Mockito.when(productMigrationRepository.save(productMigration)).thenReturn(productMigration);
    productMigrationService.save(productMigration);
    Mockito.verify(productMigrationRepository).save(productMigration);
    Mockito.verify(productMigrationRepository).findById(ID);
  }

  @Test
  public void findDistinctProductCodesForMigrationTest() throws Exception {
    Mockito.when(productMigrationRepository.findDistinctProductCodesForMigration(LIMIT))
        .thenReturn(Arrays.asList(PRODUCT_CODE));
    List<String> productCodes = productMigrationService.findDistinctProductCodesForMigration(LIMIT);
    Mockito.verify(productMigrationRepository).findDistinctProductCodesForMigration(LIMIT);
    Assertions.assertEquals(1, productCodes.size());
    Assertions.assertEquals(PRODUCT_CODE, productCodes.get(0));
  }

  @Test
  public void findProductForMigrationWithNullProductCodeTest() throws Exception {
    Mockito.when(productMigrationRepository.findProductSkusForMigration(LIMIT))
        .thenReturn(Arrays.asList(productMigration.getGdnProductSku()));
    List<String> productMigrations = productMigrationService.findProductForMigrationWithNullProductCode(LIMIT);
    Mockito.verify(productMigrationRepository).findProductSkusForMigration(LIMIT);
    Assertions.assertEquals(1, productMigrations.size());
    Assertions.assertEquals(PRODUCT_SKU, productMigrations.get(0));
  }

  @Test
  public void findFailedProductMigrationTest() {
    Mockito.when(productMigrationRepository
        .findByMigrationStatusAndRetryCountLessThanOrderByUpdatedDateDesc(MigrationStatus.FAILED.getMigrationStatus(), 3,
            PageRequest.of(0, 1))).thenReturn(new PageImpl<>(Arrays.asList(productMigration)));
    Page<ProductMigration> productMigrations =
        productMigrationService.findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), 1, 3);
    Mockito.verify(productMigrationRepository)
        .findByMigrationStatusAndRetryCountLessThanOrderByUpdatedDateDesc(MigrationStatus.FAILED.getMigrationStatus(), 3,
            PageRequest.of(0, 1));
    Assertions.assertNotNull(productMigrations.getContent().get(0));
    Assertions.assertEquals(1, productMigrations.getContent().size());
  }

  @Test
  public void updateProductMigrationStatusByProductCodesTest() {
    List<String> productCodes = new ArrayList<>();
    productCodes.add(PRODUCT_CODE);
    Mockito.doNothing().when(productMigrationRepository)
        .updateProductMigrationStatusByProductCodes(productCodes, MigrationStatus.PENDING.getMigrationStatus());
    productMigrationService
        .updateProductMigrationStatusByProductCodes(productCodes, MigrationStatus.PENDING.getMigrationStatus());
    Mockito.verify(productMigrationRepository)
        .updateProductMigrationStatusByProductCodes(productCodes, MigrationStatus.PENDING.getMigrationStatus());
  }

  @Test
  public void updateProductMigrationStatusByProductSkusTest() {
    List<String> productCodes = new ArrayList<>();
    productCodes.add(PRODUCT_CODE);
    Mockito.doNothing().when(productMigrationRepository)
        .updateProductMigrationStatusByProductSkus(productCodes, MigrationStatus.PENDING.getMigrationStatus());
    productMigrationService
        .updateProductMigrationStatusByProductSkus(productCodes, MigrationStatus.PENDING.getMigrationStatus());
    Mockito.verify(productMigrationRepository)
        .updateProductMigrationStatusByProductSkus(productCodes, MigrationStatus.PENDING.getMigrationStatus());
  }

  @Test
  public void updateProductMigrationStatusByProductCodesExceptionTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productMigrationService
          .updateProductMigrationStatusByProductSkus(new ArrayList<>(), MigrationStatus.PENDING.getMigrationStatus());
    });
  }


  @Test
  public void updateProductMigrationStatusByProductSkusExceptionTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productMigrationService
          .updateProductMigrationStatusByProductSkus(new ArrayList<>(), MigrationStatus.PENDING.getMigrationStatus());
    });
  }

  @Test
  public void updateProductMigrationStatusTest() {
    Date date = new Date();
    Mockito.doNothing().when(productMigrationRepository)
        .updateProductMigrationStatus(MigrationStatus.IN_PROGRESS.getMigrationStatus(),
            MigrationStatus.FAILED.getMigrationStatus(), date);
    productMigrationService.updateProductMigrationStatus(MigrationStatus.IN_PROGRESS.getMigrationStatus(),
        MigrationStatus.FAILED.getMigrationStatus(), date);
    Mockito.verify(productMigrationRepository)
        .updateProductMigrationStatus(MigrationStatus.IN_PROGRESS.getMigrationStatus(),
            MigrationStatus.FAILED.getMigrationStatus(), date);
  }
}
