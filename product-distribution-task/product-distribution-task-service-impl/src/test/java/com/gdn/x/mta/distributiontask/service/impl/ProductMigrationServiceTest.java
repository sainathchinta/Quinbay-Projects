package com.gdn.x.mta.distributiontask.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.x.mta.distributiontask.dao.api.ProductMigrationRepository;
import com.gdn.x.mta.distributiontask.model.ProductMigration;
import com.gdn.x.mta.distributiontask.model.enums.ProductMigrationStatus;
import com.gdn.x.mta.distributiontask.model.enums.ProductMigrationType;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class ProductMigrationServiceTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String STORE_ID = "storeId";

  private ProductMigration productMigration;

  @Mock
  private ProductMigrationRepository productMigrationRepository;

  @InjectMocks
  private ProductMigrationServiceImpl productMigrationService;

  @BeforeEach
  public void setUp() {
    productMigration = new ProductMigration(PRODUCT_CODE, ProductMigrationStatus.PUBLISHED.name(),
        ProductMigrationType.COMMON_IMAGE_MIGRATION.name());
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productMigrationRepository);
  }

  @Test
   void findCommonImageMigrationByProductCodeAndStatusTest() {
    Mockito.when(productMigrationRepository.findByProductCodeAndStatus(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name())).thenReturn(productMigration);
    ProductMigration commonImageMigration =
        productMigrationService.findProductMigrationByProductCodeAndStatus(PRODUCT_CODE,
            ProductMigrationStatus.PUBLISHED.name());
    Mockito.verify(productMigrationRepository)
        .findByProductCodeAndStatus(PRODUCT_CODE, ProductMigrationStatus.PUBLISHED.name());
    Assertions.assertEquals(this.productMigration, commonImageMigration);
  }

  @Test
   void findCommonImageMigrationByProductCodeAndStatusProductCodeEmptyTest() {
    Assertions.assertThrows(Exception.class,
      () -> productMigrationService.findProductMigrationByProductCodeAndStatus(StringUtils.EMPTY,
            ProductMigrationStatus.PUBLISHED.name()));
  }

  @Test
   void findCommonImageMigrationByProductCodeAndStatusEmptyTest() {
    Assertions.assertThrows(Exception.class,
      () -> productMigrationService.findProductMigrationByProductCodeAndStatus(PRODUCT_CODE,
        StringUtils.EMPTY));
  }

  @Test
   void saveCommonImageMigrationTest() {
    Mockito.when(productMigrationRepository.save(productMigration)).thenReturn(productMigration);
    ProductMigration commonImageMigration =
        productMigrationService.saveProductMigration(this.productMigration);
    Mockito.verify(productMigrationRepository).save(commonImageMigration);
    Assertions.assertEquals(this.productMigration, commonImageMigration);
  }

  @Test
   void findCommonImageMigrationByProductCodesAndStatusTest() {
    Mockito.when(
            productMigrationRepository.findProductMigrationByStoreIdAndMigrationTypeAndProductCodeIn(Mockito.anyString(),
                Mockito.eq(ProductMigrationType.COMMON_IMAGE_MIGRATION.name()), Mockito.anyList()))
        .thenReturn(Collections.singletonList(productMigration));
    List<ProductMigration> result = productMigrationService.findProductMigrationByProductCodes(STORE_ID,
        ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), List.of(PRODUCT_CODE));
    Mockito.verify(productMigrationRepository)
        .findProductMigrationByStoreIdAndMigrationTypeAndProductCodeIn(STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(),
            List.of(PRODUCT_CODE));
  }

  @Test
   void findCommonImageMigrationByProductCodesAndStatusStoreIdEmptyTest() {
    Assertions.assertThrows(Exception.class,
      () -> productMigrationService.findProductMigrationByProductCodes(StringUtils.EMPTY,
        ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), List.of(PRODUCT_CODE)));
  }

  @Test
   void findCommonImageMigrationByProductCodesAndStatusProductCodeEmptyTest() {
    Assertions.assertThrows(Exception.class,
      () -> productMigrationService.findProductMigrationByProductCodes(STORE_ID,
        ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), new ArrayList<>()));
  }

  @Test
   void findCommonImageMigrationByProductCodesAndStatusMigrationTypeEmptyTest() {
    Assertions.assertThrows(Exception.class,
      () -> productMigrationService.findProductMigrationByProductCodes(STORE_ID,
        StringUtils.EMPTY, List.of(PRODUCT_CODE)));
  }

  @Test
   void findCommonImageMigrationByStoreIdAndStatusTest() {
    Mockito.when(productMigrationRepository.findByStoreIdAndStatus(Mockito.anyString(), Mockito.eq(ProductMigrationType.COMMON_IMAGE_MIGRATION.name()), Mockito.anyInt()))
        .thenReturn(Collections.singletonList(productMigration));

    productMigrationService.findProductMigrationMigrationByStoreIdAndStatus(STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), 2);
    Mockito.verify(productMigrationRepository).findByStoreIdAndStatus(STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(),2);
  }

  @Test
   void findCommonImageMigrationByStoreIdAndStatusEmptyStoreIdTest() {
    Assertions.assertThrows(Exception.class,
      () -> productMigrationService.findProductMigrationMigrationByStoreIdAndStatus(StringUtils.EMPTY, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(),2));
  }

  @Test
   void findCommonImageMigrationByStoreIdAndStatusEmptyMigrationTypeTest() {
    Assertions.assertThrows(Exception.class,
      () -> productMigrationService.findProductMigrationMigrationByStoreIdAndStatus(STORE_ID,
        StringUtils.EMPTY,2));
  }
}
