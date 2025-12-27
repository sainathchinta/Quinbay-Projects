package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.ProductMigrationStatus;
import com.gdn.x.productcategorybase.ProductMigrationType;
import com.gdn.x.productcategorybase.entity.ProductMigration;
import com.gdn.x.productcategorybase.repository.ProductMigrationRepository;

public class ProductMigrationServiceTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String STORE_ID = "stoteId";

  private ProductMigration productMigration;

  @Mock
  private ProductMigrationRepository productMigrationRepository;

  @InjectMocks
  private ProductMigrationMigrationServiceImpl productMigrationMigrationServiceImpl;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    productMigration = ProductMigration.builder().productCode(PRODUCT_CODE)
      .migrationType(ProductMigrationType.COMMON_IMAGE_MIGRATION.name())
      .status(ProductMigrationStatus.PUBLISHED.name()).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productMigrationRepository);
  }

  @Test
  public void findCommonImageMigrationByProductCodeAndStatusTest() {
    Mockito.when(productMigrationRepository.findByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
        ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.COMMON_IMAGE_MIGRATION.name())).thenReturn(productMigration);
    ProductMigration commonImageMigration =
        productMigrationMigrationServiceImpl.findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
            ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.COMMON_IMAGE_MIGRATION.name());
    Mockito.verify(productMigrationRepository)
        .findByProductCodeAndStatusAndMigrationType(PRODUCT_CODE, ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.COMMON_IMAGE_MIGRATION.name());
    Assertions.assertEquals(this.productMigration, commonImageMigration);
  }

  @Test
  public void findCommonImageMigrationByProductCodeAndStatusProductCodeEmptyTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> productMigrationMigrationServiceImpl.findProductMigrationByProductCodeAndStatusAndMigrationType(StringUtils.EMPTY,
        ProductMigrationStatus.PUBLISHED.name(), ProductMigrationType.COMMON_IMAGE_MIGRATION.name()));
  }

  @Test
  public void findCommonImageMigrationByProductCodeAndStatusEmptyTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> productMigrationMigrationServiceImpl.findProductMigrationByProductCodeAndStatusAndMigrationType(PRODUCT_CODE,
        StringUtils.EMPTY, ProductMigrationType.COMMON_IMAGE_MIGRATION.name()));
  }

  @Test
  public void saveCommonImageMigrationTest() {
    Mockito.when(productMigrationRepository.saveAndFlush(productMigration)).thenReturn(productMigration);
    ProductMigration commonImageMigration =
        productMigrationMigrationServiceImpl.saveProductMigration(this.productMigration);
    Mockito.verify(productMigrationRepository).saveAndFlush(commonImageMigration);
    Assertions.assertEquals(this.productMigration, commonImageMigration);
  }

  @Test
  public void findCommonImageMigrationByProductCodesAndStatusTest() {
    Mockito.when(
            productMigrationRepository.findProductMigrationByStoreIdAndMigrationTypeAndProductCodeIn(Mockito.anyString(),
                Mockito.eq(ProductMigrationType.COMMON_IMAGE_MIGRATION.name()), Mockito.anyList()))
        .thenReturn(Arrays.asList(productMigration));
    List<ProductMigration> result = productMigrationMigrationServiceImpl.findProductMigrationByProductCodes(STORE_ID,
        ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productMigrationRepository)
        .findProductMigrationByStoreIdAndMigrationTypeAndProductCodeIn(STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), Arrays.asList(PRODUCT_CODE));
  }

  @Test
  public void findCommonImageMigrationByProductCodesAndStatusStoreIdEmptyTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        productMigrationMigrationServiceImpl.findProductMigrationByProductCodes(StringUtils.EMPTY, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), Arrays.asList(PRODUCT_CODE)));
  }

  @Test
  public void findCommonImageMigrationByProductCodesAndStatusProductCodeEmptyTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        productMigrationMigrationServiceImpl.findProductMigrationByProductCodes(STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), new ArrayList<>()));
  }

  @Test
  public void findCommonImageMigrationByProductCodesAndStatusMigrationTypeEmptyTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        productMigrationMigrationServiceImpl.findProductMigrationByProductCodes(STORE_ID, StringUtils.EMPTY, Arrays.asList(PRODUCT_CODE)));
  }

  @Test
  public void findCommonImageMigrationByStoreIdAndStatusTest() {
    Mockito.when(productMigrationRepository.findByStoreIdAndStatusPendingAndMigrationType(Mockito.anyString(), Mockito.eq(ProductMigrationType.COMMON_IMAGE_MIGRATION.name()), Mockito.anyInt()))
        .thenReturn(Arrays.asList(productMigration));
    List<ProductMigration> result =
        productMigrationMigrationServiceImpl.findProductMigrationByStoreIdAndStatus(STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), 2);
    Mockito.verify(productMigrationRepository).findByStoreIdAndStatusPendingAndMigrationType(STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(),2);
  }

  @Test
  public void findCommonImageMigrationByStoreIdAndStatusEmptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        productMigrationMigrationServiceImpl.findProductMigrationByStoreIdAndStatus(StringUtils.EMPTY, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(),2));
  }

  @Test
  public void findCommonImageMigrationByStoreIdAndStatusEmptyMigrationTypeTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        productMigrationMigrationServiceImpl.findProductMigrationByStoreIdAndStatus(STORE_ID, StringUtils.EMPTY,2));
  }
}
