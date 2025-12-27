package com.gdn.x.productcategorybase.service.impl;

import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.entity.ProductMigration;
import com.gdn.x.productcategorybase.repository.ProductMigrationRepository;
import com.gdn.x.productcategorybase.service.ProductMigrationService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductMigrationMigrationServiceImpl implements ProductMigrationService {

  @Autowired
  private ProductMigrationRepository productMigrationRepository;

  @Override
  public ProductMigration findProductMigrationByProductCodeAndStatusAndMigrationType(String productCode, String status, String migrationType) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productCode),
        ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(status), ErrorMessage.STATUS_MUST_NOT_BE_EMPTY.getMessage());
    ProductMigration commonImageMigration = productMigrationRepository.findByProductCodeAndStatusAndMigrationType(productCode, status, migrationType);
    return commonImageMigration;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public ProductMigration saveProductMigration(ProductMigration productMigration) {
    return productMigrationRepository.saveAndFlush(productMigration);
  }

  @Override
  public List<ProductMigration> findProductMigrationByProductCodes(String storeId, String migrationType,
      List<String> productCodes) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessage.STORE_ID_MUST_NOT_BE_EMPTY.getMessage());
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productCodes),
        ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_EMPTY.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(migrationType),
        ErrorMessage.MIGRATION_TYPE_MUST_NOT_BE_EMPTY.getMessage());
    return productMigrationRepository.findProductMigrationByStoreIdAndMigrationTypeAndProductCodeIn(storeId,
        migrationType, productCodes);
  }

  @Override
  public List<ProductMigration> findProductMigrationByStoreIdAndStatus(String storeId, String migrationType,
      int limit) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessage.STORE_ID_MUST_NOT_BE_EMPTY.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(migrationType),
        ErrorMessage.MIGRATION_TYPE_MUST_NOT_BE_EMPTY.getMessage());
    return productMigrationRepository.findByStoreIdAndStatusPendingAndMigrationType(storeId, migrationType, limit);
  }
}
