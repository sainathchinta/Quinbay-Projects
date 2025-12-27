package com.gdn.x.mta.distributiontask.service.impl;

import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.mta.distributiontask.dao.api.ProductMigrationRepository;
import com.gdn.x.mta.distributiontask.model.ErrorMessages;
import com.gdn.x.mta.distributiontask.model.ProductMigration;
import com.gdn.x.mta.distributiontask.service.api.ProductMigrationService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductMigrationServiceImpl implements ProductMigrationService {

  @Autowired
  private ProductMigrationRepository productMigrationRepository;

  @Override
  public ProductMigration findProductMigrationByProductCodeAndStatus(String productCode, String status) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productCode), ErrorMessages.PRODUCT_CODE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(status), ErrorMessages.STATUS_MUST_NOT_BE_EMPTY);
    ProductMigration commonImageMigration = productMigrationRepository.findByProductCodeAndStatus(productCode, status);
    return commonImageMigration;
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public ProductMigration saveProductMigration(ProductMigration commonImageMigration) {
    return productMigrationRepository.save(commonImageMigration);
  }

  @Override
  public List<ProductMigration> findProductMigrationMigrationByStoreIdAndStatus(String storeId, String migrationType,
      int limit) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(migrationType),
        ErrorMessages.MIGRATION_TYPE_MUST_NOT_BE_EMPTY);
    return productMigrationRepository.findByStoreIdAndStatus(storeId, migrationType, limit);
  }

  @Override
  public List<ProductMigration> findProductMigrationByProductCodes(String storeId, String migrationType,
      List<String> productCodes) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productCodes),
        ErrorMessages.PRODUCT_CODE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(migrationType),
        ErrorMessages.MIGRATION_TYPE_MUST_NOT_BE_EMPTY);
    return productMigrationRepository.findProductMigrationByStoreIdAndMigrationTypeAndProductCodeIn(storeId,
        migrationType, productCodes);
  }
}
