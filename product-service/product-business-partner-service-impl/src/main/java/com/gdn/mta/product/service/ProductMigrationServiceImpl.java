package com.gdn.mta.product.service;

import java.util.Date;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.entity.ProductMigration;
import com.gdn.mta.product.enums.MigrationStatus;
import com.gdn.mta.product.repository.ProductMigrationRepository;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@Transactional
public class ProductMigrationServiceImpl implements ProductMigrationService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductMigrationService.class);

  @Autowired
  private ProductMigrationRepository productMigrationRepository;

  @Override
  public List<ProductMigration> getProductMigrationByProductSkus(List<String> productSkus) throws Exception {
    List<ProductMigration> productMigrations = productMigrationRepository.findByGdnProductSkuIn(productSkus);
    GdnPreconditions
        .checkArgument(CollectionUtils.isNotEmpty(productMigrations), ErrorMessages.PRODUCT_MIGRATION_NOT_FOUND);
   return productMigrations;
  }

  @Override
  public List<ProductMigration> getProductMigrationByProductSkusWithStatus(List<String> productSkus, String migrationStatus) throws Exception {
    List<ProductMigration> productMigrations = productMigrationRepository
        .findByGdnProductSkuInAndMigrationStatus(productSkus, MigrationStatus.PENDING.getMigrationStatus());
    GdnPreconditions
        .checkArgument(CollectionUtils.isNotEmpty(productMigrations), ErrorMessages.PRODUCT_MIGRATION_NOT_FOUND);
    return productMigrations;
  }

  @Override
  public List<ProductMigration> findByProductCodes(List<String> productCodes) throws Exception {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productCodes), ErrorMessages.PRODUCT_LIST_EMPTY);
    log.info("Fetching the products for migration for product codes : {}", productCodes);
    List<ProductMigration> productMigrations = productMigrationRepository.findByProductCodeIn(productCodes);
    return productMigrations;
  }

  @Override
  public List<ProductMigration> findByProductCodesWithStatus(List<String> productCodes, String migrationStatus) throws Exception {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productCodes), ErrorMessages.PRODUCT_LIST_EMPTY);
    log.info("Fetching the products for migration for product codes : {} and migrationStatus : {}", productCodes,
        migrationStatus);
    List<ProductMigration> productMigrations = productMigrationRepository
        .findByProductCodeInAndMigrationStatus(productCodes, migrationStatus);
    return productMigrations;
  }

  @Override
  @Transactional(readOnly = false)
  public void updateBusinessPartnerDetails(String businessPartnerId, String merchantType, String merchantStatus,
      String businessPartnerName, List<String> gdnProductSkuList) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(gdnProductSkuList), ErrorMessages.PRODUCT_LIST_EMPTY);
    productMigrationRepository
        .updateBusinessPartnerDetails(businessPartnerId, merchantType, merchantStatus, businessPartnerName,
            gdnProductSkuList);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public ProductMigration save(ProductMigration productMigration) {
    if (StringUtils.isNotBlank(productMigration.getId())) {
      ProductMigration existingProductMigration = productMigrationRepository.findById(productMigration.getId()).orElse(null);
      BeanUtils.copyProperties(productMigration, existingProductMigration);
      return productMigrationRepository.save(existingProductMigration);
    }
    return productMigrationRepository.save(productMigration);
  }

  @Override
  public List<String> findDistinctProductCodesForMigration(int limit) {
    log.info("Fetching the product codes for migration with limit : {}", limit);
    List<String> productCodes = productMigrationRepository.findDistinctProductCodesForMigration(limit);
    return productCodes;
  }

  @Override
  public List<String> findProductForMigrationWithNullProductCode(int limit) {
    log.info("Fetching the products skus with null product code for migration with limit : {}", limit);
    List<String> productSkus = productMigrationRepository.findProductSkusForMigration(limit);
    return productSkus;
  }

  @Override
  public Page<ProductMigration> findFailedProductMigration(String migrationStatus, int batchSize, int retryCount) {
    return productMigrationRepository.findByMigrationStatusAndRetryCountLessThanOrderByUpdatedDateDesc(migrationStatus, retryCount, PageRequest.of(0, batchSize));
  }

  @Override
  public void updateProductMigrationStatusByProductCodes(List<String> productCodes, String migrationStatus) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productCodes), ErrorMessages.PRODUCT_LIST_EMPTY);
    log.info("Updating the migration status to {} for products: {}", migrationStatus, productCodes);
    productMigrationRepository.updateProductMigrationStatusByProductCodes(productCodes, migrationStatus);
  }

  @Override
  public void updateProductMigrationStatusByProductSkus(List<String> productSkus, String migrationStatus) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productSkus), ErrorMessages.PRODUCT_LIST_EMPTY);
    log.info("Updating the migration status to {} for products: {}", migrationStatus, productSkus);
    productMigrationRepository.updateProductMigrationStatusByProductSkus(productSkus, migrationStatus);
  }

  @Override
  public void updateProductMigrationStatus(String oldMigrationStatus, String migrationStatus, Date updatedDate) {
    productMigrationRepository.updateProductMigrationStatus(oldMigrationStatus, migrationStatus, updatedDate);
  }
}
