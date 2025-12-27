package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.ProductAndL5MigrationRequest;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.partners.product.orchestrator.constant.ProductLevel1State;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.entity.ProductMigration;
import com.gdn.mta.product.enums.MigrationStatus;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductMigrationWrapperServiceImpl implements ProductMigrationWrapperService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductMigrationWrapperService.class);

  List<String> ignoredMerchantTypes = Arrays.asList("REJECT", "RESIGN", "TERMINATE");

  private final ProductMigrationService productMigrationService;

  private final ProductService productService;

  private final XProductOutbound xProductOutbound;

  private final ProductOutbound productOutbound;

  private final ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  public ProductMigrationWrapperServiceImpl(ProductMigrationService productMigrationService,
    ProductService productService, XProductOutbound xProductOutbound,
    ProductOutbound productOutbound, ProductBusinessPartnerService productBusinessPartnerService) {
    this.productMigrationService = productMigrationService;
    this.productService = productService;
    this.xProductOutbound = xProductOutbound;
    this.productOutbound = productOutbound;
    this.productBusinessPartnerService = productBusinessPartnerService;
  }


  @Override
  public void updateMigratedProductCode(List<String> productSkus) throws Exception {
    List<ProductMigration> productMigrations = productMigrationService.getProductMigrationByProductSkus(productSkus);
    LOGGER.info("Rolling back migrated productSkus : {}", productSkus);
    for (ProductMigration productMigration : productMigrations) {
      xProductOutbound
          .updateMigratedProductCode(productMigration.getGdnProductSku(), productMigration.getProductCode(),
              true);
    }
  }

  @Override
  public Integer migrateProductByProductCode(String productCode, boolean checkStatus) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productCode), ErrorMessages.PRODUCT_CODE_BLANK);
    List<ProductMigration> productMigrations = null;
    if (checkStatus) {
      productMigrations = productMigrationService
          .findByProductCodesWithStatus(Arrays.asList(productCode), MigrationStatus.PENDING.getMigrationStatus());
    } else {
      productMigrations = productMigrationService.findByProductCodes(Arrays.asList(productCode));
    }

    if (CollectionUtils.isEmpty(productMigrations)) {
      log.warn("Skipping migration of product code because product was already processed  : {}", productCode);
      return 0;
    }
    productMigrationService.updateProductMigrationStatusByProductCodes(Arrays.asList(productCode),
        MigrationStatus.IN_PROGRESS.getMigrationStatus());
    Set<String> businessPartnerIds =
        productMigrations.stream().map(productMigration -> productMigration.getBusinessPartnerId())
            .collect(Collectors.toSet());

    Map<String, BusinessPartnerResponse> businessPartnerResponseMap = new HashMap<>();
    List<BusinessPartnerResponse> businessPartnerResponses =
        xProductOutbound.getBusinessPartnerDetails(new ArrayList<>(businessPartnerIds));
    businessPartnerResponses.stream().forEach(businessPartnerResponse -> businessPartnerResponseMap
        .put(businessPartnerResponse.getBusinessPartnerCode(), businessPartnerResponse));

    return checkProductMigrationEligibility(productMigrations, businessPartnerResponseMap);
  }

  @Override
  public void retryFailedMigratedProductsByProductCodes(List<String> productCodes) throws Exception {
    List<ProductMigration> productMigrations =
        productMigrationService.findByProductCodesWithStatus(productCodes, MigrationStatus.FAILED.getMigrationStatus());
    retryFailedMigration(productMigrations);
  }

  @Override
  public void retryFailedMigratedProducts(int batchSize, int retryCount) throws Exception {
    Page<ProductMigration> productMigrations = productMigrationService
        .findFailedProductMigration(MigrationStatus.FAILED.getMigrationStatus(), batchSize, retryCount);
    retryFailedMigration(productMigrations.getContent());
  }

  @Override
  public void migrateProductAndL5DetailsByProductSku(String storeId,
    ProductAndL5MigrationRequest productAndL5MigrationRequest, String username) throws Exception {
    GdnPreconditions.checkArgument(
      StringUtils.isNotBlank(productAndL5MigrationRequest.getProductSku()),
      ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY);
    productService.migrateProductAndL5Details(storeId, productAndL5MigrationRequest, username);
  }

  private void retryFailedMigration(List<ProductMigration> productMigrations) {
    for (ProductMigration productMigration : productMigrations) {
      try {
        productMigration.setRetryCount(productMigration.getRetryCount() + 1);
        ProductAndItemsResponse productAndItemsResponse =
            xProductOutbound.getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true, false).getValue();
        if (StringUtils.isNotBlank(productMigration.getProductCode()) && StringUtils
            .isBlank(productMigration.getMerchantType())) {
          List<BusinessPartnerResponse> businessPartnerResponses =
              xProductOutbound.getBusinessPartnerDetails(Arrays.asList(productMigration.getBusinessPartnerId()));
          if (ignoredMerchantTypes.contains(businessPartnerResponses.get(0).getMerchantStatus())
              || !Constants.CM_MERCHANT.equalsIgnoreCase(businessPartnerResponses.get(0).getMerchantType())) {
            productMigration.setBusinessPartnerName(businessPartnerResponses.get(0).getBusinessPartnerName());
            productMigration.setMerchantStatus(businessPartnerResponses.get(0).getMerchantStatus());
            productMigration.setMerchantType(businessPartnerResponses.get(0).getMerchantType());
            productMigration.setMigrationStatus(MigrationStatus.NO_MIGRATION.getMigrationStatus());
            productMigrationService.save(productMigration);
            continue;
          }
          productMigration.setBusinessPartnerName(businessPartnerResponses.get(0).getBusinessPartnerName());
          productMigration.setMerchantStatus(businessPartnerResponses.get(0).getMerchantStatus());
          productMigration.setMerchantType(businessPartnerResponses.get(0).getMerchantType());
        }
        if (StringUtils.isEmpty(productMigration.getMigratedProductCode())) {
          checkSyncStatusAndGenerateProductCode(productMigration, productAndItemsResponse);
        } else if (StringUtils.isEmpty(productMigration.getMigratedProductId())) {
          retryProductMigrationInPCB(productMigration, productAndItemsResponse);
          updateL3Data(productMigration);
          productPublish(productMigration);
          saveSuccessfulProductMigrationAndHistory(productMigration);
        } else if (!productMigration.isL3Updated()) {
          updateL3Data(productMigration);
          productPublish(productMigration);
          saveSuccessfulProductMigrationAndHistory(productMigration);
        } else if (!productMigration.isProductPublished()) {
          productPublish(productMigration);
          saveSuccessfulProductMigrationAndHistory(productMigration);
        }
      } catch (Exception e) {
        LOGGER.error("Error while retrying migrating the product sku : {} with details : {}",
            productMigration.getGdnProductSku(), productMigration, e);
        productMigration.setMigrationStatus(MigrationStatus.FAILED.getMigrationStatus());
        productMigrationService.save(productMigration);
      }
    }
  }

  private void retryProductMigrationInPCB(ProductMigration productMigration,
      ProductAndItemsResponse productAndItemsResponse) throws Exception {
    if (productMigration.isSyncStatus()) {
      String productId = productOutbound
          .migrateProduct(productMigration.getProductCode(), productMigration.getMigratedProductCode(),
              productMigration.getBusinessPartnerId(), null);
      productMigration.setMigratedProductId(productId);
    } else {
      ProductRequest productRequest = ConverterUtil.toProductRequest(productAndItemsResponse);
      String productId = productOutbound
          .migrateProduct(null, productMigration.getMigratedProductCode(),
              productMigration.getBusinessPartnerId(), productRequest);
      productMigration.setMigratedProductId(productId);
    }
    productService.updateProductCollectionForMigratedProduct(productMigration, productAndItemsResponse);
  }

  private void updateL3Data(ProductMigration productMigration) throws Exception {
    xProductOutbound
        .updateMigratedProductCode(productMigration.getGdnProductSku(), productMigration.getMigratedProductCode(),
            false);
    productMigration.setL3Updated(true);
  }

  private void productPublish(ProductMigration productMigration) throws Exception {
    productOutbound.republishProduct(Constants.MIGRATION, Arrays.asList(productMigration.getMigratedProductCode()));
    productMigration.setProductPublished(true);
  }

  private void checkSyncStatusAndGenerateProductCode(ProductMigration productMigration,
      ProductAndItemsResponse productAndItemsResponse) throws Exception {
    if (productAndItemsResponse.getProduct().isMarkForDelete() && !productAndItemsResponse.getProduct().isSuspended()) {
      log.warn("Product sku is in deleted state for product code : {} and product sku : {}",
          productMigration.getProductCode(), productMigration.getGdnProductSku());
      productMigration.setMigrationStatus(MigrationStatus.NO_MIGRATION.getMigrationStatus());
      productMigrationService.save(productMigration);
    } else {
      if (!productAndItemsResponse.getProduct().isSynchronized()) {
        productMigration.setSyncStatus(false);
      } else {
        productMigration.setSyncStatus(true);
      }
      String newProductCode = productService.generateProductCode();
      productMigration.setMigratedProductCode(newProductCode);
      retryProductMigrationInPCB(productMigration, productAndItemsResponse);
      updateL3Data(productMigration);
      productPublish(productMigration);
      saveSuccessfulProductMigrationAndHistory(productMigration);
    }
  }

  private void saveSuccessfulProductMigrationAndHistory(ProductMigration productMigration) {
    productService.saveMasterProductMigrationHistory(productMigration);
    productMigration.setMigrationStatus(MigrationStatus.COMPLETED.getMigrationStatus());
    productMigrationService.save(productMigration);
  }

  private Integer checkProductMigrationEligibility(List<ProductMigration> productMigrations,
      Map<String, BusinessPartnerResponse> businessPartnerResponseMap) {
    int countNonCMProducts = 0;
    int countCMNonMigratedProducts = 0;
    List<ProductMigration> syncProductsToMigrate = new ArrayList<>();
    Map<String, ProductMigration> unsyncProductsToMigrate = new LinkedHashMap<>();
    Map<String, ProductAndItemsResponse> productAndItemsResponseHashMap = new LinkedHashMap<>();

    for (ProductMigration productMigration : productMigrations) {
      productMigration.setMigrationStatus(MigrationStatus.IN_PROGRESS.getMigrationStatus());
      BusinessPartnerResponse businessPartnerResponse =
          businessPartnerResponseMap.get(productMigration.getBusinessPartnerId());
      if (checkNullBusinessPartnerResponse(productMigration, businessPartnerResponse)) {
        continue;
      }

      if (Constants.CM_MERCHANT.equalsIgnoreCase(businessPartnerResponse.getMerchantType())) {
        if (ignoredMerchantTypes.contains(businessPartnerResponse.getMerchantStatus()) || StringUtils
            .isBlank(productMigration.getGdnProductSku())) {
          log.info("CM merchant belong to deleted state or product sku is null. Product code : {} and product sku : {}",
              productMigration.getProductCode(), productMigration.getGdnProductSku());
          productMigration.setMigrationStatus(MigrationStatus.NO_MIGRATION.getMigrationStatus());
          productMigration.setMerchantType(businessPartnerResponse.getMerchantType());
          productMigration.setMerchantStatus(businessPartnerResponse.getMerchantStatus());
          productMigration.setBusinessPartnerName(businessPartnerResponse.getBusinessPartnerName());
          productMigrationService.save(productMigration);
          countCMNonMigratedProducts++;
          continue;
        }
      } else {
        log.info("Not a CM merchant's product. product code : {} and product sku : {}",
            productMigration.getProductCode(), productMigration.getGdnProductSku());
        productMigration.setMigrationStatus(MigrationStatus.NO_MIGRATION.getMigrationStatus());
        productMigration.setMerchantType(businessPartnerResponse.getMerchantType());
        productMigration.setMerchantStatus(businessPartnerResponse.getMerchantStatus());
        productMigration.setBusinessPartnerName(businessPartnerResponse.getBusinessPartnerName());
        productMigrationService.save(productMigration);
        countNonCMProducts++;
        continue;
      }

      productMigration.setMerchantType(businessPartnerResponse.getMerchantType());
      productMigration.setMerchantStatus(businessPartnerResponse.getMerchantStatus());
      productMigration.setBusinessPartnerName(businessPartnerResponse.getBusinessPartnerName());
      ProductAndItemsResponse productAndItemsResponse = null;
      try {
        productAndItemsResponse =
            xProductOutbound.getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true, false).getValue();
      } catch (Exception e) {
        log.warn("Error while fetching the x-product response for product code : {} and product sku : {}",
            productMigration.getProductCode(), productMigration.getGdnProductSku(), e);
        productMigration.setMigrationStatus(MigrationStatus.FAILED.getMigrationStatus());
        productMigrationService.save(productMigration);
        continue;
      }

      if (productAndItemsResponse.getProduct().isMarkForDelete() && !productAndItemsResponse.getProduct()
          .isSuspended()) {
        log.warn("Product sku is in deleted state for product code : {} and product sku : {}",
            productMigration.getProductCode(), productMigration.getGdnProductSku());
        productMigration.setMigrationStatus(MigrationStatus.NO_MIGRATION.getMigrationStatus());
        productMigrationService.save(productMigration);
        continue;
      } else {
        if (!productAndItemsResponse.getProduct().isSynchronized()) {
          productMigration.setSyncStatus(false);
          unsyncProductsToMigrate.put(productMigration.getGdnProductSku(), productMigration);
          productAndItemsResponseHashMap.put(productMigration.getGdnProductSku(), productAndItemsResponse);
        } else {
          productMigration.setSyncStatus(true);
          syncProductsToMigrate.add(productMigration);
        }
      }
    }

    if(syncProductsToMigrate.size() > 0 || unsyncProductsToMigrate.size() > 0) {
      migrateSyncAndUnsyncProducts(countNonCMProducts, countCMNonMigratedProducts, syncProductsToMigrate,
          unsyncProductsToMigrate, productAndItemsResponseHashMap);
    }
    return syncProductsToMigrate.size() + unsyncProductsToMigrate.size();
  }

  private void migrateSyncAndUnsyncProducts(int countNonCMProducts, int countCMNonMigratedProducts,
      List<ProductMigration> syncProductsToMigrate, Map<String, ProductMigration> unsyncProductsToMigrate,
      Map<String, ProductAndItemsResponse> productAndItemsResponseHashMap) {
    if (countCMNonMigratedProducts > 0 || countNonCMProducts > 0) {
      syncProductsToMigrate.stream().forEach(productMigration -> migrateSyncProduct(productMigration));
      for (Map.Entry<String, ProductMigration> entry : unsyncProductsToMigrate.entrySet()) {
        migrateUnsyncProduct(unsyncProductsToMigrate.get(entry.getKey()),
            productAndItemsResponseHashMap.get(entry.getKey()));
      }
    } else {
      if (syncProductsToMigrate.size() == 0 && unsyncProductsToMigrate.size() > 0) {
        int count = 0;
        for (Map.Entry<String, ProductMigration> entry : unsyncProductsToMigrate.entrySet()) {
          if (count == unsyncProductsToMigrate.size()-1) {
            updateUnsyncProduct(unsyncProductsToMigrate.get(entry.getKey()),
                productAndItemsResponseHashMap.get(entry.getKey()));
          } else {
            migrateUnsyncProduct(unsyncProductsToMigrate.get(entry.getKey()),
                productAndItemsResponseHashMap.get(entry.getKey()));
          }
          count++;
        }
      } else {
        ProductMigration productMigration = syncProductsToMigrate.get(0);
        log.warn("No need of migration of product product code : {} productSku : {}",
            productMigration.getProductCode(), productMigration.getGdnProductSku());
        productMigration.setMigrationStatus(MigrationStatus.NO_MIGRATION.getMigrationStatus());
        productMigrationService.save(productMigration);
        syncProductsToMigrate.remove(0);
        syncProductsToMigrate.stream().forEach(productMigration1 -> migrateSyncProduct(productMigration1));
        for (Map.Entry<String, ProductMigration> entry : unsyncProductsToMigrate.entrySet()) {
          migrateUnsyncProduct(unsyncProductsToMigrate.get(entry.getKey()),
              productAndItemsResponseHashMap.get(entry.getKey()));
        }
      }
    }
  }

  private Integer checkProductMigrationEligibilityForNullProductCodes(List<ProductMigration> productMigrations,
      Map<String, BusinessPartnerResponse> businessPartnerResponseMap) {
    int migratedProductCount = 0;
    for (ProductMigration productMigration : productMigrations) {
      productMigration.setMigrationStatus(MigrationStatus.IN_PROGRESS.getMigrationStatus());
      BusinessPartnerResponse businessPartnerResponse =
          businessPartnerResponseMap.get(productMigration.getBusinessPartnerId());
      if (checkNullBusinessPartnerResponse(productMigration, businessPartnerResponse)) {
        continue;
      }

      if (ignoredMerchantTypes.contains(businessPartnerResponse.getMerchantStatus())) {
        log.info("Merchant belongs to deleted state or product sku is null. Product code : {} and product sku : {}",
            productMigration.getProductCode(), productMigration.getGdnProductSku());
        productMigration.setMigrationStatus(MigrationStatus.NO_MIGRATION.getMigrationStatus());
        productMigration.setMerchantType(businessPartnerResponse.getMerchantType());
        productMigration.setMerchantStatus(businessPartnerResponse.getMerchantStatus());
        productMigrationService.save(productMigration);
        continue;
      }

      productMigration.setMerchantType(businessPartnerResponse.getMerchantType());
      productMigration.setMerchantStatus(businessPartnerResponse.getMerchantStatus());
      productMigration.setBusinessPartnerName(businessPartnerResponse.getBusinessPartnerName());
      ProductAndItemsResponse productAndItemsResponse = null;
      try {
        productAndItemsResponse =
            xProductOutbound.getProductAndItemsWithProductData(true, productMigration.getGdnProductSku(), true, false).getValue();
      } catch (Exception e) {
        log.warn("Error while fetching the x-product response for product code : {} and product sku : {}",
            productMigration.getProductCode(), productMigration.getGdnProductSku(), e);
        productMigration.setMigrationStatus(MigrationStatus.FAILED.getMigrationStatus());
        productMigrationService.save(productMigration);
        continue;
      }

      if (productAndItemsResponse.getProduct().isMarkForDelete() && !productAndItemsResponse.getProduct()
          .isSuspended()) {
        log.warn("Product sku is in deleted state for product code : {} and product sku : {}",
            productMigration.getProductCode(), productMigration.getGdnProductSku());
        productMigration.setMigrationStatus(MigrationStatus.NO_MIGRATION.getMigrationStatus());
        productMigrationService.save(productMigration);
        continue;
      } else {
        migrateUnsyncProduct(productMigration, productAndItemsResponse);
        migratedProductCount++;
      }
    }
    return migratedProductCount;
  }

  private boolean checkNullBusinessPartnerResponse(ProductMigration productMigration,
      BusinessPartnerResponse businessPartnerResponse) {
    if (Objects.isNull(businessPartnerResponse)) {
      log.warn("Business partner not found for product code : {} and product sku : {}",
          productMigration.getProductCode(), productMigration.getGdnProductSku());
      productMigration.setMigrationStatus(MigrationStatus.NO_MIGRATION.getMigrationStatus());
      productMigrationService.save(productMigration);
      return true;
    }
    return false;
  }

  private void migrateSyncProduct(ProductMigration productMigration) {
    try {
      //generate new productCode
      String newProductCode = productService.generateProductCode();
      productMigration.setSyncStatus(true);
      productMigration.setMigratedProductCode(newProductCode);
      productMigration.setMigrationStatus(MigrationStatus.IN_PROGRESS.getMigrationStatus());
      productMigration = productMigrationService.save(productMigration);

      // call PCB to create a new product
      String productId = productOutbound
          .migrateProduct(productMigration.getProductCode(), productMigration.getMigratedProductCode(),
              productMigration.getBusinessPartnerId(), null);
      productMigration.setMigratedProductId(productId);

      //Update the details in PBP
      productService.updateProductCollectionForMigratedProduct(productMigration, null);

      // updating new product details in x-product
      updateL3Data(productMigration);

      // productPublish
      productPublish(productMigration);

      //save history
      saveSuccessfulProductMigrationAndHistory(productMigration);
    } catch (Exception e) {
      LOGGER.error("Error while migrating the product sku : {} with details : {} ", productMigration.getGdnProductSku(),
          productMigration, e);
      productMigration.setMigrationStatus(MigrationStatus.FAILED.getMigrationStatus());
      productMigrationService.save(productMigration);
    }
  }

  private void migrateUnsyncProduct(ProductMigration productMigration,
      ProductAndItemsResponse productAndItemsResponse) {
    try {
      //generate new productCode
      String newProductCode = productService.generateProductCode();
      productMigration.setSyncStatus(false);
      productMigration.setMigratedProductCode(newProductCode);
      productMigration.setMigrationStatus(MigrationStatus.IN_PROGRESS.getMigrationStatus());
      productMigration = productMigrationService.save(productMigration);

      //create ProductRequest from productAndItemsResponse
      ProductRequest productRequest = ConverterUtil.toProductRequest(productAndItemsResponse);

      // copyProduct
      String productId = productOutbound
          .migrateProduct(null, productMigration.getMigratedProductCode(), productMigration.getBusinessPartnerId(),
              productRequest);
      productMigration.setMigratedProductId(productId);

      //Update the details in PBP
      productService.updateProductCollectionForMigratedProduct(productMigration, productAndItemsResponse);

      // updating new product details in x-product
      updateL3Data(productMigration);

      // productPublish
      productPublish(productMigration);

      //save history
      saveSuccessfulProductMigrationAndHistory(productMigration);
    } catch (Exception e) {
      LOGGER.error("Error while migrating the product sku : {} with details : {}", productMigration.getGdnProductSku(),
          productMigration, e);
      productMigration.setMigrationStatus(MigrationStatus.FAILED.getMigrationStatus());
      productMigrationService.save(productMigration);
    }
  }

  private void updateUnsyncProduct(ProductMigration productMigration, ProductAndItemsResponse productAndItemsResponse) {
    try {
      productMigration.setSyncStatus(false);
      productMigration.setMigrationStatus(MigrationStatus.IN_PROGRESS.getMigrationStatus());
      productMigration = productMigrationService.save(productMigration);

      //create ProductRequest from productAndItemsResponse
      ProductRequest productRequest = ConverterUtil.toProductRequest(productAndItemsResponse);

      //updating the product content
      String productId = productOutbound
          .migrateProduct(productMigration.getProductCode(), productMigration.getProductCode(),
              productMigration.getBusinessPartnerId(), productRequest);
      productMigration.setMigratedProductId(productId);
      productMigration.setMigratedProductCode(productMigration.getProductCode());
      productService.saveSyncHistoryForItems(productMigration);
      productMigration.setMigrationStatus(MigrationStatus.UPDATE_COMPLETED.getMigrationStatus());
      productMigrationService.save(productMigration);
    } catch (Exception e) {
      LOGGER.error("Error while updating the product sku during migration : {} with details : {}",
          productMigration.getGdnProductSku(), productMigration, e);
      productMigration.setMigrationStatus(MigrationStatus.UPDATE_FAILED.getMigrationStatus());
      productMigrationService.save(productMigration);
    }
  }

  @Override
  public Integer migrateProductByProductSkus(List<String> productSkus) throws Exception {
    List<ProductMigration> productMigrations = productMigrationService
        .getProductMigrationByProductSkusWithStatus(productSkus, MigrationStatus.PENDING.getMigrationStatus());
    Set<String> businessPartnerIds =
        productMigrations.stream().map(productMigration -> productMigration.getBusinessPartnerId())
            .collect(Collectors.toSet());
    Map<String, BusinessPartnerResponse> businessPartnerResponseMap = new HashMap<>();
    List<BusinessPartnerResponse> businessPartnerResponses =
        xProductOutbound.getBusinessPartnerDetails(new ArrayList<>(businessPartnerIds));
    businessPartnerResponses.stream().forEach(businessPartnerResponse -> businessPartnerResponseMap
        .put(businessPartnerResponse.getBusinessPartnerCode(), businessPartnerResponse));
    return checkProductMigrationEligibilityForNullProductCodes(productMigrations, businessPartnerResponseMap);
  }
}
