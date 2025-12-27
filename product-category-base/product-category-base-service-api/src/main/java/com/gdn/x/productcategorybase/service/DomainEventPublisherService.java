package com.gdn.x.productcategorybase.service;

import java.util.List;
import java.util.Set;

import com.gdn.x.productcategorybase.domain.event.model.BrandHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.InternalProductHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.VendorPublishEventModel;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import org.apache.commons.lang3.tuple.Pair;

import com.gdn.x.productcategorybase.CategoryChangeEventType;
import com.gdn.x.productcategorybase.ProductMasterEvent;
import com.gdn.x.productcategorybase.domain.event.model.AggregateImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductItemDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.BrandApprovedOrRejectedDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.BrandAuthDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.BrandDeleteDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.BrandDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeExtractionModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCreationFailureDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductMasterEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.gdn.x.productcategorybase.domain.event.model.ProductScoreUpdateDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBatchPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBrandListDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBatchPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBrandDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrUpdateBrandDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.TerminatedSellerSkuCleanupStatusEventModel;
import com.gdn.x.productcategorybase.domain.event.model.TerminatedSellerSkuImageCleanupEventModel;
import com.gdn.x.productcategorybase.domain.event.model.VatUpdateDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.VatUpdateHistoryDomainEventModel;
import com.gdn.x.productcategorybase.dto.ProductAndItemLevelUpdatesDTO;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;

public interface DomainEventPublisherService {

  CategoryDomainEventModel publishCategory(Category category, List<CategoryChangeEventType> eventTypes,
      Set<String> categoryChangeEventTypesV2, boolean newCategory) throws Exception;

  /**
   * API for publish event for all categories tree hierarchy from C1 to Cn
   * @param category
   * @return
   * @throws Exception
   */
  CategoryDomainEventModel publishAllCategory(Category category) throws Exception;

  ProductDomainEventModel publishProduct(Product product) throws Exception;


  /**
   * Publish product with sales category changes if present
   *
   * @param product
   * @param productSalesCategoryMapping
   * @return
   * @throws Exception
   */
  ProductDomainEventModel publishProduct(Product product, ProductSalesCategoryMapping productSalesCategoryMapping)
      throws Exception;

  /**
   * @param product
   * @param productSalesCategoryMapping
   * @param isBrandChanged
   * @param scoreUpdate
   * @param solrUpdated
   * @param categoryChange
   * @return
   * @throws Exception
   */
  ProductDomainEventModel publishProductChangeCategory(Product product, ProductSalesCategoryMapping productSalesCategoryMapping,
      boolean isBrandChanged, boolean scoreUpdate, boolean solrUpdated, boolean categoryChange, Set<String> productPublishEventTypes) throws Exception;

  /**
   * product publish event with category change and updating flags based on product or item level info update
   *
   * @param productPublishUpdateDTO
   * @param productSalesCategoryMapping
   * @param isBrandChanged
   * @param scoreUpdated
   * @param solrUpdated
   * @param categoryChange
   * @param productPublishEventTypes
   * @param isItemUpdatePublish
   * @param updateMasterData
   * @param updatedFields
   * @return
   * @throws Exception
   */
  ProductDomainEventModel publishProductChangeCategory(ProductPublishUpdateDTO productPublishUpdateDTO,
      ProductSalesCategoryMapping productSalesCategoryMapping, boolean isBrandChanged, boolean scoreUpdated,
      boolean solrUpdated, boolean categoryChange, boolean ignoreSalesCategoryPublish, Set<String> productPublishEventTypes,
      boolean isItemUpdatePublish, boolean updateMasterData, Set<String> updatedFields) throws Exception;

  /**
   * Publish product event with Brand gets changed
   *
   * @param product
   * @param productSalesCategoryMapping
   * @param isBrandChanged
   * @param scoreUpdated
   * @param solrUpdated
   * @param pristineCategory
   * @param ignoreSalesCategoryPublish
   * @param productAndItemLevelUpdatesDTO
   * @param updatedFields
   * @param deletedItems
   * @return
   * @throws Exception
   */
  ProductDomainEventModel publishProduct(Product product, ProductSalesCategoryMapping productSalesCategoryMapping,
      boolean isBrandChanged, boolean scoreUpdated, boolean solrUpdated, Boolean pristineCategory,
      boolean ignoreSalesCategoryPublish, ProductAndItemLevelUpdatesDTO productAndItemLevelUpdatesDTO,
      Set<String> updatedFields, Set<String> deletedItems) throws Exception;

  /**
   * push product to kafka
   *
   * @param product
   * @param isNewProduct
   * @return
   * @throws Exception
   */
  ProductDomainEventModel publishProduct(Product product, boolean isNewProduct) throws Exception;

  ProductDomainEventModel toProductDomainEventModel(
      Product product, boolean isNewProduct, String migrationType) throws Exception;

  ProductDomainEventModel republishProductToAgp(
      ProductDomainEventModel productDomainEventModel);

  AggregateImageDomainEventModel republishImageToAgp(
      AggregateImageDomainEventModel aggregateImageDomainEventModel);

  AggregateProductItemDomainEventModel republishProductItemToAgp(
      AggregateProductItemDomainEventModel aggregateProductItemDomainEventModel);

  AggregateProductCategoryDomainEventModel republishProductCategoryToAgp(
      AggregateProductCategoryDomainEventModel aggregateProductCategoryDomainEventModel);

  AggregateProductAttributeDomainEventModel republishProductAttributeToAgp(
      AggregateProductAttributeDomainEventModel aggregateProductAttributeDomainEventModel);

  /**
   * push Product events to kafka
   * @param product related product
   * @param event event name
   * @return
   * @throws Exception
   */
  ProductMasterEventModel publishEvent(Product product, ProductMasterEvent event) throws Exception;

  /**
   * push Brand events to kafka
   * @param brand related brand that has been updated
   * @return
   * @throws Exception
   */
  BrandDomainEventModel publishBrandUpdated(Brand brand) throws Exception;

  /**
   * push Brand events to kafka
   * @param brandCode related brand code that has been deleted
   * @return
   * @throws Exception
   */
  BrandDeleteDomainEventModel publishBrandDeleted(String storeId, String brandCode) throws Exception;


  /**
   * Publish kafka events to add brands to brand_collection solr
   *
   * @param solrAddBrandListDomainEventModel
   * @return
   * @throws Exception
   */
  SolrAddBrandListDomainEventModel publishSolrAddBrandEvent(SolrAddBrandListDomainEventModel solrAddBrandListDomainEventModel)
      throws Exception;


  /**
   * Publish kafka events to delete brands from brand_collection solr
   *
   * @param solrDeleteBrandDomainEventModel
   * @return
   * @throws Exception
   */
  SolrDeleteBrandDomainEventModel publishSolrDeleteBrandEvent(SolrDeleteBrandDomainEventModel solrDeleteBrandDomainEventModel)
      throws Exception;

  /**
   * Publish kafka events to update brands in brand_collection solr
   *
   * @param solrUpdateBrandDomainEventModel
   * @return
   * @throws Exception
   */
  SolrUpdateBrandDomainEventModel publishSolrUpdateBrandEvent(SolrUpdateBrandDomainEventModel solrUpdateBrandDomainEventModel)
      throws Exception;


  /**
   * publish kafka event on approval/rejection of brand
   *
   * @param brandApprovedOrRejectedDomainEventModel
   * @return
   */
  BrandApprovedOrRejectedDomainEventModel publishBrandApprovedOrRejectedDomainEventModel(
      BrandApprovedOrRejectedDomainEventModel brandApprovedOrRejectedDomainEventModel);

  /**
   * Publish solr batch add pcb event
   * @param solrAddBatchPcbProductDomainEventModel
   * @return
   */
  SolrAddBatchPcbProductDomainEventModel publishSolrAddBatchPcbProductEvent(
      SolrAddBatchPcbProductDomainEventModel solrAddBatchPcbProductDomainEventModel);

  /**
   * Publish solr batch Delete Pcb Product Event
   * @param solrDeleteBatchPcbProductDomainEventModel
   * @return
   */
  SolrDeleteBatchPcbProductDomainEventModel publishSolrBatchDeletePcbProductEvent (
      SolrDeleteBatchPcbProductDomainEventModel solrDeleteBatchPcbProductDomainEventModel);

  /**
   *
   * @param masterAttributeDomainEventModel
   * @return
   */
  AttributeDomainEventModel publishMasterAttributeInfoEvent(AttributeDomainEventModel masterAttributeDomainEventModel);

  /**
   * push Brand events to kafka
   * @param brand related brand that has been created
   * @return
   * @throws Exception
   */
  BrandDomainEventModel publishBrandCreated(BrandWip brand) throws Exception;

  /**
   * publish the product score update event for products
   * @param productCodes
   * @return
   * @throws Exception
   */
  ProductScoreUpdateDomainEventModel publishProductScoreUpdate(List<String> productCodes) throws Exception;

  /**
   *
   * @param product
   * @param newProduct
   * @return
   * @throws Exception
   */
  ProductDomainEventModel publishProductForMigratedProducts(Product product, boolean newProduct) throws Exception;

  /**
   *
   * @param productCreationFailureDomainEventModel
   * @return
   */
  ProductCreationFailureDomainEventModel publishProductFailure(
      ProductCreationFailureDomainEventModel productCreationFailureDomainEventModel);

  /**
   * @param product
   * @param productSalesCategoryMapping
   * @param isBrandChanged
   * @param scoreUpdate
   * @param solrUpdated
   * @param isItemUpdatePublish
   * @param updateMasterData
   * @return
   * @throws Exception
   */

  ProductDomainEventModel publishProductForEdit(Product product, ProductSalesCategoryMapping productSalesCategoryMapping,
    boolean isBrandChanged, boolean scoreUpdate, boolean solrUpdated, boolean isItemUpdatePublish,
    boolean updateMasterData) throws Exception;

  /**
   *
   * product publish event for edited case and updating flags based on product or item level info update
   *
   * @param productPublishUpdateDTO
   * @param productSalesCategoryMapping
   * @param isBrandChanged
   * @param scoreUpdate
   * @param solrUpdated
   * @param isItemUpdatePublish
   * @param updateMasterData
   * @return
   * @throws Exception
   */

  ProductDomainEventModel publishProductForEdit(ProductPublishUpdateDTO productPublishUpdateDTO,
      ProductSalesCategoryMapping productSalesCategoryMapping, boolean isBrandChanged, boolean scoreUpdate,
      boolean solrUpdated, boolean isItemUpdatePublish, boolean updateMasterData) throws Exception;

  /**
   * publish vat update event
   * @param itemCode
   * @param vatApplicable
   * @return
   */
  VatUpdateDomainEventModel publishVatApplicableUpdateEvent(String itemCode, boolean vatApplicable);

  /**
   * publish event to add external history
   * @param requestId
   * @param storeId
   * @param productItemId
   * @param itemCode
   * @param itemName
   * @param updatedBy
   * @param oldValue
   * @param newValue
   * @return
   */
  VatUpdateHistoryDomainEventModel publishVatApplicableExternalHistoryEvent(String requestId, String storeId, String productItemId,
      String itemCode, String itemName, String updatedBy, String oldValue, String newValue);

  /**
   * publish attribute extraction backfilling event
   * @param storeId
   * @param username
   * @param productCode
   * @param cnCategoryCode
   * @param cnCategoryName
   * @param extractionType
   * @return
   */
  ProductAttributeExtractionModel publishProductAttributeExtractionBackfillingEvent(String storeId, String username,
      String productCode, String cnCategoryCode, String cnCategoryName, String extractionType);

  /**
   * publish product for master data migration
   *
   * @param product
   * @param migrationType
   * @return
   * @throws Exception
   */
  ProductDomainEventModel publishProductForMasterDataMigration(Product product,
    String migrationType) throws Exception;

  /**
   *
   * @param brandCode
   * @param sellerCode
   * @param brandAuthDomainEventModel
   * @return
   */
  BrandAuthDomainEventModel publishBrandAuthHistoryEvent(String brandCode, String sellerCode,
    BrandAuthorisationHistory brandAuthDomainEventModel);

  /**
   * publish product to be deleted
   * @param storeId
   * @param productCodes
   */
  void publishProductToBeDeleted(String storeId, List<String> productCodes);

  /**
   * publish deleted product
   * @param storeId
   * @param productCode
   */
  void publishDeletedProduct(String storeId, String productCode);

  /**
   * @param storeId
   * @param productCode
   * @param imagePaths
   */
  void publishImagePathUpdateEvent(String storeId, String productCode, Set<Pair<String, String>> imagePaths);

  /**
   * publish terminated seller sku cleanup status event
   * @param terminatedSellerSkuCleanupStatusEventModel
   */
  void publishTerminatedSellerSkuCleanupStatusEvent(
      TerminatedSellerSkuCleanupStatusEventModel terminatedSellerSkuCleanupStatusEventModel);

  /**
   * publish terminated seller sku image clean up event
   * @param terminatedSellerSkuImageCleanupEventModel
   */
  void publishTerminatedSellerSkuImageCleanupEvent(
      TerminatedSellerSkuImageCleanupEventModel terminatedSellerSkuImageCleanupEventModel);

  /**
   * Publish restricted keyword history
   *
   * @param restrictedKeywordHistoryEventModelList Keyword History List
   */
  void publishRestrictedKeywordHistory(
    List<RestrictedKeywordHistoryEventModel> restrictedKeywordHistoryEventModelList);

  /**
   * Publish history for category update
   *
   * @param categoryHistoryEventModels category update history event list
   */
  void publishCategoryUpdateHistory(List<CategoryHistoryEventModel> categoryHistoryEventModels);

  /**
   * Publish history event for brand
   * @param brandHistoryEventModel
   */
  void publishBrandHistory(BrandHistoryEventModel brandHistoryEventModel);

  void publishPBPAttributeMigrationEvent(ProductAttributeValue productAttributeValue, String productCode);

  /**
   * Publish vendor event for content refresh
   * @param vendorPublishEventModel vendorPublishEventModel
   *
   */
  void publishVendorEvent(VendorPublishEventModel vendorPublishEventModel);

  /**
   * Publish internal history event
   * @param internalProductHistoryEventModel internalProductHistoryEventModel
   *
   */
  void publishInternalHistoryEvent(InternalProductHistoryEventModel internalProductHistoryEventModel);
}
