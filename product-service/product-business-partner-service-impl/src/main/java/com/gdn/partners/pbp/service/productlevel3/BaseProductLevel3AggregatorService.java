package com.gdn.partners.pbp.service.productlevel3;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.product.entity.ProductLevel3Image;
import com.gdn.mta.product.entity.ProductLevel3Price;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryMinified;
import com.gdn.mta.product.entity.ProductLevel3ViewConfig;
import com.gdn.mta.product.repository.CategoryRepository;
import com.gdn.mta.product.repository.PickupPointRepository;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

public abstract class BaseProductLevel3AggregatorService {

  @Autowired
  protected CategoryRepository categoryRepository;

  @Autowired
  protected PickupPointRepository pickupPointRepository;

  @Autowired
  protected ProductLevel3Converter modelConverter;

  private static final Logger LOGGER = LoggerFactory
      .getLogger(BaseProductLevel3AggregatorService.class);

  protected Map<String, List<CategoryResponse>> generateProductCategoriesData(
      Collection<ItemSummaryResponse> productDatas) throws Exception {
    Map<String, List<CategoryResponse>> categoriesData = new HashMap<>();
    for (ItemSummaryResponse productData : productDatas) {
      String categoryCodeData = "";
      MasterCatalogDTO catalogDTO = productData.getMasterCatalog();
      if (catalogDTO != null && catalogDTO.getCategory() != null) {
        categoryCodeData = catalogDTO.getCategory().getCategoryCode();
      }
      if (categoriesData.get(categoryCodeData) == null) {
        try {
          List<CategoryResponse> categoryData =
              categoryRepository.findHierarchyByCategoryCode(categoryCodeData);
          categoriesData.put(categoryCodeData, categoryData);
        } catch (Exception e) {
          LOGGER.error("Error while retrieving product categories. ItemSku: {}, CategoryCode: {} ",
              productData.getItemSku(), categoryCodeData);
        }
      }
    }
    return categoriesData;
  }

  protected Map<String, PickupPointResponse> generateProductPickupPointsData(
      Collection<ItemSummaryResponse> productDatas) throws Exception {
    Map<String, PickupPointResponse> pickupPointDatas = new HashMap<>();
    for (ItemSummaryResponse productData : productDatas) {
      String pickupPointCodeData = productData.getPickupPointCode();
      if (pickupPointDatas.get(pickupPointCodeData) == null) {
        PickupPointResponse pickupPointData =
            pickupPointRepository.findByPickupPointCode(pickupPointCodeData);
        pickupPointDatas.put(pickupPointCodeData, pickupPointData);
      }
    }
    return pickupPointDatas;
  }

  private String[] generateCategoryNameAndHierarchy(List<CategoryResponse> categories) {
    StringBuilder categoryHierarchy = new StringBuilder();
    String categoryName = org.apache.commons.lang3.StringUtils.EMPTY;
    if (CollectionUtils.isNotEmpty(categories)) {
      // take category name from start index only
      categoryName = categories.get(0).getName();

      // generate hierarchy from last index
      for (int end = categories.size() - 1; end >= 0; end--) {
        categoryHierarchy.append(categories.get(end).getName());
        if (end > 0) {
          categoryHierarchy.append(" > ");
        }
      }
    }
    return new String[] {categoryName, categoryHierarchy.toString()};
  }

  protected boolean skipGenerateProductData(ItemSummaryResponse productData,
      Map<String, ProductLevel3Inventory> inventoryDatas) {
    boolean isSkip;
    MasterCatalogDTO masterCatalog = productData.getMasterCatalog();
    ProductLevel3Inventory inventoryData = inventoryDatas.get(productData.getItemSku());
    isSkip =
        !(masterCatalog != null && inventoryData != null && masterCatalog.getCategory() != null);
    return isSkip;
  }

  protected Page<ProductLevel3SummaryMinified> constructProductLevel3SummariesMinified(
      Page<ItemSummaryResponse> productDatas, Map<String, ProductLevel3Inventory> inventoryDatas,
      Pageable pageable) {
    List<ProductLevel3SummaryMinified> products = new ArrayList<>();
    for (ItemSummaryResponse productData : productDatas) {
      if (skipGenerateProductData(productData, inventoryDatas)) {
        LOGGER.info("Skipping generate incomplete product data: {}", productData.getItemSku());
        continue;
      }

      ProductLevel3SummaryMinified product = new ProductLevel3SummaryMinified();
      product.setItemSku(productData.getItemSku());
      product.setItemName(productData.getGeneratedItemName());
      product.setMerchantSku(productData.getMerchantSku());
      product.setSkuCode(productData.getItemCode());

      List<ProductLevel3Price> productPrices =
          modelConverter.convertItemPricesToProductLevel3Prices(productData.getPrice());
      List<ProductLevel3ViewConfig> productViewConfigs =
          modelConverter.convertItemViewConfigsToProductLevel3ViewConfigs(productData
              .getItemViewConfigs());
      List<ProductLevel3Image> productImages =
          modelConverter.convertMasterDataItemImagesToProductLevel3Images(productData
              .getMasterDataItemImages());

      product.setPrices(productPrices);
      product.setViewConfigs(productViewConfigs);
      product.setImages(productImages);
      product.setPromoBundling(productData.isPromoBundling());
      products.add(product);

    }
    return new PageImpl<>(products, pageable, productDatas.getTotalElements());

  }

  protected Page<ProductLevel3Summary> constructProductLevel3Summaries(
      Page<ItemSummaryResponse> productDatas, Map<String, List<CategoryResponse>> categoriesData,
      Map<String, PickupPointResponse> pickupPointDatas,
      Map<String, ProductLevel3Inventory> inventoryDatas, Pageable pageRequest) {
    List<ProductLevel3Summary> products = new ArrayList<>();
    for (ItemSummaryResponse productData : productDatas) {
      if (skipGenerateProductData(productData, inventoryDatas)) {
        LOGGER.info("Skipping generate incomplete product data: {}", productData);
        continue;
      }
      MasterCatalogDTO masterCatalogDTO = productData.getMasterCatalog();
      ProductLevel3Inventory inventory = inventoryDatas.get(productData.getItemSku());
      String[] categoryNameAndHierarchy =
          generateCategoryNameAndHierarchy(categoriesData.get(masterCatalogDTO.getCategory()
              .getCategoryCode()));
      PickupPointResponse pickupPointData = pickupPointDatas.get(productData.getPickupPointCode());
      ProductLevel3Summary product = new ProductLevel3Summary();
      product.setItemSku(productData.getItemSku());
      product.setSkuCode(productData.getItemCode());
      product.setMerchantSku(productData.getMerchantSku());
      product.setItemName(productData.getGeneratedItemName());
      product.setCreatedDate(productData.getCreatedDate());
      product.setCreatedBy(productData.getCreatedBy());
      product.setUpdatedDate(productData.getUpdatedDate());
      product.setUpdatedBy(productData.getUpdatedBy());
      product.setVersion(productData.getVersion());
      product.setIsArchived(productData.getArchived());
      product.setCategoryCode(masterCatalogDTO.getCategory().getCategoryCode());
      product.setCategoryName(categoryNameAndHierarchy[0]);
      product.setCategoryHierarchy(categoryNameAndHierarchy[1]);
      product.setProductType(productData.getProductType().getCode());
      product.setPickupPointCode(pickupPointData.getCode());
      product.setPickupPointName(pickupPointData.getName());
      product.setLateFulfillment(productData.isLateFulfillment());
      product.setAvailableStockLevel1(inventory.getWarehouseAvailable());
      product.setReservedStockLevel1(inventory.getWarehouseReserved());
      product.setAvailableStockLevel2(inventory.getWebAvailable());
      product.setReservedStockLevel2(inventory.getWebReserved());
      product.setMinimumStockLevel2(inventory.getWebMinAlert());
      product.setSynchronizeStock(inventory.isWebSyncStock());
      product.setOff2OnActiveFlag(productData.isOff2OnChannelActive());
      product.setProductCode(productData.getProductCode());
      product.setProductSku(productData.getProductSku());
      product.setWholesalePriceActivated(productData.getWholesalePriceActivated());
      product.setNonDistributionAvailable(inventory.getNonDistributionAvailable());
      product.setNonDistributionReserved(inventory.getNonDistributionReserved());
      product.setProductScore(productData.getProductScore());
      product.setProductName(productData.getProductName());
      product.setActivePromoBundlings(productData.getActivePromoBundlings());
      product.setPreOrder(productData.isPreOrder());
      product.setOriginalSellingPrice(productData.getOriginalSellingPrice());

      List<ProductLevel3Price> productPrices =
          modelConverter.convertItemPricesToProductLevel3Prices(productData.getPrice());
      List<ProductLevel3ViewConfig> productViewConfigs =
          modelConverter.convertItemViewConfigsToProductLevel3ViewConfigs(productData
              .getItemViewConfigs());
      List<ProductLevel3Image> productImages =
          modelConverter.convertMasterDataItemImagesToProductLevel3Images(productData
              .getMasterDataItemImages());

      product.setPrices(productPrices);
      product.setViewConfigs(productViewConfigs);
      product.setImages(productImages);
      product.setPromoBundling(productData.isPromoBundling());
      product.setMerchantPromoDiscount(productData.isMerchantPromoDiscount());
      product.setMerchantPromoDiscountActivated(productData.isMerchantPromoDiscountActivated());
      product.setPromoTypes(productData.getPromoTypes());
      product.setPriceEditDisabled(productData.isPriceEditDisabled());
      product.setForceReview(productData.isForceReview());
      product.setPreOrderDate(productData.getPreOrderDate());
      products.add(product);
    }
    LOGGER.info("Total products in page : {} are {}", pageRequest, products.size());
    return new PageImpl<>(products, pageRequest, productDatas.getTotalElements());
  }

  protected Page<ProductLevel3Summary> constructProductLevel3Summaries(Page<ItemSummaryResponse> itemSummaryResponses,
    Map<String, List<CategoryResponse>> categories, Pageable pageRequest) {
    List<ProductLevel3Summary> products = new ArrayList<>();
    long actualTotalRecords = itemSummaryResponses.getTotalElements();

    for (ItemSummaryResponse response : itemSummaryResponses) {
      if (Objects.isNull(response.getMasterCatalog()) || Objects.isNull((response.getMasterCatalog().getCategory()))) {
        LOGGER.info("Skipping generate incomplete product data: {}", response);
        actualTotalRecords--;
        continue;
      }

      MasterCatalogDTO masterCatalogDTO = response.getMasterCatalog();
      String[] categoryNameAndHierarchy = generateCategoryNameAndHierarchy(
        categories.get(masterCatalogDTO.getCategory().getCategoryCode())
      );

      ProductLevel3Summary product = new ProductLevel3Summary();
      product.setItemSku(response.getItemSku());
      product.setSkuCode(response.getItemCode());
      product.setMerchantSku(response.getMerchantSku());
      product.setItemName(response.getGeneratedItemName());
      product.setCreatedDate(response.getCreatedDate());
      product.setCreatedBy(response.getCreatedBy());
      product.setUpdatedDate(response.getUpdatedDate());
      product.setUpdatedBy(response.getUpdatedBy());
      product.setIsArchived(response.getArchived());
      product.setCategoryCode(masterCatalogDTO.getCategory().getCategoryCode());
      product.setCategoryName(categoryNameAndHierarchy[0]);
      product.setCategoryHierarchy(categoryNameAndHierarchy[1]);

      product.setProductCode(response.getProductCode());
      product.setProductSku(response.getProductSku());

      List<ProductLevel3Price> productPrices = modelConverter
        .convertItemPricesToProductLevel3Prices(response.getPrice());

      product.setPrices(productPrices);
      products.add(product);
    }
    return new PageImpl<>(products, pageRequest, actualTotalRecords);
  }

}
