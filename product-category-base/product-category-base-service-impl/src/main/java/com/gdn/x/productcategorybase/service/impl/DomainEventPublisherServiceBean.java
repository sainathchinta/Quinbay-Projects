package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.domain.event.model.BrandHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.DistributionInfoEventModel;
import com.gdn.x.productcategorybase.domain.event.model.InternalProductHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.PBPProductAttributeBackFillingEventModel;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.gdn.x.productcategorybase.CategoryChangeEventType;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ProductChangeType;
import com.gdn.x.productcategorybase.ProductMasterEvent;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.AggregateImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductItemDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.BrandApprovedOrRejectedDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.BrandAuthDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.BrandDeleteDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.BrandDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CatalogDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImagePathUpdateDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.OSCDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.PredefinedAllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeExtractionModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCodeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCreationFailureDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductMasterEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.gdn.x.productcategorybase.domain.event.model.ProductScoreUpdateDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBatchPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBrandListDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBatchPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBrandDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrUpdateBrandDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.TerminatedSellerSkuCleanupStatusEventModel;
import com.gdn.x.productcategorybase.domain.event.model.TerminatedSellerSkuImageCleanupEventModel;
import com.gdn.x.productcategorybase.domain.event.model.VatUpdateDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.VatUpdateHistoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.VendorPublishEventModel;
import com.gdn.x.productcategorybase.dto.ProductAndItemLevelUpdatesDTO;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.response.DimensionsAndUomResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionItemInfoResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.entity.ProductItemUomInfo;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.enums.UpdatedFields;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.brand.BrandService;
import com.gdn.x.productcategorybase.service.brand.BrandWipService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import com.gdn.x.productcategorybase.util.CommonUtil;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class DomainEventPublisherServiceBean implements DomainEventPublisherService {

  public static final String STORE_ID = "10001";

  public static final String BRAND = "Brand";
  public static final String DASH = "-";
  private static final String EVENT_PUBLISH_LOG = "Publishing event {}, payload : {} ";

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Autowired
  @Lazy
  private BrandService brandServiceBean;

  @Autowired
  @Lazy
  private BrandWipService brandWipService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${product.attribute.migration.predefined.attribute.value.filtering.enabled}")
  private boolean productAttributeMigrationPredefinedAttributeValueFilteringEnabled;

  private String getBrandCode(ProductAttributeValue productAttributeValue) {
    return Optional.ofNullable(productAttributeValue)
        .map(ProductAttributeValue::getPredefinedAllowedAttributeValue)
        .map(PredefinedAllowedAttributeValue::getPredefinedAllowedAttributeCode)
        .orElse(null);
  }

  private String getBrandLogoPath(String brandCode) {
    try {
      Brand brand = brandServiceBean.findByBrandCode(brandCode);
      if (Objects.isNull(brand)) {
        return getBrandLogoPathFromBrandWip(brandCode);
      }
      return brand.getBrandLogoPath();
    } catch (Exception e) {
      log.error("Error occurred when findByBrandCode({}) in function getBrandLogoUrl with error {}", brandCode, e);
      return null;
    }
  }

  private String getBrandLogoPathFromBrandWip(String brandCode) {
    return Optional.ofNullable(brandCode).map(code -> brandWipService.getBrandWipDetail(STORE_ID, code))
        .map(BrandWipResponse::getBrandLogoPath)
        .orElse(null);
  }

  public String getBrandLogoUrl(ProductAttributeValue productAttributeValue) {
    String brandCode = getBrandCode(productAttributeValue);
    String brandLogoPath = getBrandLogoPath(brandCode);
    if(!StringUtils.isEmpty(brandLogoPath)) {
      return String.format("/%s/%s",brandCode,brandLogoPath);
    } else {
      return null;
    }
  }

  private void convertProductToProductDomainEventModel(Product product,
      ProductDomainEventModel productDomainEventModel, boolean isNewProduct) {
    BeanUtils.copyProperties(product, productDomainEventModel, new String[] {"createdMerchant","productCategories", "productAttributes", "productImages", "productItems"});
    productDomainEventModel.setNewProduct(isNewProduct);
    productDomainEventModel.setSellerCode(product.getCreatedMerchant());
    productDomainEventModel.setProductItems(new ArrayList<ProductItemDomainEventModel>());
    productDomainEventModel.setProductCategories(new ArrayList<ProductCategoryDomainEventModel>());
    productDomainEventModel.setProductAttributes(new ArrayList<ProductAttributeDomainEventModel>());
    productDomainEventModel.setImages(new ArrayList<ImageDomainEventModel>());
    productDomainEventModel.setproductMarkForDelete(product.isMarkForDelete());
    productDomainEventModel.setId(product.getId());
    productDomainEventModel.setStoreId(product.getStoreId());
    if (ranchIntegrationEnabled) {
      DistributionInfoResponse distributionInfoResponse = CommonUtil.getDistributionInfoResponse(product);
      DistributionInfoEventModel distributionInfoEventModel = new DistributionInfoEventModel();
      if (Objects.nonNull(distributionInfoResponse)) {
        BeanUtils.copyProperties(distributionInfoResponse, distributionInfoEventModel);
        productDomainEventModel.setDistributionInfo(distributionInfoEventModel);
      }
    }
    productDomainEventModel.setSellerCode(product.getCreatedMerchant());
    if (Objects.nonNull(product.getCreatedMerchant()) && product.getCreatedMerchant().equalsIgnoreCase("internal")) {
      productDomainEventModel.setFlowType(Constants.FLOW_3);
    } else {
      productDomainEventModel.setFlowType(Constants.FLOW_1);
    }
    for (ProductItem productItem : product.getProductItems()) {
      if (!productItem.isMarkForDelete()) {
        ProductItemDomainEventModel productItemDomainEventModel = new ProductItemDomainEventModel();
        BeanUtils.copyProperties(productItem, productItemDomainEventModel);
        productItemDomainEventModel.setImages(new ArrayList<ImageDomainEventModel>());
        productItem.setProductItemImages(
            productItem.getProductItemImages().stream().filter(productItemImage -> !productItemImage.isMarkForDelete())
                .filter(productItemImage -> ConverterUtil.filterProcessedItemImages(productItemImage))
                .collect(Collectors.toList()));
        for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
          if (!productItemImage.isMarkForDelete()) {
            ImageDomainEventModel imageDomainEventModel = new ImageDomainEventModel();
            BeanUtils.copyProperties(productItemImage, imageDomainEventModel);
            imageDomainEventModel.setMainImage(productItemImage.isMainImages());
            productItemDomainEventModel.getImages().add(imageDomainEventModel);
          }
        }
        if (ranchIntegrationEnabled) {
          CommonUtil.setProductItemUomInfoEventModel(productItem, productItemDomainEventModel);
        }
        productDomainEventModel.getProductItems().add(productItemDomainEventModel);
      }
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      if (!productCategory.isMarkForDelete()) {
        Catalog catalog = productCategory.getCategory().getCatalog();
        Category category = productCategory.getCategory();
        CatalogDomainEventModel catalogDomainEventModel =
            new CatalogDomainEventModel(catalog.getName(), catalog.getCatalogCode(), catalog.getCatalogType().name());
        CategoryDomainEventModel categoryDomainEventModel = new CategoryDomainEventModel();
        BeanUtils.copyProperties(category, categoryDomainEventModel, new String[] {"catalog"});
        if (category.getParentCategory() != null) {
          categoryDomainEventModel.setParentCategoryId(category.getParentCategory().getId());
        }
        categoryDomainEventModel.setCatalog(catalogDomainEventModel);
        ProductCategoryDomainEventModel productCategoryDomainEventModel =
            new ProductCategoryDomainEventModel(categoryDomainEventModel);
        productDomainEventModel.getProductCategories().add(productCategoryDomainEventModel);
      }
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      if (!productAttribute.isMarkForDelete()) {
        Attribute attribute = productAttribute.getAttribute();
        AttributeDomainEventModel attributeDomainEventModel = new AttributeDomainEventModel();
        BeanUtils.copyProperties(attribute, attributeDomainEventModel, new String[] {"attributeType"});
        attributeDomainEventModel.setAttributeType(attribute.getAttributeType().name());
        ProductAttributeDomainEventModel productAttributeDomainEventModel = new ProductAttributeDomainEventModel();
        BeanUtils.copyProperties(productAttribute, productAttributeDomainEventModel, new String[] {"attribute",
            "productAttributeValues"});
        productAttributeDomainEventModel.setAttribute(attributeDomainEventModel);
        productAttributeDomainEventModel.setOwnByProductItem(productAttribute.isOwnByProductItem());
        productAttributeDomainEventModel
            .setProductAttributeValues(new ArrayList<ProductAttributeValueDomainEventModel>());
        for (ProductAttributeValue productAttributeValue : productAttribute.getProductAttributeValues()) {
          if (!productAttributeValue.isMarkForDelete()) {
            ProductAttributeValueDomainEventModel productAttributeValueDomainEventModel =
                new ProductAttributeValueDomainEventModel();
            if (productAttributeValue.getAllowedAttributeValue() != null) {
              AllowedAttributeValue allowedAttributeValue = productAttributeValue.getAllowedAttributeValue();
              AllowedAttributeValueDomainEventModel allowedAttributeValueDomainEventModel =
                  new AllowedAttributeValueDomainEventModel();
              BeanUtils.copyProperties(allowedAttributeValue, allowedAttributeValueDomainEventModel);
              productAttributeValueDomainEventModel.setAllowedAttributeValue(allowedAttributeValueDomainEventModel);
            }
            if (productAttributeValue.getPredefinedAllowedAttributeValue() != null) {
              PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
                  productAttributeValue.getPredefinedAllowedAttributeValue();
              PredefinedAllowedAttributeValueDomainEventModel predefinedAllowedAttributeValueDomainEventModel =
                  new PredefinedAllowedAttributeValueDomainEventModel();
              BeanUtils
                  .copyProperties(predefinedAllowedAttributeValue, predefinedAllowedAttributeValueDomainEventModel);
              productAttributeValueDomainEventModel
                  .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDomainEventModel);

              // Add brand logo
              if (BRAND.equalsIgnoreCase(productAttribute.getProductAttributeName())) {
                productDomainEventModel.setBrandLogoUrl(getBrandLogoUrl(productAttributeValue));
              }
            }
            productAttributeValueDomainEventModel.setDescriptiveAttributeValueType(productAttributeValue
                .getDescriptiveAttributeValueType().name());
            productAttributeValueDomainEventModel.setDescriptiveAttributeValue(productAttributeValue
                .getDescriptiveAttributeValue());
            productAttributeDomainEventModel.getProductAttributeValues().add(productAttributeValueDomainEventModel);
          }
        }
        productDomainEventModel.getProductAttributes().add(productAttributeDomainEventModel);
      }
    }
    product.setProductImages(product.getProductImages().stream().filter(productImage -> !productImage.isMarkForDelete())
        .filter(productImage -> ConverterUtil.filterProcessedProductImages(productImage)).collect(Collectors.toList()));

    for (ProductImage productImage : product.getProductImages()) {
      if (!productImage.isMarkForDelete()) {
        ImageDomainEventModel imageDomainEventModel = new ImageDomainEventModel();
        BeanUtils.copyProperties(productImage, imageDomainEventModel);
        imageDomainEventModel.setMainImage(productImage.isMainImages());
        productDomainEventModel.getImages().add(imageDomainEventModel);
      }
    }
  }

  private void convertCategoryToCategoryDomainEventModel(Category category,
      CategoryDomainEventModel categoryDomainEventModel, List<CategoryChangeEventType> eventTypes) throws Exception {
    Catalog catalog = category.getCatalog();
    CatalogDomainEventModel catalogDomainEventModel =
        new CatalogDomainEventModel(catalog.getName(), catalog.getCatalogCode(), catalog.getCatalogType().name());
    BeanUtils.copyProperties(category, categoryDomainEventModel, new String[] {"catalog", "categoryChangeTypes", "originalSalesCategory"});
    categoryDomainEventModel.setParentCategoryId(category.getParentCategory() == null ? null : category
        .getParentCategory().getId());
    categoryDomainEventModel.setCategoryChangeTypes(eventTypes);
    if (Objects.nonNull(eventTypes)) {
      categoryDomainEventModel
          .setCategoryChangeTypesV2(eventTypes.stream().map(CategoryChangeEventType::name).collect(Collectors.toSet()));
    }
    categoryDomainEventModel.setCatalog(catalogDomainEventModel);
    if (Objects.nonNull(category.getOriginalSalesCategory())) {
      OSCDomainEventModel oscDomainEventModel = new OSCDomainEventModel();
      BeanUtils.copyProperties(category.getOriginalSalesCategory(), oscDomainEventModel, "masterCategories");
      categoryDomainEventModel.setOriginalSalesCategory(oscDomainEventModel);
    }
    List<String> attributeCodes =
        category.getCategoryAttributes().stream().map(CategoryAttribute::getAttribute).filter(Objects::nonNull)
            .map(Attribute::getAttributeCode).toList();
    categoryDomainEventModel.setAttributeCodes(attributeCodes);
  }

  private BrandDomainEventModel convertBrandToBrandDomainEventModel(Brand brand) {
    BrandDomainEventModel brandDomainEventModel = new BrandDomainEventModel();
    BeanUtils.copyProperties(brand, brandDomainEventModel);
    return brandDomainEventModel;
  }

  @Override
  public ProductDomainEventModel toProductDomainEventModel(Product product, boolean isNewProduct,
    String migrationType) throws Exception {
    ProductDomainEventModel productDomainEventModel = new ProductDomainEventModel();
    convertProductToProductDomainEventModel(product, productDomainEventModel, isNewProduct);
    productDomainEventModel.setScoreUpdated(false);
    productDomainEventModel.setSolrUpdateRequired(false);
    productDomainEventModel.setMigrationType(migrationType);
    log.debug("push new product to kafka: productId= {}, productCode= {}, markForDelete= {}",
        productDomainEventModel.getId(), productDomainEventModel.getProductCode(), productDomainEventModel.isproductMarkForDelete());
    return productDomainEventModel;
  }

  private ProductDomainEventModel toProductDomainEventModel(Product product,
      ProductSalesCategoryMapping productSalesCategoryMapping) throws Exception {
    ProductDomainEventModel productDomainEventModel = new ProductDomainEventModel();
    convertProductToProductDomainEventModel(product, productDomainEventModel, false);
    log.info("push new product to kafka: productId= {}, productCode= {}, markForDelete= {}",
        productDomainEventModel.getId(), productDomainEventModel.getProductCode(), productDomainEventModel.isproductMarkForDelete());
    if(Objects.nonNull(productSalesCategoryMapping)) {
      productDomainEventModel.setProductSalesCategoryMapping(productSalesCategoryMapping);
      Set<ProductChangeType> productChangeTypes = productDomainEventModel.getProductChangeTypes();
      productChangeTypes.add(ProductChangeType.CATEGORY_CHANGE);
      productDomainEventModel.setProductChangeTypes(productChangeTypes);
    }
    return productDomainEventModel;
  }

  private ProductDomainEventModel toProductDomainEventModelCategoryChange(Product product,
      ProductSalesCategoryMapping productSalesCategoryMapping, boolean isBrandChanged, boolean scoreUpdated, boolean solrUpdated,
      boolean categoryChange) throws Exception {
    ProductDomainEventModel productDomainEventModel = new ProductDomainEventModel();
    convertProductToProductDomainEventModel(product, productDomainEventModel, false);
    log.info("push product to kafka: productId= {}, productCode= {}, markForDelete= {}, brandChanged= {}",
        productDomainEventModel.getId(), productDomainEventModel.getProductCode(),
        productDomainEventModel.isproductMarkForDelete(), isBrandChanged);
    if (Objects.nonNull(productSalesCategoryMapping) || categoryChange) {
      productDomainEventModel.setProductSalesCategoryMapping(productSalesCategoryMapping);
      Set<ProductChangeType> productChangeTypes = productDomainEventModel.getProductChangeTypes();
      productChangeTypes.add(ProductChangeType.CATEGORY_CHANGE);
      productDomainEventModel.setProductChangeTypes(productChangeTypes);
      productDomainEventModel.getUpdatedFields().add(UpdatedFields.CATEGORY_UPDATE.name());
    }
    productDomainEventModel.setBrandChanged(isBrandChanged);
    productDomainEventModel.setScoreUpdated(scoreUpdated);
    productDomainEventModel.setSolrUpdateRequired(solrUpdated);
    if (isBrandChanged) {
      productDomainEventModel.getUpdatedFields().add(UpdatedFields.BRAND_UPDATE.name());
    }
    return productDomainEventModel;
  }

  private ProductDomainEventModel toProductDomainEventModel(Product product,
      ProductSalesCategoryMapping productSalesCategoryMapping, boolean isBrandChanged, boolean scoreUpdated,
      boolean solrUpdated, Boolean pristineCategory) throws Exception {
    ProductDomainEventModel productDomainEventModel = new ProductDomainEventModel();
    convertProductToProductDomainEventModel(product, productDomainEventModel, false);
    log.info("push product to kafka: productId= {}, productCode= {}, markForDelete= {}, brandChanged= {}",
        productDomainEventModel.getId(), productDomainEventModel.getProductCode(),
        productDomainEventModel.isproductMarkForDelete(), isBrandChanged);
    if (Objects.nonNull(productSalesCategoryMapping)) {
      productDomainEventModel.setProductSalesCategoryMapping(productSalesCategoryMapping);
      Set<ProductChangeType> productChangeTypes = productDomainEventModel.getProductChangeTypes();
      productChangeTypes.add(ProductChangeType.CATEGORY_CHANGE);
      productDomainEventModel.setProductChangeTypes(productChangeTypes);
      productDomainEventModel.getUpdatedFields().add(UpdatedFields.CATEGORY_UPDATE.name());
    }
    productDomainEventModel.setBrandChanged(isBrandChanged);
    productDomainEventModel.setScoreUpdated(scoreUpdated);
    productDomainEventModel.setPristineCategory(pristineCategory);
    productDomainEventModel.setSolrUpdateRequired(solrUpdated);
    if (isBrandChanged) {
      productDomainEventModel.getUpdatedFields().add(UpdatedFields.BRAND_UPDATE.name());
    }
    return productDomainEventModel;
  }

  @Override
  public ProductDomainEventModel publishProduct(Product product) throws Exception {
    return publishProduct(product, false);
  }

  @Override
  public ProductDomainEventModel publishProduct(Product product, boolean isNewProduct) throws Exception {
    ProductDomainEventModel productDomainEventModel = toProductDomainEventModel(product, isNewProduct,
      StringUtils.EMPTY);
    kafkaPublisher.send(DomainEventName.PRODUCT_PUBLISH, product.getProductCode(), productDomainEventModel);
    return productDomainEventModel;
  }

  @Override
  public ProductDomainEventModel publishProduct(Product product,
      ProductSalesCategoryMapping productSalesCategoryMapping) throws Exception {
    ProductDomainEventModel productDomainEventModel =  toProductDomainEventModel(product, productSalesCategoryMapping);
    kafkaPublisher.send(DomainEventName.PRODUCT_PUBLISH, product.getProductCode(), productDomainEventModel);
    return productDomainEventModel;
  }

  @Override
  public ProductDomainEventModel publishProductChangeCategory(Product product,
      ProductSalesCategoryMapping productSalesCategoryMapping, boolean isBrandChanged, boolean scoreUpdated,
      boolean solrUpdated, boolean categoryChange, Set<String> productPublishEventTypes) throws Exception {
    ProductDomainEventModel productDomainEventModel =
        toProductDomainEventModelCategoryChange(product, productSalesCategoryMapping, isBrandChanged, scoreUpdated,
            solrUpdated, categoryChange);
    productDomainEventModel.setEventTypes(productPublishEventTypes);
    kafkaPublisher.send(DomainEventName.PRODUCT_PUBLISH, product.getProductCode(), productDomainEventModel);
    return productDomainEventModel;
  }

  @Override
  public ProductDomainEventModel publishProductChangeCategory(ProductPublishUpdateDTO productPublishUpdateDTO,
      ProductSalesCategoryMapping productSalesCategoryMapping, boolean isBrandChanged, boolean scoreUpdated,
      boolean solrUpdated, boolean categoryChange, boolean ignoreSalesCategoryPublish, Set<String> productPublishEventTypes,
      boolean isItemUpdatePublish, boolean updateMasterData, Set<String> updatedFields) throws Exception {
    ProductDomainEventModel productDomainEventModel =
        toProductDomainEventModelCategoryChange(productPublishUpdateDTO.getProduct(), productSalesCategoryMapping,
            isBrandChanged, scoreUpdated, solrUpdated, categoryChange);
    productDomainEventModel.setItemUpdatePublish(isItemUpdatePublish);
    productDomainEventModel.setUpdateMasterData(updateMasterData);
    productDomainEventModel.setIgnoreSalesCategoryPublish(ignoreSalesCategoryPublish);
    productDomainEventModel.setEventTypes(productPublishEventTypes);
    CommonUtil.productDomainEventModelFlagUpdateBasedOnProductOrItemLevelChange(productDomainEventModel,
        productPublishUpdateDTO.isProductLevelDataUpdated(), productPublishUpdateDTO.getUpdatedItemSkuCodes());
    productDomainEventModel.getUpdatedFields().addAll(updatedFields);
    kafkaPublisher.send(DomainEventName.PRODUCT_PUBLISH, productPublishUpdateDTO.getProduct().getProductCode(),
        productDomainEventModel);
    log.info("Published {} event for productCode : {} and with payload : {}", DomainEventName.PRODUCT_PUBLISH,
        productDomainEventModel.getProductCode(), productDomainEventModel);
    return productDomainEventModel;
  }

  @Override
  public ProductDomainEventModel publishProduct(Product product,
      ProductSalesCategoryMapping productSalesCategoryMapping, boolean isBrandChanged, boolean scoreUpdated,
      boolean solrUpdated, Boolean pristineCategory, boolean ignoreSalesCategoryPublish,
      ProductAndItemLevelUpdatesDTO productAndItemLevelUpdatesDTO, Set<String> updatedFields, Set<String> deletedItems) throws Exception {
    ProductDomainEventModel productDomainEventModel =
        toProductDomainEventModel(product, productSalesCategoryMapping, isBrandChanged, scoreUpdated, solrUpdated,
            pristineCategory);
    productDomainEventModel.setIgnoreSalesCategoryPublish(ignoreSalesCategoryPublish);
    productDomainEventModel.setUpdatedFields(updatedFields);
    productDomainEventModel.setDeletedItems(deletedItems);
    CommonUtil.productDomainEventModelFlagUpdateBasedOnProductOrItemLevelChange(productDomainEventModel,
        productAndItemLevelUpdatesDTO.isProductLevelDataUpdated(), productAndItemLevelUpdatesDTO.getUpdatedItemSkuCodes());
    kafkaPublisher.send(DomainEventName.PRODUCT_PUBLISH, product.getProductCode(), productDomainEventModel);
    log.info("Published {} event for productCode : {} and with payload : {}", DomainEventName.PRODUCT_PUBLISH,
        productDomainEventModel.getProductCode(), productDomainEventModel);
    return productDomainEventModel;
  }

  @Override
  public ProductDomainEventModel republishProductToAgp(ProductDomainEventModel productDomainEventModel) {
    kafkaPublisher
        .send(DomainEventName.PRODUCT_PUBLISH_ALL, productDomainEventModel.getProductCode(), productDomainEventModel);
    return productDomainEventModel;
  }

  @Override
  public AggregateImageDomainEventModel republishImageToAgp(
      AggregateImageDomainEventModel aggregateImageDomainEventModel) {
    kafkaPublisher.send(DomainEventName.IMAGE_PUBLISH_ALL, aggregateImageDomainEventModel.getProductCode(),
        aggregateImageDomainEventModel);
    return aggregateImageDomainEventModel;
  }

  @Override
  public AggregateProductItemDomainEventModel republishProductItemToAgp(
      AggregateProductItemDomainEventModel aggregateProductItemDomainEventModel) {
    kafkaPublisher.send(DomainEventName.PRODUCT_ITEM_PUBLISH_ALL, aggregateProductItemDomainEventModel.getProductCode(),
        aggregateProductItemDomainEventModel);
    return aggregateProductItemDomainEventModel;
  }

  @Override
  public AggregateProductCategoryDomainEventModel republishProductCategoryToAgp(
      AggregateProductCategoryDomainEventModel aggregateProductCategoryDomainEventModel) {
    kafkaPublisher
        .send(DomainEventName.PRODUCT_CATEGORY_PUBLISH_ALL, aggregateProductCategoryDomainEventModel.getProductCode(),
            aggregateProductCategoryDomainEventModel);
    return aggregateProductCategoryDomainEventModel;
  }

  @Override
  public AggregateProductAttributeDomainEventModel republishProductAttributeToAgp(
      AggregateProductAttributeDomainEventModel aggregateProductAttributeDomainEventModel) {
    kafkaPublisher.send(DomainEventName.PRODUCT_ATTRIBUTE_PUBLISH_ALL, aggregateProductAttributeDomainEventModel.getProductCode(),
        aggregateProductAttributeDomainEventModel);
    return aggregateProductAttributeDomainEventModel;
  }

  @Override
  public ProductMasterEventModel publishEvent(Product product, ProductMasterEvent event) {
    ProductMasterEventModel productMasterEvent =
        new ProductMasterEventModel(product.getProductCode(), event.toString().toLowerCase());
    kafkaPublisher.send(DomainEventName.PRODUCT_EVENT, productMasterEvent.getProductMasterCode(), productMasterEvent);
    return productMasterEvent;
  }

  @Override
  public CategoryDomainEventModel publishCategory(Category category, List<CategoryChangeEventType> eventTypes,
      Set<String> categoryChangeEventTypesV2, boolean newCategory)
      throws Exception {
    CategoryDomainEventModel categoryDomainEventModel = new CategoryDomainEventModel();
    convertCategoryToCategoryDomainEventModel(category, categoryDomainEventModel, eventTypes);
    categoryDomainEventModel.setCategoryChangeTypesV2(categoryChangeEventTypesV2);
    if(Objects.nonNull(eventTypes)) {
      categoryDomainEventModel.getCategoryChangeTypesV2().addAll(eventTypes.stream().map(CategoryChangeEventType::name).collect(Collectors.toList()));
    }
    categoryDomainEventModel.setNewData(newCategory);
    kafkaPublisher.send(DomainEventName.CATEGORY_PUBLISH, category.getCategoryCode(), categoryDomainEventModel);
    log.info("Publishing event {} with payload : {}", DomainEventName.CATEGORY_PUBLISH, categoryDomainEventModel);
    return categoryDomainEventModel;
  }

  @Override
  public CategoryDomainEventModel publishAllCategory(Category category) throws Exception {
    CategoryDomainEventModel categoryDomainEventModel = new CategoryDomainEventModel();
    convertCategoryToCategoryDomainEventModel(category, categoryDomainEventModel, null);
    kafkaPublisher
        .send(DomainEventName.ALL_CATEGORY_PUBLISH,  category.getCategoryCode(), categoryDomainEventModel);
    return categoryDomainEventModel;
  }

  @Override
  public BrandDeleteDomainEventModel publishBrandDeleted(String storeId, String brandCode) {
    BrandDeleteDomainEventModel brandDeleteDomainEventModel = new BrandDeleteDomainEventModel();
    brandDeleteDomainEventModel.setBrandCode(brandCode);
    brandDeleteDomainEventModel.setStoreId(storeId);
    kafkaPublisher.send(DomainEventName.BRAND_DELETED, brandCode, brandDeleteDomainEventModel);
    return brandDeleteDomainEventModel;
  }

  @Override
  public BrandDomainEventModel publishBrandUpdated(Brand brand) {
    BrandDomainEventModel brandDomainEventModel = convertBrandToBrandDomainEventModel(brand);
    kafkaPublisher.send(DomainEventName.BRAND_UPDATED, brand.getBrandCode(), brandDomainEventModel);
    return brandDomainEventModel;
  }

  @Override
  public SolrAddBrandListDomainEventModel publishSolrAddBrandEvent(
      SolrAddBrandListDomainEventModel solrAddBrandListDomainEventModel) throws Exception {
    kafkaPublisher.send(DomainEventName.SOLR_ADD_BRAND_EVENT, solrAddBrandListDomainEventModel);
    return solrAddBrandListDomainEventModel;
  }

  @Override
  public SolrDeleteBrandDomainEventModel publishSolrDeleteBrandEvent(
      SolrDeleteBrandDomainEventModel solrAddBrandDomainEventModel) throws Exception {
    kafkaPublisher.send(DomainEventName.SOLR_DELETE_BRAND_EVENT, solrAddBrandDomainEventModel);
    return solrAddBrandDomainEventModel;
  }

  @Override
  public SolrUpdateBrandDomainEventModel publishSolrUpdateBrandEvent(
      SolrUpdateBrandDomainEventModel solrUpdateBrandDomainEventModel) throws Exception {
    kafkaPublisher.send(DomainEventName.SOLR_UPDATE_BRAND_EVENT, solrUpdateBrandDomainEventModel);
    return solrUpdateBrandDomainEventModel;
  }

  @Override
  public SolrAddBatchPcbProductDomainEventModel publishSolrAddBatchPcbProductEvent(
      SolrAddBatchPcbProductDomainEventModel eventModel) {
    log.warn("PCB solr collection re-index batch size published {}",
        eventModel.getProductDomainEventModelList().size());
    kafkaPublisher.send(DomainEventName.SOLR_ADD_BATCH_PCB_PRODUCT_EVENT, eventModel);
    return eventModel;
  }

  @Override
  public SolrDeleteBatchPcbProductDomainEventModel publishSolrBatchDeletePcbProductEvent(
      SolrDeleteBatchPcbProductDomainEventModel solrDeleteBatchPcbProductDomainEventModel) {
    kafkaPublisher.send(DomainEventName.SOLR_DELETE_BATCH_PCB_PRODUCT_EVENT, solrDeleteBatchPcbProductDomainEventModel);
    return solrDeleteBatchPcbProductDomainEventModel;
  }

  @Override
  public BrandApprovedOrRejectedDomainEventModel publishBrandApprovedOrRejectedDomainEventModel(
      BrandApprovedOrRejectedDomainEventModel brandApprovedOrRejectedDomainEventModel) {
    kafkaPublisher
        .send(DomainEventName.BRAND_APPROVED_OR_REJECTED_EVENT, brandApprovedOrRejectedDomainEventModel.getBrandCode(),
            brandApprovedOrRejectedDomainEventModel);
    return brandApprovedOrRejectedDomainEventModel;
  }

  @Override
  public AttributeDomainEventModel publishMasterAttributeInfoEvent(
      AttributeDomainEventModel masterAttributeDomainEventModel) {
    kafkaPublisher.send(DomainEventName.MASTER_ATTRIBUTE_INFO_EVENT, masterAttributeDomainEventModel);
    return masterAttributeDomainEventModel;
  }

  @Override
  public BrandDomainEventModel publishBrandCreated(BrandWip brand) {
    BrandDomainEventModel brandDomainEventModel = convertBrandWipToBrandDomainEventModel(brand);
    kafkaPublisher.send(DomainEventName.BRAND_CREATED, brand.getBrandCode(), brandDomainEventModel);
    return brandDomainEventModel;
  }

  private BrandDomainEventModel convertBrandWipToBrandDomainEventModel(BrandWip brand) {
    BrandDomainEventModel brandDomainEventModel = new BrandDomainEventModel();
    BeanUtils.copyProperties(brand, brandDomainEventModel);
    return brandDomainEventModel;
  }

  @Override
  public ProductScoreUpdateDomainEventModel publishProductScoreUpdate(List<String> productCodes) {
    ProductScoreUpdateDomainEventModel productScoreUpdateDomainEventModel = new ProductScoreUpdateDomainEventModel();
    productScoreUpdateDomainEventModel.setProductCodes(productCodes);
    log.info("Publishing event {} with payload : {}", DomainEventName.PRODUCT_SCORE_UPDATE_EVENT_NAME,
        productScoreUpdateDomainEventModel);
    kafkaPublisher.send(DomainEventName.PRODUCT_SCORE_UPDATE_EVENT_NAME, productScoreUpdateDomainEventModel);
    return productScoreUpdateDomainEventModel;
  }

  @Override
  public ProductDomainEventModel publishProductForMigratedProducts(Product product, boolean newProduct)
      throws Exception {
    ProductDomainEventModel productDomainEventModel = toProductDomainEventModel(product);
    productDomainEventModel.setNewProduct(newProduct);
    kafkaPublisher.send(DomainEventName.PRODUCT_PUBLISH, product.getProductCode(), productDomainEventModel);
    return productDomainEventModel;
  }

  private ProductDomainEventModel toProductDomainEventModel(Product product) throws Exception {
    ProductDomainEventModel productDomainEventModel = new ProductDomainEventModel();
    convertProductToProductDomainEventModel(product, productDomainEventModel, true);
    log.info("push product to kafka for migration : productId= {}, productCode= {}, markForDelete= {}",
        productDomainEventModel.getId(), productDomainEventModel.getProductCode(),
        productDomainEventModel.isproductMarkForDelete());
    productDomainEventModel.setSolrUpdateRequired(true);
    productDomainEventModel.setMigratedProduct(true);
    return productDomainEventModel;
  }

  @Override
  public ProductCreationFailureDomainEventModel publishProductFailure(
      ProductCreationFailureDomainEventModel productCreationFailureDomainEventModel) {
    productCreationFailureDomainEventModel.setSellerCode(productCreationFailureDomainEventModel.getCreatedMerchant());
    kafkaPublisher.send(DomainEventName.PRODUCT_PUBLISH, productCreationFailureDomainEventModel.getProductCode(),
        productCreationFailureDomainEventModel);
    return productCreationFailureDomainEventModel;
  }

  @Override
  public ProductDomainEventModel publishProductForEdit(Product product,
    ProductSalesCategoryMapping productSalesCategoryMapping, boolean isBrandChanged, boolean scoreUpdated,
    boolean solrUpdated, boolean isItemUpdatePublish, boolean updateMasterData) throws Exception {
    ProductDomainEventModel productDomainEventModel =
        toProductDomainEventModel(product, productSalesCategoryMapping, isBrandChanged, scoreUpdated, solrUpdated,
            isItemUpdatePublish);
    productDomainEventModel.setUpdateMasterData(updateMasterData);
    kafkaPublisher.send(DomainEventName.PRODUCT_PUBLISH, product.getProductCode(), productDomainEventModel);
    return productDomainEventModel;
  }

  @Override
  public ProductDomainEventModel publishProductForEdit(ProductPublishUpdateDTO productPublishUpdateDTO,
      ProductSalesCategoryMapping productSalesCategoryMapping, boolean isBrandChanged, boolean scoreUpdated,
      boolean solrUpdated, boolean isItemUpdatePublish, boolean updateMasterData) throws Exception {
    ProductDomainEventModel productDomainEventModel =
        toProductDomainEventModel(productPublishUpdateDTO.getProduct(), productSalesCategoryMapping,
            isBrandChanged, scoreUpdated, solrUpdated, isItemUpdatePublish);
    productDomainEventModel.setUpdateMasterData(updateMasterData);
    CommonUtil.productDomainEventModelFlagUpdateBasedOnProductOrItemLevelChange(productDomainEventModel,
        productPublishUpdateDTO.isProductLevelDataUpdated(), productPublishUpdateDTO.getUpdatedItemSkuCodes());
    kafkaPublisher.send(DomainEventName.PRODUCT_PUBLISH, productPublishUpdateDTO.getProduct().getProductCode(),
        productDomainEventModel);
    log.info("Published {} event for productCode : {} and with payload : {}", DomainEventName.PRODUCT_PUBLISH,
        productDomainEventModel.getProductCode(), productDomainEventModel);
    return productDomainEventModel;
  }

  @Override
  public VatUpdateDomainEventModel publishVatApplicableUpdateEvent(String itemCode, boolean vatApplicable) {
    log.info("Publishing vat applicable update event. topic = {}, itemCode = {}, vatApplicable = {} ",
        DomainEventName.VAT_UPDATE_PUBLISH, itemCode, vatApplicable);
    VatUpdateDomainEventModel vatUpdateDomainEventModel = new VatUpdateDomainEventModel(itemCode, vatApplicable);
    kafkaPublisher.send(DomainEventName.VAT_UPDATE_PUBLISH, itemCode, vatUpdateDomainEventModel);
    return vatUpdateDomainEventModel;
  }

  @Override
  public VatUpdateHistoryDomainEventModel publishVatApplicableExternalHistoryEvent(String requestId, String storeId,
      String productItemId, String itemCode, String itemName, String updatedBy, String oldValue, String newValue) {
    log.info(
        "Publishing vat applicable update external history event. topic = {}, requestId = {}, productItemId = {}, itemCode = {}, itemName = {}, oldValue = {}, newValue = {}",
        DomainEventName.VAT_UPDATE_EXTERNAL_HISTORY_PUBLISH, requestId, productItemId, itemCode, itemName, oldValue,
        newValue);
    VatUpdateHistoryDomainEventModel vatUpdateHistoryDomainEventModel =
        new VatUpdateHistoryDomainEventModel(requestId, storeId, productItemId, itemCode, itemName, updatedBy, oldValue,
            newValue);
    kafkaPublisher.send(DomainEventName.VAT_UPDATE_EXTERNAL_HISTORY_PUBLISH, itemCode,
        vatUpdateHistoryDomainEventModel);
    return vatUpdateHistoryDomainEventModel;
  }

  @Override
  public ProductAttributeExtractionModel publishProductAttributeExtractionBackfillingEvent(String storeId,
      String username, String productCode, String cnCategoryCode, String cnCategoryName, String extractionType) {
    log.info(
        "Publishing  product attribute extraction backfilling event. topic = {}, storeId = {}, username = {}, productCode = {}, cnCategoryCode = {}, cnCategoryName = {}, extractionType = {} ",
        DomainEventName.PRODUCT_ATTRIBUTE_EXTRACTION_BACKFILLING_PUBLISH, storeId, username, productCode,
        cnCategoryCode, cnCategoryName, extractionType);
    ProductAttributeExtractionModel productAttributeExtractionModel =
        new ProductAttributeExtractionModel(storeId, username, productCode, cnCategoryCode, cnCategoryName,
            extractionType);
    kafkaPublisher
        .send(DomainEventName.PRODUCT_ATTRIBUTE_EXTRACTION_BACKFILLING_PUBLISH, productCode,
            productAttributeExtractionModel);
    return productAttributeExtractionModel;
  }

  @Override
  public ProductDomainEventModel publishProductForMasterDataMigration(Product product,
    String migrationType) throws Exception {
    ProductDomainEventModel productDomainEventModel = toProductDomainEventModel(product, false,
      migrationType);
    kafkaPublisher.send(DomainEventName.PRODUCT_MASTER_DATA_MIGRATION, product.getProductCode(), productDomainEventModel);
    return productDomainEventModel;
  }

  private ProductDomainEventModel toProductDomainEventModel(Product product,
      ProductSalesCategoryMapping productSalesCategoryMapping, boolean isBrandChanged, boolean scoreUpdated,
      boolean solrUpdated, boolean isItemUpdatePublish) throws Exception {
    ProductDomainEventModel productDomainEventModel = new ProductDomainEventModel();
    convertProductToProductDomainEventModel(product, productDomainEventModel, false);
    log.info("push product to kafka: productId= {}, productCode= {}, markForDelete= {}, brandChanged= {}",
        productDomainEventModel.getId(), productDomainEventModel.getProductCode(),
        productDomainEventModel.isproductMarkForDelete(), isBrandChanged);
    if (Objects.nonNull(productSalesCategoryMapping)) {
      productDomainEventModel.setProductSalesCategoryMapping(productSalesCategoryMapping);
      Set<ProductChangeType> productChangeTypes = productDomainEventModel.getProductChangeTypes();
      productChangeTypes.add(ProductChangeType.CATEGORY_CHANGE);
      productDomainEventModel.setProductChangeTypes(productChangeTypes);
      productDomainEventModel.getUpdatedFields().add(UpdatedFields.CATEGORY_UPDATE.name());
    }
    productDomainEventModel.setBrandChanged(isBrandChanged);
    productDomainEventModel.setScoreUpdated(scoreUpdated);
    productDomainEventModel.setSolrUpdateRequired(solrUpdated);
    productDomainEventModel.setItemUpdatePublish(isItemUpdatePublish);
    if (isBrandChanged) {
      productDomainEventModel.getUpdatedFields().add(UpdatedFields.BRAND_UPDATE.name());
    }
    return productDomainEventModel;
  }

  @Override
  public BrandAuthDomainEventModel publishBrandAuthHistoryEvent(String brandCode, String sellerCode,
    BrandAuthorisationHistory brandAuthorisationHistory) {
    BrandAuthDomainEventModel authDomainEventModel =
      toBrandAuthHistoryDomainEventModel(brandAuthorisationHistory);
    kafkaPublisher.send(DomainEventName.BRAND_AUTH_HISTORY_EVENT, brandCode + DASH + sellerCode,
      authDomainEventModel);
    return authDomainEventModel;
  }

  @Override
  public void publishProductToBeDeleted(String storeId, List<String> productCodes) {
    for (String productCode : productCodes) {
      ProductCodeDomainEventModel productCodeDomainEventModel = new ProductCodeDomainEventModel(storeId, productCode);
      kafkaPublisher.send(DomainEventName.DELETE_REJECTED_PRODUCT, productCode, productCodeDomainEventModel);
      log.info("Publishing event : {}, payload : {} ", DomainEventName.DELETE_REJECTED_PRODUCT,
          productCodeDomainEventModel);
    }
  }

  @Override
  public void publishDeletedProduct(String storeId, String productCode) {
    ProductCodeDomainEventModel productCodeDomainEventModel = new ProductCodeDomainEventModel(storeId, productCode);
    kafkaPublisher.send(DomainEventName.DELETE_REJECTED_MERCHANT_PRODUCT, productCode, productCodeDomainEventModel);
    log.info("Publishing event : {}, payload : {} ", DomainEventName.DELETE_REJECTED_MERCHANT_PRODUCT,
        productCodeDomainEventModel);
  }

  @Override
  public void publishImagePathUpdateEvent(String storeId, String productCode, Set<Pair<String, String>> imagePaths) {
    ImagePathUpdateDomainEventModel imagePathUpdateDomainEventModel =
        ConverterUtil.toImagePathUpdateDomainEventModel(storeId, productCode, imagePaths);
    kafkaPublisher.send(DomainEventName.UPDATE_PRODUCT_ITEM_IMAGE_PATH, productCode, imagePathUpdateDomainEventModel);
    log.info("Publishing event : {}, payload : {} ", DomainEventName.UPDATE_PRODUCT_ITEM_IMAGE_PATH,
        imagePathUpdateDomainEventModel);
  }

  private BrandAuthDomainEventModel toBrandAuthHistoryDomainEventModel(
    BrandAuthorisationHistory brandAuthorisationHistory) {
    BrandAuthDomainEventModel brandAuthDomainEventModel = new BrandAuthDomainEventModel();
    BeanUtils.copyProperties(brandAuthorisationHistory, brandAuthDomainEventModel);
    brandAuthDomainEventModel.setStoreId(brandAuthorisationHistory.getStoreId());
    brandAuthDomainEventModel.setUsername(brandAuthorisationHistory.getUpdatedBy());
    return brandAuthDomainEventModel;
  }

  @Override
  public void publishTerminatedSellerSkuCleanupStatusEvent(
      TerminatedSellerSkuCleanupStatusEventModel terminatedSellerSkuCleanupStatusEventModel) {
    kafkaPublisher.send(kafkaTopicProperties.getTerminatedSellerSkuCleanupStatus(),
        terminatedSellerSkuCleanupStatusEventModel.getProductCode(),
        terminatedSellerSkuCleanupStatusEventModel);
    log.info("Published event : {}, with payload : {} ",
      kafkaTopicProperties.getTerminatedSellerSkuCleanupStatus(),
        terminatedSellerSkuCleanupStatusEventModel);
  }

  @Override
  public void publishTerminatedSellerSkuImageCleanupEvent(
      TerminatedSellerSkuImageCleanupEventModel terminatedSellerSkuImageCleanupEventModel) {
    kafkaPublisher.send(kafkaTopicProperties.getTerminatedSellerSkuImageCleanup(),
        terminatedSellerSkuImageCleanupEventModel.getProductCode(),
        terminatedSellerSkuImageCleanupEventModel);
    log.info("Published event : {}, payload : {} ", kafkaTopicProperties.getTerminatedSellerSkuImageCleanup(),
        terminatedSellerSkuImageCleanupEventModel);
  }

  @Override
  public void publishRestrictedKeywordHistory(
    List<RestrictedKeywordHistoryEventModel> restrictedKeywordHistoryEventModelList) {
    log.info("Publishing event : {}, payload : {} ",
      kafkaTopicProperties.getRestrictedKeywordHistoryEvent(),
      restrictedKeywordHistoryEventModelList);
    restrictedKeywordHistoryEventModelList.forEach(
      restrictedKeywordHistoryEvent -> kafkaPublisher.send(
        kafkaTopicProperties.getRestrictedKeywordHistoryEvent(),
        restrictedKeywordHistoryEvent.getKeywordId(), restrictedKeywordHistoryEvent));
  }

  @Override
  public void publishCategoryUpdateHistory(
      List<CategoryHistoryEventModel> categoryHistoryEventModels) {
    log.info("Publishing event : {}, payload : {}",
        kafkaTopicProperties.getCategoryUpdateHistoryEvent(), categoryHistoryEventModels);
    categoryHistoryEventModels.forEach(categoryHistoryEventModel -> kafkaPublisher.send(
        kafkaTopicProperties.getCategoryUpdateHistoryEvent(),
        categoryHistoryEventModel.getCategoryCode(), categoryHistoryEventModel));
  }

  @Override
  public void publishBrandHistory(BrandHistoryEventModel brandHistoryEventModel) {
    log.info(EVENT_PUBLISH_LOG, brandHistoryEventModel,
      kafkaTopicProperties.getBrandHistoryEvent());
    kafkaPublisher.send(kafkaTopicProperties.getBrandHistoryEvent(),
      brandHistoryEventModel.getBrandRequestCode(), brandHistoryEventModel);
  }

  @Override
  public void publishPBPAttributeMigrationEvent(ProductAttributeValue productAttributeValue,
      String productCode) {
    PBPProductAttributeBackFillingEventModel pbpProductAttributeBackFillingEventModel =
        new PBPProductAttributeBackFillingEventModel();
    pbpProductAttributeBackFillingEventModel.setAttributeCode(
        productAttributeValue.getProductAttribute().getAttribute().getAttributeCode());
    pbpProductAttributeBackFillingEventModel.setAttributeId(
        productAttributeValue.getProductAttribute().getAttribute().getId());
    pbpProductAttributeBackFillingEventModel.setProductCode(productCode);
    pbpProductAttributeBackFillingEventModel.setAttributeName(
        productAttributeValue.getProductAttribute().getAttribute().getName());
    String attributeValue = getAttributeValueBasedOnAttributeType(productAttributeValue);
    if (productAttributeMigrationPredefinedAttributeValueFilteringEnabled && StringUtils.isBlank(
        attributeValue)) {
      return;
    }
    pbpProductAttributeBackFillingEventModel.setAttributeValue(attributeValue);
    pbpProductAttributeBackFillingEventModel.setSkuValue(
        productAttributeValue.getProductAttribute().getAttribute().isSkuValue());
    String event = kafkaTopicProperties.getPbpAttributeMigrationEvent();
    log.info("Publishing Product Attribute Back Filling Event {} with message {}", event,
        pbpProductAttributeBackFillingEventModel);
    kafkaPublisher.send(event, pbpProductAttributeBackFillingEventModel);

  }

  private String getAttributeValueBasedOnAttributeType(ProductAttributeValue productAttributeValue) {
    String attributeType =
        Optional.ofNullable(productAttributeValue).map(ProductAttributeValue::getProductAttribute)
            .map(ProductAttribute::getAttribute).map(Attribute::getAttributeType).map(Enum::name)
            .orElse(StringUtils.EMPTY);
    if (AttributeType.DEFINING_ATTRIBUTE.name().equals(attributeType)) {
      return productAttributeValue.getAllowedAttributeValue().getValue();
    } else if (AttributeType.DESCRIPTIVE_ATTRIBUTE.name().equals(attributeType)) {
      return productAttributeValue.getDescriptiveAttributeValue();
    } else if (AttributeType.PREDEFINED_ATTRIBUTE.name().equals(attributeType)) {
      if(productAttributeMigrationPredefinedAttributeValueFilteringEnabled) {
        return Optional.ofNullable(productAttributeValue)
            .map(ProductAttributeValue::getPredefinedAllowedAttributeValue)
            .map(PredefinedAllowedAttributeValue::getValue).orElse(StringUtils.EMPTY);
      }
      else {
        return productAttributeValue.getPredefinedAllowedAttributeValue().getValue();
      }
    }
    return null;
  }

  @Override
  public void publishVendorEvent(VendorPublishEventModel vendorPublishEventModel) {
    log.info(EVENT_PUBLISH_LOG, vendorPublishEventModel,
        kafkaTopicProperties.getPublishVendorEvent());
    kafkaPublisher.send(kafkaTopicProperties.getPublishVendorEvent(),
        vendorPublishEventModel.getProductCode(), vendorPublishEventModel);
  }

  @Override
  public void publishInternalHistoryEvent(InternalProductHistoryEventModel internalProductHistoryEventModel) {
    log.info(EVENT_PUBLISH_LOG, internalProductHistoryEventModel,
        kafkaTopicProperties.getProductInternalHistoryEvent());
    kafkaPublisher.send(kafkaTopicProperties.getProductInternalHistoryEvent(),
        internalProductHistoryEventModel.getProductCode(), internalProductHistoryEventModel);
  }
}
