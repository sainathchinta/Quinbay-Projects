package com.gdn.x.productcategorybase.service.impl;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.gdn.x.productcategorybase.dto.request.ProductMasterDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.AuditTrailDto;
import com.gdn.x.productcategorybase.dto.response.AuditTrailListResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import com.gdn.x.productcategorybase.entity.ProductItemUomInfo;
import com.gdn.x.productcategorybase.service.ProductItemUomInfoService;
import com.gdn.x.productcategorybase.dto.request.ProductBrandDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductMasterDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.AuditTrailDto;
import com.gdn.x.productcategorybase.dto.response.AuditTrailListResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationServiceBean;
import jakarta.annotation.PostConstruct;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.productcategorybase.dto.MigrationPayload;
import com.gdn.x.productcategorybase.dto.VideoDTO;
import com.gdn.x.productcategorybase.dto.response.BasicInfoProductResponse;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.exception.ValidationException;
import com.gdn.x.productcategorybase.service.ProductItemAttributeValueService;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.hibernate.Hibernate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnDigestUtil;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.inventory.dto.WarehouseMasterSKUEvent;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.ProductPublishEventType;
import com.gdn.x.productcategorybase.Status;
import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.gdn.x.productcategorybase.dto.CategoryChangeDTO;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.GeneratedProductImagesPathDto;
import com.gdn.x.productcategorybase.dto.GeneratedProductItemImagesPathDto;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.ImagePathDTO;
import com.gdn.x.productcategorybase.dto.LocationPathAndCommonImage;
import com.gdn.x.productcategorybase.dto.MasterProductDataUpdateDTO;
import com.gdn.x.productcategorybase.dto.ProductAndItemLevelUpdatesDTO;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateDTO;
import com.gdn.x.productcategorybase.dto.ProductDTO;
import com.gdn.x.productcategorybase.dto.ProductDetailEditDTO;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.dto.ReplaceProductImagesDTO;
import com.gdn.x.productcategorybase.dto.ReplaceProductItemImagesDTO;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateDTO;
import com.gdn.x.productcategorybase.dto.UpdateNeedRevisionDTO;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.request.CopyImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.EditProductDetailRequest;
import com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.NeedRevisionConfigRequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.NewlySavedItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductSalesCategoryMappingResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.GdnBaseEntity;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductImageCleanup;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.entity.SystemParameter;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.repository.ProductImageCleanupRepository;
import com.gdn.x.productcategorybase.repository.ProductImageRepository;
import com.gdn.x.productcategorybase.repository.ProductItemImageRepository;
import com.gdn.x.productcategorybase.repository.ProductRepository;
import com.gdn.x.productcategorybase.repository.ProductRepositoryCustom;
import com.gdn.x.productcategorybase.repository.ProductScoreUpdateRepository;
import com.gdn.x.productcategorybase.service.AllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.ApplicationConfigPropertiesService;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.CategoryReferenceService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.CategoryShippingService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.FileStorageService;
import com.gdn.x.productcategorybase.service.ImageService;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductAttributeExtractionService;
import com.gdn.x.productcategorybase.service.ProductAttributeService;
import com.gdn.x.productcategorybase.service.ProductAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductCategoryService;
import com.gdn.x.productcategorybase.service.ProductItemService;
import com.gdn.x.productcategorybase.service.ProductItemServiceWrapper;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.service.SystemParameterService;
import com.gdn.x.productcategorybase.service.brand.BrandService;
import com.gdn.x.productcategorybase.util.CommonUtil;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import com.gdn.x.productcategorybase.util.ObjectCopyUtil;
import com.gdn.x.productcategorybase.util.ProductImageUtil;
import com.gdn.x.productcategorybase.util.ProductUtil;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Lazy
@Service
@Transactional(readOnly = true)
public class ProductServiceBean implements ProductService {

  private static final String EXISTING_UPC_CODE_ON_PRODUCT =
      "There is existing upc code %s in product %s (id = %s). You must change the upc code before"
          + " continue your last action.";
  private static final Logger LOG = LoggerFactory.getLogger(ProductServiceBean.class);
  private static final String HYPHEN = "-";
  private static final String SPACE = " ";
  private static final String WARNA = "Warna";
  private static final String COLOR = "Color";
  private static final String ATTRIBUTE_CODE_FOR_BRAND = "BR-M036969";

  @Autowired
  private AllowedAttributeValueService allowedAttributeValueService;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Autowired
  private AttributeService attributeService;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private PredefinedAllowedAttributeValueService predefinedAllowedAttributeValueService;

  @Lazy
  @Autowired
  private ProductItemService productItemService;

  @Autowired
  private ProductItemUomInfoService productItemUomInfoService;

  @Autowired
  private ProductRepository repository;

  @Autowired
  private ProductRepositoryCustom productRepositoryCustom;

  @Autowired
  private ProductScoreUpdateRepository productScoreUpdateRepository;

  @Autowired
  private ProductImageRepository productImageRepository;

  @Autowired
  private ProductItemImageRepository productItemImageRepository;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  private ApplicationConfigPropertiesService applicationConfigPropertiesService;

  @Autowired
  private CategoryReferenceService categoryReferenceService;

  @Autowired
  private ProductCategoryService productCategoryService;

  @Autowired
  private ProductAttributeService productAttributeService;

  @Autowired
  private ProductAttributeValueService productAttributeValueService;

  @Autowired
  private ProductItemAttributeValueService productItemAttributeValueService;

  @Autowired
  private ImageService imageService;

  @Autowired
  @Lazy
  private ProductItemServiceWrapper productItemServiceWrapper;

  @Autowired
  private ApplicationContext applicationContext;

  @Autowired
  private BrandService brandService;

  @Autowired
  private CategoryShippingService categoryShippingService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private ProductImageCleanupRepository productImageCleanupRepository;

  @Autowired
  private ProductAttributeExtractionService productAttributeExtractionService;

  @Autowired
  private FileStorageService fileStorageService;

  @Value("${add.delete.variants.switch}")
  private boolean addDeleteVariantsSwitch;

  @Value("${avoid.brand.delete}")
  private boolean avoidBrandDelete;

  @Value("${brand.attribute.id}")
  private String brandAttributeId;

  @Value("${remove.duplicate.attributes}")
  private boolean removeDuplicateAttributes;

  @Value("${process.variant.image.newly.added.items}")
  private boolean processVariantImageNewlyAddedItems;

  @Value("${validate.dimension.warehouse.event.switch.enabled}")
  private boolean validateDimensionSwitch;

  @Value("${unique.main.image.at.item.level}")
  private boolean uniqueMainImageAtItemLevel;

  @Value("${relax.active.image.check.in.nr.image.update}")
  private boolean relaxActiveImageCheckInNRImageUpdate;

  @Value("${set.missing.attributes.in.creation.enabled}")
  private boolean setMissingAttributesInCreationEnabled;

  @Value("${validate.missing.allowed.attribute}")
  private boolean validateMissingAllowedAttribute;

  @Value("${imei.migration.enabled}")
  private boolean imeiMigrationEnabled;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private BrandAuthorisationServiceBean brandAuthorisationServiceBean;

  @PostConstruct
  public void init() {
    ProductImageUtil.setFileStorageService(fileStorageService);
    ConverterUtil.setFileStorageService(fileStorageService);
  }

  private void activatedDeactivatedProduct(Product product, boolean isActivated) throws Exception {
    product.setActivated(isActivated);
    this.update(product);
    evictProductCache(product.getStoreId(), product);
    applicationCacheServiceBean.evictProductItemsCacheByStoreIdAndProductId(product.getStoreId(), product.getId());
  }

  @Value("${image.source.directory}")
  private String imageSourceDirectory;

  @Value("${full.image.source.directory}")
  private String fullImageSourceDirectory;

  @Value("${medium.image.source.directory}")
  private String mediumImageSourceDirectory;

  @Value("${thumbnail.image.source.directory}")
  private String thumbnailImageSourceDirectory;

  @Value("${set.common.image.main.image}")
  private boolean setCommonImageAsMainImage;

  @Value("${regenerate.product.images}")
  private boolean regenerateProductImages;

  @Value("${validate.at.least.one.item.for.a.product}")
  private boolean validateAtleastOneItemForAProduct;

  @Value("${validate.product.item.attributes}")
  private boolean validateProductItemAttributes;

  @Value("${product.basic.info.fetch.batch.size}")
  private int productBasicInfoFetchBatchSize;

  @Override
  @Transactional(readOnly = false, propagation = Propagation.REQUIRES_NEW)
  public void activateProduct(String storeId, String id) throws Exception {
    Product product = getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(storeId, id);
    productItemServiceWrapper.setProductItemsCached(storeId, product, false);
    for (ProductItem productItem : product.getProductItems()) {
      if (!productItem.isMarkForDelete()) {
        productItem.setActivated(true);
      }
    }
    this.activatedDeactivatedProduct(product, true);
  }


  /**
   * Compare old productAttribute and new productAttribute, delete productAttributeValue of old
   * productAttribute if not existing in new productAttribute, and create new productAttributeValue
   * in old productAttribute if existing in new productAttribute but not in old productAttribute
   * <p/>
   * <strong>changes will only apply on old productAttribute</strong>
   *
   * @param storeId
   * @param oldProductAttribute
   * @param newProductAttribute
   */
  private void adjustProductAttributeValue(String storeId, ProductAttribute oldProductAttribute,
      ProductAttribute newProductAttribute) {
    for (ProductAttributeValue oldProductAttributeValue : oldProductAttribute.getProductAttributeValues()) {
      if (!oldProductAttributeValue.isMarkForDelete()) {
        boolean identicalValue = false;
        for (ProductAttributeValue newProductAttributeValue : newProductAttribute.getProductAttributeValues()) {
          if (oldProductAttributeValue.getId().equals(newProductAttributeValue.getId())) {
            identicalValue = true;
            if (AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(oldProductAttribute.getAttribute().getAttributeType())) {
              oldProductAttributeValue.setDescriptiveAttributeValue(newProductAttributeValue
                  .getDescriptiveAttributeValue());
            } else if (AttributeType.PREDEFINED_ATTRIBUTE.equals(oldProductAttribute.getAttribute().getAttributeType())) {
              oldProductAttributeValue.setPredefinedAllowedAttributeValue(newProductAttributeValue
                  .getPredefinedAllowedAttributeValue());
            }
          }
        }
        if (!identicalValue) {
          oldProductAttributeValue.setMarkForDelete(true);
        }
      }
    }
    for (ProductAttributeValue newProductAttributeValue : newProductAttribute.getProductAttributeValues()) {
      if (newProductAttributeValue.getId() == null) {
        ProductAttributeValue productAttributeValue = new ProductAttributeValue();
        BeanUtils.copyProperties(newProductAttributeValue, productAttributeValue, "productAttribute",
            "allowedAttributeValue", "predefinedAllowedAttributeValue", "descriptiveAttributeValueType");

        productAttributeValue.setProductAttribute(oldProductAttribute);
        DescriptiveAttributeValueType descriptiveAttributeValueType =
            newProductAttributeValue.getDescriptiveAttributeValueType();
        productAttributeValue.setDescriptiveAttributeValueType(descriptiveAttributeValueType);
        if (descriptiveAttributeValueType.toString().equals(DescriptiveAttributeValueType.NONE.toString())) {
          productAttributeValue.setAllowedAttributeValue(checkAllowAttributeId(storeId, newProductAttributeValue));
        } else if (descriptiveAttributeValueType.toString().equals(DescriptiveAttributeValueType.PREDEFINED.toString())) {
          if(!productAttributeValue.getProductAttribute().getAttribute().isSkuValue()){
            if(Objects.nonNull(newProductAttributeValue.getPredefinedAllowedAttributeValue())){
              productAttributeValue.setPredefinedAllowedAttributeValue(this.predefinedAllowedAttributeValueService
                      .findByStoreIdAndId(storeId, newProductAttributeValue.getPredefinedAllowedAttributeValue().getId()));
            } else {
              LOG.info("Set Predefined Allowed Attribute value as null");
              productAttributeValue.setPredefinedAllowedAttributeValue(null);
            }
          }
        } else {
          productAttributeValue.setDescriptiveAttributeValue(newProductAttributeValue.getDescriptiveAttributeValue());
        }
        oldProductAttribute.getProductAttributeValues().add(productAttributeValue);
        /**
         * newProductAttributeValue.setProductAttribute(oldProductAttribute ); if
         * (newProductAttributeValue.getDescriptiveAttributeValueType ().toString()
         * .equals(DescriptiveAttributeValueType.NONE.toString())) { newProductAttributeValue
         * newProductAttributeValue.getAllowedAttributeValue() .getId())); }
         * oldProductAttribute.getProductAttributeValues(). add(newProductAttributeValue);
         */

      }
    }
  }

  private AllowedAttributeValue checkAllowAttributeId(String storeId, ProductAttributeValue productAttributeValue) {
    if (productAttributeValue.getProductAttribute().getAttribute().getAttributeType()
        .equals(AttributeType.DESCRIPTIVE_ATTRIBUTE)) {
      return null;
    }
    if (validateMissingAllowedAttribute && Objects.isNull(productAttributeValue.getAllowedAttributeValue())) {
      log.error("Allowed Attribute Value is null for Product Attribute Value: {} ",
          productAttributeValue);
      throw new ValidationException(ErrorCategory.VALIDATION.getCode(),
          ErrorMessage.INVALID_ATTRIBUTE_VALUE.getMessage());
    }
    return this.allowedAttributeValueService
        .findByStoreIdAndId(storeId, productAttributeValue.getAllowedAttributeValue().getId());
  }

  private boolean isDefiningAttributeOrVariantCreationTrue(Attribute attribute) {
    return AttributeType.DEFINING_ATTRIBUTE.equals(attribute.getAttributeType()) || attribute.isVariantCreation();
  }

  private boolean isNotDefiningAttributeAndVariantCreationFalse(Attribute attribute) {
    return !AttributeType.DEFINING_ATTRIBUTE.equals(attribute.getAttributeType()) &&
        !attribute.isVariantCreation();
  }

  /**
   * Compare old product and new product, delete productItem of old product if not existing in new
   * product, and create new productItem in old product if existing in new product but not in old
   * product
   * <strong>changes will only apply on old product</strong>
   * @param savedProduct
   */

  @Override
  public Product checkBrandChanges(Product savedProduct) {
    for (ProductItem savedProductItem : savedProduct.getProductItems()) {
      for (ProductItemAttributeValue value : savedProductItem.getProductItemAttributeValues()) {
        if (Constants.BRAND.equals(value.getAttribute().getName()) && !StringUtils.equals(value.getValue(),
            savedProduct.getBrand())) {
          value.setValue(savedProduct.getBrand());
        }
      }
    }
    return savedProduct;
  }

  @Override
  @Transactional(readOnly = false)
  public Pair<Map<String, ProductDTO>, Map<ProductItem, String>> adjustProductItem(String storeId,
    Product oldProduct, Product newProduct, boolean computecommonImage,
    boolean resetExtractedAttributeValue, boolean combinedUpdateForEditEnabled) throws Exception {
    Map<ProductItem, String> itemWithOldVatValueMap = new HashMap<>();
    Map<String, Attribute> attributeMap = new HashMap<>();
    Set<String> deletedItems = new HashSet<>();
    log.info("Updating Content for product code : {} , with request : {} ",
      newProduct.getProductCode(), newProduct);
    newProduct.getProductItems().forEach(
      newProductItem -> copyProductItemAndHandleItemImagesForAddDeleteRequest(storeId, oldProduct,
        newProduct, newProductItem, attributeMap));
    for (ProductAttribute oldProductAttribute : oldProduct.getProductAttributes()) {
      boolean identicalValue = false;
      // will be true if attributes , categories, images or items are unchanged
      if (!oldProductAttribute.isMarkForDelete()) {
        for (ProductAttribute newProductAttribute : newProduct.getProductAttributes()) {
          if (oldProductAttribute.getId().equals(newProductAttribute.getId())
              && !newProductAttribute.isMarkForDelete()) {
            identicalValue = true;
            this.adjustProductAttributeValue(storeId, oldProductAttribute, newProductAttribute);
          }
        }
        if (!identicalValue) {
          this.setMarkForDeleteToThisProductAttributeAndProductAttributeValues(oldProductAttribute);
        }
      }
    }

    updateAttributeMap(storeId, newProduct, attributeMap);
    for (ProductAttribute newProductAttribute : newProduct.getProductAttributes()) {
      if (newProductAttribute.getId() == null) {
        newProductAttribute.setProduct(oldProduct);
        newProductAttribute.setAttribute(attributeMap.get(newProductAttribute.getAttribute()
            .getId()));
        oldProduct.getProductAttributes().add(newProductAttribute);
      }
    }


    List<ProductCategory> newProductCategories = new ArrayList<>();
    for (ProductCategory newProductCategory : newProduct.getProductCategories()) {
      boolean identicalValue = false;
      for (ProductCategory oldProductCategory : oldProduct.getProductCategories()) {
        if (oldProductCategory.getCategory().getId().equals(newProductCategory.getCategory().getId())) {
          identicalValue = true;
          oldProductCategory.setMarkForDelete(newProductCategory.isMarkForDelete());
        } else {
          oldProductCategory.setMarkForDelete(true);
        }
      }
      if (!identicalValue) {
        newProductCategory.setProduct(oldProduct);
        newProductCategory.setCategory(
            this.categoryService.getCategoryByStoreIdAndIdCached(
                storeId, newProductCategory.getCategory().getId()));
        newProductCategories.add(newProductCategory);
      }
    }

    if (!newProductCategories.isEmpty()) {
      oldProduct.getProductCategories().addAll(newProductCategories);
    }
    Set<String> deletedImagesPath = new HashSet<>();
    Set<String> activeImagesPath = new HashSet<>();
    List<ProductImage> tempProductImages = new ArrayList<ProductImage>();
    for (ProductImage newProductImage : newProduct.getProductImages()) {
      boolean same = false;
      for (ProductImage oldProductImage : oldProduct.getProductImages()) {
        if (!oldProductImage.isMarkForDelete() && (oldProductImage.getId().equals(newProductImage.getId()))) {
          same = true;
          activeImagesPath.add(oldProductImage.getLocationPath());
          break;
        }
      }

      if (!same) {
        newProductImage.setProduct(oldProduct);
        tempProductImages.add(newProductImage);
        activeImagesPath.add(newProductImage.getLocationPath());
      }
    }

    for (ProductImage oldProductImage : oldProduct.getProductImages()) {
      ProductImage tempProductImage = null;
      boolean same = false;
      if (!oldProductImage.isMarkForDelete()) {
        tempProductImage = newProduct.getProductImages()
          .stream().filter(productImage -> Objects.nonNull(productImage.getId()))
          .filter(newProductImage -> oldProductImage.getId().equals(newProductImage.getId()))
          .findFirst()
          .orElse(null);

       same = Objects.nonNull(tempProductImage);

        if (!same) {
          oldProductImage.setMarkForDelete(true);
        } else {
          oldProductImage.setLocationPath(tempProductImage.getLocationPath());
          oldProductImage.setMainImages(tempProductImage.isMainImages());
          oldProductImage.setMarkForDelete(tempProductImage.isMarkForDelete());
          oldProductImage.setSequence(tempProductImage.getSequence());
          oldProductImage.setHashCode(tempProductImage.getHashCode());
          oldProductImage.setActive(tempProductImage.isActive());
          oldProductImage.setOriginalImage(tempProductImage.getOriginalImage());
          oldProductImage.setCommonImage(tempProductImage.isCommonImage());
        }

        if (oldProductImage.isMarkForDelete()) {
          deletedImagesPath.add(oldProductImage.getLocationPath());
        } else {
          activeImagesPath.add(oldProductImage.getLocationPath());
        }
      } else {
        deletedImagesPath.add(oldProductImage.getLocationPath());
      }
    }

    if (!tempProductImages.isEmpty()) {
      oldProduct.getProductImages().addAll(tempProductImages);
    }
    ProductServiceBean.LOG.debug("Active image paths are  " + activeImagesPath);

    List<ProductItem> modifiedProductItems = new ArrayList<>();
    for (ProductItem oldProductItem : oldProduct.getProductItems()) {
      ProductItem tempProductItem = null;
      boolean identicalValue = false;
      if (!oldProductItem.isMarkForDelete()) {
        for (ProductItem newProductItem : newProduct.getProductItems()) {
          if (new String(oldProductItem.getHash()).equals(new String(newProductItem.getHash()))) {
            identicalValue = true;
            tempProductItem = newProductItem;
            oldProductItem.setUpcCode(newProductItem.getUpcCode());
            oldProductItem.setDangerousGoodsLevel(newProductItem.getDangerousGoodsLevel());
            if (ranchIntegrationEnabled) {
              if (StringUtils.isNotBlank(newProductItem.getOmniChannelSku())) {
                oldProductItem.setOmniChannelSku(newProductItem.getOmniChannelSku());
              }
              if (Objects.nonNull(newProductItem.getProductItemUomInfo())) {
                if (Objects.isNull(oldProductItem.getProductItemUomInfo())) {
                  ProductItemUomInfo productItemUomInfo = newProductItem.getProductItemUomInfo();
                  productItemUomInfo.setSellerCode(oldProductItem.getCreatedMerchant());
                  productItemUomInfo.setProductItem(oldProductItem);
                  oldProductItem.setProductItemUomInfo(productItemUomInfo);
                } else {
                  ProductItemUomInfo existingProductItemUomInfo = oldProductItem.getProductItemUomInfo();
                  BeanUtils.copyProperties(newProductItem.getProductItemUomInfo(), existingProductItemUomInfo, "id", "version", "createdDate", "storeId", "createdBy", "sellerCode");
                  existingProductItemUomInfo.setProductItem(oldProductItem);
                  oldProductItem.setProductItemUomInfo(existingProductItemUomInfo);
                }
              }
            }
            if (addDeleteVariantsSwitch && newProductItem.isMarkForDelete()) {
              log.info("Item : {} getting deleted for productCode : {} ", oldProductItem.getSkuCode(),
                oldProduct.getProductCode());
              oldProductItem.setMarkForDelete(true);
              deletedItems.add(oldProductItem.getSkuCode());
              identicalValue = false;
            }
            if (oldProductItem.isActivated() && oldProductItem.isViewable()) {
              oldProductItem.setContentChanged(newProductItem.isContentChanged());
            }
          }
        }
        if (!identicalValue) {
          oldProductItem.setMarkForDelete(true);
        } else {
          if (!CollectionUtils.isEmpty(tempProductItem.getProductItemAttributeValues())) {
            List<ProductItemAttributeValue> oldProductItemAttributeValues = new ArrayList<>();
            for (ProductItemAttributeValue oldProductItemAttributeValue : oldProductItem.getProductItemAttributeValues()) {
              for (ProductItemAttributeValue productItemAttributeValue : tempProductItem.getProductItemAttributeValues()) {
                if (Objects.nonNull(productItemAttributeValue.getAttribute())) {
                  if (productItemAttributeValue.getAttribute().isVariantCreatingUI() && productItemAttributeValue.getAttribute().isScreeningMandatory()
                    && productItemAttributeValue.getAttribute().getAttributeCode().equals(oldProductItemAttributeValue.getAttribute().getAttributeCode())) {
                    LOG.debug("Source : {}", productItemAttributeValue);
                    LOG.debug("Destination : {}", oldProductItemAttributeValue);
                    oldProductItemAttributeValue.setValue(productItemAttributeValue.getValue());
                  }
                }
              }
              oldProductItemAttributeValues.add(oldProductItemAttributeValue);
            }
            oldProductItem.setProductItemAttributeValues(oldProductItemAttributeValues);
          }
          List<ProductItemImage> tempProductItemImages = new ArrayList<>();
          Map<String, ProductItemImage> productItemImages =
            oldProductItem.getProductItemImages().stream()
              .filter(productItemImage -> !productItemImage.isMarkForDelete()).collect(
                Collectors.toMap(ProductItemImage::getId, Function.identity(), (image1, image2) -> image1));
          for (ProductItemImage newProductItemImage : tempProductItem.getProductItemImages()) {
            boolean sameItem;
            sameItem = productItemImages.containsKey(newProductItemImage.getId());
            if (!sameItem) {
              newProductItemImage.setProductItem(oldProductItem);
              tempProductItemImages.add(newProductItemImage);
              activeImagesPath.add(newProductItemImage.getLocationPath());
            }
          }

          Map<String, ProductItemImage> existingImageIdImageMapForNewProductItem = new HashMap<>();
          if (!CollectionUtils.isEmpty(tempProductItem.getProductItemImages())) {
            existingImageIdImageMapForNewProductItem =
              tempProductItem.getProductItemImages().stream()
                .filter(productItemImage -> !productItemImage.isMarkForDelete())
                .filter(productItemImage -> StringUtils.isNotBlank(productItemImage.getId()))
                .collect(Collectors.toMap(ProductItemImage::getId, Function.identity()));
          }
          for (ProductItemImage oldProductItemImage : oldProductItem.getProductItemImages()) {
            ProductItemImage tempProductItemImage;
            if (!oldProductItemImage.isMarkForDelete()) {
              tempProductItemImage = existingImageIdImageMapForNewProductItem.get(oldProductItemImage.getId());
              if (Objects.isNull(tempProductItemImage)) {
                oldProductItemImage.setMarkForDelete(true);
              } else {
                oldProductItemImage.setLocationPath(tempProductItemImage.getLocationPath());
                oldProductItemImage.setMainImages(tempProductItemImage.isMainImages());
                oldProductItemImage.setMarkForDelete(tempProductItemImage.isMarkForDelete());
                oldProductItemImage.setSequence(tempProductItemImage.getSequence());
                oldProductItemImage.setHashCode(tempProductItemImage.getHashCode());
                oldProductItemImage.setActive(tempProductItemImage.isActive());
                oldProductItemImage.setOriginalImage(tempProductItemImage.getOriginalImage());
                oldProductItemImage.setCommonImage(tempProductItemImage.isCommonImage());
              }
              if (oldProductItemImage.isMarkForDelete()) {
                deletedImagesPath.add(oldProductItemImage.getLocationPath());
              } else {
                activeImagesPath.add(oldProductItemImage.getLocationPath());
              }
            } else {
              deletedImagesPath.add(oldProductItemImage.getLocationPath());
            }
          }
          if (CollectionUtils.isNotEmpty(tempProductItemImages)) {
            oldProductItem.getProductItemImages().addAll(tempProductItemImages);
          }
          if (!Objects.equals(oldProductItem.getVatApplicable(), tempProductItem.getVatApplicable())) {
            itemWithOldVatValueMap.put(oldProductItem, String.valueOf(oldProductItem.getVatApplicable()));
            oldProductItem.setVatApplicable(tempProductItem.getVatApplicable());
          }
        }
        modifiedProductItems.add(oldProductItem);
      }
      oldProduct.setProductItems(modifiedProductItems);
    }
    ProductUtil.validateProductItemAttributes(oldProduct, validateProductItemAttributes);
    ProductImageUtil.setCommonImageFlagForProductAndItemImages(oldProduct, computecommonImage);
    ProductImageUtil.checkCommonImageMaxCount(false, true, false, false, false, oldProduct, new HashSet<>());
    ProductUtil.resetExtractedAttributeValueFlag(oldProduct, resetExtractedAttributeValue);
    LOG.info("Final product to update : {}", oldProduct);
    if (validateAtleastOneItemForAProduct && checkAllItemsAreMfdTrue(oldProduct)) {
      throw new ValidationException(ErrorCategory.VALIDATION.getCode(),
          ErrorMessage.PRODUCT_SHOULD_HAVE_AT_LEAST_ONE_VARIANT.getMessage());
    }
    if (!combinedUpdateForEditEnabled) {
      ProductImageUtil.resetProductImages(oldProduct, regenerateProductImages);
      repository.saveAndFlush(oldProduct);
      deletedImagesPath.removeAll(activeImagesPath);
      saveProductImageCleanup(oldProduct.getProductCode(), deletedImagesPath, storeId);
    }
    Map<ProductItem, String> itemWithOldVatValueMapDTO = itemWithOldVatValueMap.entrySet().stream()
      .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    ProductDTO oldProductDTO = ConverterUtil.convertProductToDTO(oldProduct);
    oldProductDTO.setDeletedItems(deletedItems);
    return Pair.of(
      ImmutableMap.of(Constants.NEW_PRODUCT, ConverterUtil.convertProductToDTO(newProduct),
        Constants.OLD_PRODUCT, oldProductDTO),
      itemWithOldVatValueMapDTO);
  }

  private boolean checkAllItemsAreMfdTrue(Product product) {
    return product.getProductItems().stream()
        .noneMatch(Predicate.not(ProductItem::isMarkForDelete));
  }


  private void copyProductItemAndHandleItemImagesForAddDeleteRequest(String storeId, Product oldProduct, Product newProduct,
    ProductItem newProductItem, Map<String, Attribute> attributeMap) {
    boolean same = false;
    for (ProductItem oldProductItem : oldProduct.getProductItems()) {
      if (!oldProductItem.isMarkForDelete()) {
        if (new String(oldProductItem.getHash()).equals(new String(newProductItem.getHash())) && !newProductItem.isNewlyAddedItem()) {
          // it will always overwrite generated item name
          String oldProductItemDefiningValuesSpaceSeparated = oldProductItem.getProductItemAttributeValues().stream()
            .filter(oldProductItemAttributeValue -> isDefiningAttributeOrVariantCreationTrue(
              oldProductItemAttributeValue.getAttribute())).sorted(Comparator.comparing(
              oldProductItemAttributeValue -> oldProductItemAttributeValue.getAttribute().getId()))
            .map(ProductItemAttributeValue::getValue).collect(Collectors.joining(StringUtils.SPACE));
          String newProductItemName =
            newProduct.getName() + StringUtils.SPACE + oldProductItemDefiningValuesSpaceSeparated;
          oldProductItem.setGeneratedItemName(newProductItemName);
        }
        if (!oldProductItem.isMarkForDelete()) {
          if (new String(oldProductItem.getHash()).equals(new String(newProductItem.getHash()))) {
            same = true;
          }
        } else {
          if (new String(oldProductItem.getHash()).equals(new String(newProductItem.getHash()))
            && !newProductItem.isNewlyAddedItem()) {
            same = true;
            oldProductItem.setMarkForDelete(false);
          }
        }
      }
    }

    if (!same) {
      ProductItem productItem = new ProductItem();
      BeanUtils.copyProperties(newProductItem,
        productItem,
        "product",
        "productItemAttributeValues");
      updateAttributeMapForNewProductItem(storeId, newProductItem, attributeMap);
      for (ProductItemAttributeValue value : newProductItem.getProductItemAttributeValues()) {
        ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
        BeanUtils.copyProperties(value, productItemAttributeValue, "productItem", "attribute");
        productItemAttributeValue.setAttribute(attributeMap.get(value.getAttribute().getId()));
        if (value.getAttribute().getName().equals("Brand") && !value.getValue().equals(newProduct.getBrand())){
          value.setValue(newProduct.getBrand());
        }
        productItemAttributeValue.setProductItem(productItem);
        if (StringUtils.isEmpty(productItemAttributeValue.getStoreId())) {
          productItemAttributeValue.setStoreId(storeId);
        }
        productItem.getProductItemAttributeValues().add(productItemAttributeValue);
      }
      if (addDeleteVariantsSwitch) {
        List<ProductItemImage> productItemImageList = new ArrayList<>();
        for (ProductItemImage value : newProductItem.getProductItemImages()) {
          ProductItemImage productItemImage = new ProductItemImage();
          BeanUtils.copyProperties(value, productItemImage, "productItem");
          productItemImage.setProductItem(productItem);
          if (StringUtils.isEmpty(productItemImage.getStoreId())) {
            productItemImage.setStoreId(storeId);
          }
          productItemImageList.add(productItemImage);
        }
        productItem.setProductItemImages(productItemImageList);
      }
      productItem.setProduct(oldProduct);
      oldProduct.getProductItems().add(productItem);
    }
  }

  @Transactional(readOnly = false)
  private Pair<Product, Map<String, Map<String, String>>> updateImagesAndFetchUpdatedProduct(
    EditProductDetailRequest editProductDetailRequest, String storeId,
    ProductDTO updatedOldProduct) throws Exception {
    Pair<ProductPublishUpdateDTO, Map<String, Map<String, String>>> errorMapAndUpdatedProductPair =
      Pair.of(null, new HashMap<>());
    if(CollectionUtils.isNotEmpty(editProductDetailRequest.getProductImageEditRequests())) {
      log.info("Proceeding with Image for product : {} , with Image request : {} ",
        updatedOldProduct.getProductCode(), editProductDetailRequest.getProductImageEditRequests());
      try {
        errorMapAndUpdatedProductPair = updateImages(storeId, false, editProductDetailRequest.getProductImageEditRequests(),
          updatedOldProduct, true);
      }
      catch (Exception e){
        log.error("Error While processing Image Update for product : {} " ,
          updatedOldProduct.getProductCode(), e);
        throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          String.format(ErrorMessage.IMAGE_UPDATE_FAILED_FOR_PDP_EDIT_REQUEST.getMessage(),
            updatedOldProduct.getProductCode()));
      }
    }
    Optional<ProductDTO> productDTOOptional =
      Optional.ofNullable(errorMapAndUpdatedProductPair).map(Pair::getKey)
        .map(ProductPublishUpdateDTO::getProductDTO);
    productDTOOptional.ifPresent(
      productDTO -> log.info("Updated Product and Product Item Images for product : {} ",
        productDTO));
    ProductDTO finalProductDTO = productDTOOptional.orElse(updatedOldProduct);
    return Pair.of(ConverterUtil.convertProductDTOToProduct(finalProductDTO),
      errorMapAndUpdatedProductPair.getRight());
  }

  @Transactional(readOnly = false)
  @Override
  public ProductDetailEditDTO updateProductContentAndImages(String storeId, Product newProduct,
    EditProductDetailRequest editProductDetailRequest, List<NewlySavedItemResponse> newlySavedItemResponseList) throws Exception {
    Product oldProduct = getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId,
      newProduct.getProductCode());
    setCompleteProductDetailsCached(storeId, oldProduct, false);
    boolean productDetailChanged = ProductUtil.isProductDetailChanged(newProduct, oldProduct);
    Set<String> productItemUpcCodeUpdateCodes =
      Optional.ofNullable(editProductDetailRequest.getProductItemUpcCodeUpdateRequestList())
        .orElse(Collections.emptyList()).stream().filter(Objects::nonNull)
        .map(ProductItemUpcCodeUpdateRequest::getSkuCode).collect(Collectors.toSet());

    ProductAndItemLevelUpdatesDTO productAndItemLevelUpdatesDTO =
      CommonUtil.productAndItemLevelUpdates(newProduct, oldProduct);
    productAndItemLevelUpdatesDTO.getUpdatedItemSkuCodes().addAll(productItemUpcCodeUpdateCodes);

    BeanUtils.copyProperties(newProduct, oldProduct, "createdMerchant", "productAttributes",
      "productItems", "productCategories", "productImages", "version", "createdBy", "createdDate");
    checkBrandChanges(oldProduct);
    log.info("proceeding with content and Images update for product : {} with request : {} ",
      newProduct.getProductCode(), newProduct);

    Pair<Map<String, ProductDTO>, Map<ProductItem, String>> productAndProductItemMap =
      adjustProductItem(storeId, oldProduct, newProduct, false, false, true);

    if(processVariantImageNewlyAddedItems){
      productAndProductItemMap =
        processVariantImagesForNewlyAddedVariants(productAndProductItemMap, newProduct,
          editProductDetailRequest.getProductImageEditRequests(), storeId);
    }

    Pair<Product, Map<String, Map<String, String>>> productImageMapPair =
      updateImagesAndFetchUpdatedProduct(editProductDetailRequest, storeId, getProductDTOForEdit(editProductDetailRequest, productAndProductItemMap));


    Product finalProduct = productImageMapPair.getKey();
    if(processVariantImageNewlyAddedItems){
      setMFDForNewlyAddedVariantsItemImage(finalProduct, newProduct);
    }
    setProductForNewlyAddedItems(finalProduct);
    log.info("Saving Final Product For PDP Edit for productCode : {}, product :{} ",
      finalProduct.getProductCode(), finalProduct);
    setBrandInProductItemAttributesValue(finalProduct);
    setProductAndProductItemInFinalProductItemImages(finalProduct);
    ProductImageUtil.resetProductImages(finalProduct, regenerateProductImages);

    log.info("Regenerated product and item images for product : {} , ProductImage : {} and Item Image : {} ", finalProduct.getProductCode(), finalProduct.getProductImages(),
      finalProduct.getProductItems().stream().map(ProductItem::getProductItemImages).flatMap(List::stream).collect(Collectors.toList()));

    Product savedProduct = repository.saveAndFlush(finalProduct);
    Map<String, String> itemIdAndCodeMap =
      savedProduct.getProductItems().stream().collect(Collectors.toMap(ProductItem::getSkuCode,
        ProductItem::getId));
    setProductItemIdForNewlyAddedItem(newlySavedItemResponseList, itemIdAndCodeMap);
    log.info("New variants added for product : {} are : {} " , finalProduct.getProductCode(), newlySavedItemResponseList);
    return new ProductDetailEditDTO(productImageMapPair, productAndProductItemMap, finalProduct,
      productDetailChanged, productAndItemLevelUpdatesDTO);
  }

  private Pair<Map<String, ProductDTO>, Map<ProductItem, String>> processVariantImagesForNewlyAddedVariants(
    Pair<Map<String, ProductDTO>, Map<ProductItem, String>> productAndProductItemMap, Product newProduct,
    List<ProductImageEditRequest> productImageEditRequests, String storeId) {
    ProductDTO updatedOldProduct = productAndProductItemMap.getKey().get(Constants.OLD_PRODUCT);
    List<ProductItem> productItems =
      updatedOldProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).filter(productItem -> StringUtils.isEmpty(productItem.getId())).collect(
        Collectors.toList());

    Map<String, ProductImage> commonProductImagesMap = updatedOldProduct.getProductImages().stream()
      .filter(ProductImage::isCommonImage)
      .filter(Predicate.not(ProductImage::isMarkForDelete))
      .collect(Collectors.toMap(ProductImage::getLocationPath, Function.identity(), (a,b) -> a));

    Map<String, ProductImageEditRequest> productImageEditRequestMap =
      Optional.ofNullable(productImageEditRequests).orElse(Collections.emptyList()).stream()
        .collect(Collectors.toMap(ProductImageEditRequest::getImagePath, Function.identity(), (a,b) -> a));
    productItems.stream().map(ProductItem::getProductItemImages).flatMap(List::stream).forEach(productItemImage -> {
      if(productImageEditRequestMap.containsKey(productItemImage.getLocationPath())){
        ProductImageEditRequest productImageEditRequest =
          productImageEditRequestMap.get(productItemImage.getLocationPath());
        productItemImage.setMarkForDelete(productImageEditRequest.isMarkForDelete());
        Optional.ofNullable(productImageEditRequest.getCopyToAllVariantImages()).ifPresent(image -> {
          productItemImage.setMainImages(image.isMainImage());
        });
      }
    });

    if(CollectionUtils.isNotEmpty(productItems)) {
      Map<String, ProductItem> productItemMap =
        Optional.of(productItems).orElse(Collections.emptyList()).stream().collect(
          Collectors.toMap(ProductItem::getGeneratedItemName, Function.identity(), (a, b) -> b));
      Map<String, List<ProductItemImage>> newlyAddedProductItemImages =
        newProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).collect(
          Collectors.toMap(ProductItem::getGeneratedItemName, ProductItem::getProductItemImages));


      productItemMap.forEach((generatedItemName, productItem) -> {
        resetImagesForNewlyAddedVariants(productImageEditRequests, storeId, generatedItemName, productItem,
          newlyAddedProductItemImages, commonProductImagesMap);
      });

      // Update the product items in updatedOldProduct
      List<ProductItem> updatedProductItems = new ArrayList<>(updatedOldProduct.getProductItems());
      updatedProductItems.replaceAll(productItem -> {
        String generatedItemName = productItem.getGeneratedItemName();
        return productItemMap.getOrDefault(generatedItemName, productItem);
      });
      updatedOldProduct.setProductItems(updatedProductItems);
      Map<String, ProductDTO> updatedMap = new HashMap<>(productAndProductItemMap.getKey());
      updatedMap.put(Constants.OLD_PRODUCT, updatedOldProduct);
      return Pair.of(updatedMap, productAndProductItemMap.getValue());
    }
    return productAndProductItemMap;
  }

  private void resetImagesForNewlyAddedVariants(List<ProductImageEditRequest> productImageEditRequests, String storeId,
    String generatedItemName, ProductItem productItem,
    Map<String, List<ProductItemImage>> newlyAddedProductItemImages,
    Map<String, ProductImage> commonProductImagesMap) {
    List<ProductItemImage> newProductItemImages = newlyAddedProductItemImages.get(generatedItemName);
    Optional.ofNullable(newProductItemImages).orElse(Collections.emptyList()).removeIf(GdnBaseEntity::isMarkForDelete);
    if (newProductItemImages != null) {
      productItem.setProductItemImages(newProductItemImages);
    }
    // handle regeneration of new variant for single variants
    if (CollectionUtils.isEmpty(productItem.getProductItemImages())) {
      productItem.setProductItemImages(productImageEditRequests.stream().filter(request -> Objects.nonNull(request.getCopyToAllVariantImages()))
        .filter(request -> !request.getCopyToAllVariantImages().isMarkForDelete()).filter(request -> request.getCopyToAllVariantImages().isMainImage())
        .map(request -> mapToProductItemImage(request, productItem, storeId)).collect(Collectors.toList()));
     }
    List<String> missingLocationPaths = new ArrayList<>();
    for (String locationPath : commonProductImagesMap.keySet()) {
      if (productItem.getProductItemImages().stream()
        .noneMatch(itemImage -> locationPath.equals(itemImage.getLocationPath()))) {
        missingLocationPaths.add(locationPath);
      }
    }

    // Populate missing location paths for common product image in productItem
    List<ProductItemImage> missingLocationPathsImages = missingLocationPaths.stream()
      .map(commonProductImagesMap::get)
      .map(this::mapCommonImageToProductItemImage)
      .collect(Collectors.toList());
    productItem.getProductItemImages().addAll(missingLocationPathsImages);
  }

  private ProductItemImage mapCommonImageToProductItemImage(ProductImage productImage) {
    ProductItemImage productItemImage = new ProductItemImage();
    BeanUtils.copyProperties(productImage, productItemImage, "id");
    return productItemImage;

  }

  private ProductItemImage mapToProductItemImage(ProductImageEditRequest request,
    ProductItem productItem, String storeId) {
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setMainImages(true);
    productItemImage.setLocationPath(request.getImagePath());
    productItemImage.setMarkForDelete(false);
    productItemImage.setEdited(request.getCopyToAllVariantImages().isAdd());
    productItemImage.setProductItem(productItem);
    productItemImage.setProductItemId(productItem.getId());
    productItemImage.setStoreId(storeId);
    productItemImage.setActive(request.getCopyToAllVariantImages().isAdd());
    productItemImage.setSequence(0);
    productItemImage.setOriginalImage(request.getCopyToAllVariantImages().isAdd());
    productItemImage.setHashCode(request.getCopyToAllVariantImages().getHashCode());
    return productItemImage;
  }

  private static void setMFDForNewlyAddedVariantsItemImage(Product finalProduct, Product newProduct) {
    List<ProductItem> newlyAddedItems = newProduct.getProductItems().stream()
      .filter(ProductItem::isNewlyAddedItem)
      .filter(productItem -> StringUtils.isEmpty(productItem.getId()))
      .collect(Collectors.toList());

    // map of item name to images for newly added items (except common images)
    Map<String, List<ProductItemImage>> newlyAddedItemNameAndItemImagesMap =
      newlyAddedItems.stream().collect(Collectors.toMap(
        ProductItem::getGeneratedItemName,
        productItem -> productItem.getProductItemImages().stream()
          .filter(Predicate.not(ProductItemImage::isCommonImage))
          .collect(Collectors.toList())
      ));

    // Update images for newly added items in finalProduct

    String mainImagePath = null;
    for (ProductItem productItem : finalProduct.getProductItems()) {
      if (StringUtils.isEmpty(productItem.getId()) && productItem.isNewlyAddedItem()) {
        List<ProductItemImage> productItemImages = newlyAddedItemNameAndItemImagesMap.get(productItem.getGeneratedItemName());

        if (Optional.ofNullable(productItemImages).orElse(Collections.emptyList()).stream()
          .anyMatch(ProductItemImage::isMainImages)) {
          mainImagePath = productItemImages.stream().filter(ProductItemImage::isMainImages)
            .map(ProductItemImage::getLocationPath).findFirst().orElse(null);
        }

        if (Objects.nonNull(productItemImages)) {
          Set<String> existingLocationPaths =
            productItem.getProductItemImages().stream().map(ProductItemImage::getLocationPath)
              .collect(Collectors.toSet());

          List<ProductItemImage> uniqueProductItemImages = productItemImages.stream()
            .filter(image -> !existingLocationPaths.contains(image.getLocationPath())).filter(Predicate.not(ProductItemImage::isMarkForDelete))
            .collect(Collectors.toList());

          productItem.getProductItemImages().addAll(uniqueProductItemImages);

          if (Objects.nonNull(mainImagePath)) {
            String finalMainImagePath = mainImagePath;
            productItem.getProductItemImages().forEach(productItemImage -> {
              if (!productItemImage.getLocationPath().equals(finalMainImagePath)) {
                productItemImage.setMainImages(false);
              }
            });
          }
        }
      }
    }

  }



  private static void setBrandInProductItemAttributesValue(Product finalProduct) {
    Optional<ProductItemAttributeValue> brandItemAttributeValue =
      finalProduct.getProductItems().stream()
        .flatMap(productItem -> productItem.getProductItemAttributeValues().stream())
        .filter(attributeValue -> {
          Attribute attribute = attributeValue.getAttribute();
          return Objects.nonNull(attribute) && Constants.BRAND.equals(attribute.getName());
        }).findFirst();

    log.info("Setting Brand Attribute value in Newly Added variants for product : {} with value : {} ",
      finalProduct.getProductCode(), brandItemAttributeValue);

    brandItemAttributeValue.ifPresent(productItemAttributeValue ->
      finalProduct.getProductItems().stream().filter(ProductItem::isNewlyAddedItem).
        forEach(productItem -> productItem.getProductItemAttributeValues().add(productItemAttributeValue)));

    finalProduct.getProductAttributes().stream().map(ProductAttribute::getProductAttributeValues)
      .flatMap(List::stream).forEach(
        productAttributeValue -> productAttributeValue.getProductAttribute()
          .setProductId(finalProduct.getId()));
  }

  public void setProductItemIdForNewlyAddedItem(List<NewlySavedItemResponse> newlySavedItemResponseList,
    Map<String, String> itemIdAndCodeMap) {
    if (CollectionUtils.isNotEmpty(newlySavedItemResponseList)) {
      for (NewlySavedItemResponse newlySavedItemResponse : newlySavedItemResponseList) {
        newlySavedItemResponse.setProductItemId(itemIdAndCodeMap.get(newlySavedItemResponse.getItemCode()));
      }
    }
  }

  private ProductDTO getProductDTOForEdit(EditProductDetailRequest editProductDetailRequest,
    Pair<Map<String, ProductDTO>, Map<ProductItem, String>> productAndProductItemMap) {
    ProductDTO updatedOldProduct = productAndProductItemMap.getKey().get(Constants.OLD_PRODUCT);
    List<ProductItem> productItems = updatedOldProduct.getProductItems();
    Map<String, ProductItem> productItemMap =
      Optional.of(productItems).orElse(Collections.emptyList()).stream()
        .collect(Collectors.toMap(ProductItem::getSkuCode, Function.identity(), (a, b) -> b));
    if (
      CollectionUtils.isNotEmpty(editProductDetailRequest.getProductItemUpcCodeUpdateRequestList())
        && MapUtils.isNotEmpty(productItemMap)) {
      log.info("Proceeding with UPC code update request for Product : {} with request : {} ",
        updatedOldProduct.getProductCode(), editProductDetailRequest.getProductItemUpcCodeUpdateRequestList());
      updateUpcCodeForProductItems(
        editProductDetailRequest.getProductItemUpcCodeUpdateRequestList(), productItemMap);
    }
    updatedOldProduct.setProductItems(new ArrayList<>(productItemMap.values()));
    return updatedOldProduct;
  }

  private void updateUpcCodeForProductItems(
    List<ProductItemUpcCodeUpdateRequest> productItemUpcCodeUpdateRequestList,
    Map<String, ProductItem> productItems) {
    for (ProductItemUpcCodeUpdateRequest request : productItemUpcCodeUpdateRequestList) {
      String skuCode = request.getSkuCode();
      if (productItems.containsKey(skuCode)) {
        ProductItem productItem = productItems.get(skuCode);
        productItem.setUpcCode(request.getUpcCode());
        productItems.put(skuCode, productItem);
      }
    }
  }

  @Override
  public void evictCacheAndPublishForProductItemUpdate(String storeId, Product oldProduct, Product newProduct,
      Boolean pristineCategory, boolean onlyVatChanged, boolean scoreUpdated,
      Map<ProductItem, String> itemWithOldVatValueMap, boolean ignoreSalesCategoryPublish,
      ProductAndItemLevelUpdatesDTO productAndItemLevelUpdatesDTO, Set<String> updatedFields, Set<String> deletedItems) throws Exception {
    ProductSalesCategoryMapping salesCategoryReferenceByMasterCategory =
        getProductSalesCategoryMappingChanges(newProduct, false);
    evictAllProductDetailCache(oldProduct.getStoreId(), oldProduct);
    if (MapUtils.isNotEmpty(itemWithOldVatValueMap)) {
      for (Map.Entry<ProductItem, String> entry : itemWithOldVatValueMap.entrySet()) {
          domainEventPublisherService.publishVatApplicableUpdateEvent(entry.getKey().getSkuCode(),
            entry.getKey().getVatApplicable());
          domainEventPublisherService.publishVatApplicableExternalHistoryEvent(
              GdnMandatoryParameterUtil.getRequestId(), storeId, entry.getKey().getId(),
            entry.getKey().getSkuCode(), entry.getKey().getGeneratedItemName(),
              GdnMandatoryParameterUtil.getUsername(), entry.getValue(),
            String.valueOf(entry.getKey().getVatApplicable()));
      }
    }
    if (!onlyVatChanged) {
      domainEventPublisherService.publishProduct(oldProduct, salesCategoryReferenceByMasterCategory, false, scoreUpdated, true,
            pristineCategory, ignoreSalesCategoryPublish, productAndItemLevelUpdatesDTO, updatedFields, deletedItems);
    }
  }

  private void saveProductImageCleanup(String productCode, Set<String> locationPaths, String storeId) {
    List<String> existingPaths = productImageCleanupRepository.findLocationPathByProductCode(productCode);
    if (CollectionUtils.isNotEmpty(existingPaths)) {
      locationPaths.removeAll(existingPaths);
    }
    if (CollectionUtils.isNotEmpty(locationPaths)) {
      List<ProductImageCleanup> productImageCleanups = new ArrayList<>();
      for (String locationPath : locationPaths) {
        log.debug("Creating ProductImageCleanup product code : {}, location path : {}", productCode, locationPath);
        ProductImageCleanup productImageCleanup = new ProductImageCleanup();
        productImageCleanup.setStoreId(storeId);
        productImageCleanup.setLocationPath(locationPath);
        productImageCleanup.setProductCode(productCode);
        productImageCleanup.setMarkForDelete(false);
        productImageCleanups.add(productImageCleanup);
      }
      if (CollectionUtils.isNotEmpty(productImageCleanups)) {
        productImageCleanupRepository.saveAll(productImageCleanups);
      }
    }
  }

  private ProductSalesCategoryMapping getProductSalesCategoryMappingChanges(Product newProduct, boolean ignoreHalalCategories) {
    String oldMasterCategoryId = StringUtils.EMPTY;
    String newMasterCategoryId = StringUtils.EMPTY;
    if (newProduct.getProductCategories().size() > 1 && !StringUtils.equals(newProduct.getProductCategories().get(0).getCategory().getId(),newProduct.getProductCategories().get(1).getCategory().getId())){
      for (ProductCategory newProductCategory : newProduct.getProductCategories()) {
        if (newProductCategory.isMarkForDelete()) {
          oldMasterCategoryId = newProductCategory.getCategory().getId();
        } else {
          newMasterCategoryId = newProductCategory.getCategory().getId();
        }
      }
    }
    return categoryReferenceService
        .getSalesCategoryReferenceByMasterCategory(oldMasterCategoryId, newMasterCategoryId, ignoreHalalCategories);
  }

  private void updateAttributeMapForNewProductItem(
      String storeId, ProductItem newProductItem, Map<String, Attribute> attributeMap) {
    newProductItem.getProductItemAttributeValues()
        .stream()
        .filter(productItemAttributeValue -> !attributeMap.containsKey(productItemAttributeValue.getAttribute().getId())
            && Objects.isNull(productItemAttributeValue.getId()))
        .map(productItemAttributeValue -> productItemAttributeValue.getAttribute().getId())
        .map(attributeId -> attributeService.getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(storeId, attributeId))
        .forEach(attribute -> attributeMap.put(attribute.getId(), attribute));
  }

  private void updateAttributeMap(String storeId, Product newProduct, Map<String, Attribute> attributeMap) {
    newProduct.getProductAttributes()
        .stream()
        .filter(productAttribute -> !attributeMap.containsKey(productAttribute.getAttribute().getId())
            && Objects.isNull(productAttribute.getId()))
        .map(productAttribute -> productAttribute.getAttribute().getId())
        .map(attributeId -> attributeService.getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(storeId, attributeId))
        .forEach(attribute -> attributeMap.put(attribute.getId(), attribute));
  }

  @Override
  public Long countByProductCode(String storeId, String productCode) {
    return this.repository.countByStoreIdAndProductCode(storeId, productCode);
  }

  @Override
  @Transactional(readOnly = false, propagation = Propagation.REQUIRES_NEW)
  public void deactivateProduct(String storeId, String id) throws Exception {
    Product product = getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(storeId, id);
    productItemServiceWrapper.setProductItemsCached(storeId, product, false);
    for (ProductItem item : product.getProductItems()) {
      item.setActivated(false);
    }
    this.activatedDeactivatedProduct(product, false);
  }

  @Override
  @Transactional(readOnly = false)
  public void delete(String id) throws Exception {
    // TODO Auto-generated method stub
  }

  @Override
  @Transactional(readOnly = false)
  public void deleteProductItemsByProductAttributeValues(String storeId,
      List<ProductAttributeValue> productAttributeValues) throws Exception {
    Set<Product> products = new HashSet<>();
    for (ProductAttributeValue productAttributeValue : productAttributeValues) {
      Hibernate.initialize(productAttributeValue.getProductAttribute());
      Product product = getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(storeId,
              productAttributeValue.getProductAttribute().getProductId());
      setCompleteProductDetailsCached(storeId, product, false);
      products.add(product);
    }

    for (Product product : products) {
      for (ProductAttributeValue productAttributeValue : productAttributeValues) {
        for (ProductAttribute productAttribute : product.getProductAttributes()) {
          for (ProductAttributeValue productAttributeValue2 : productAttribute.getProductAttributeValues()) {
            if (productAttributeValue2.getId().equals(productAttributeValue.getId())) {
              productAttributeValue2.setMarkForDelete(true);
            }
          }
        }
      }
      for (ProductItem item : this.getDeletedProductItem(product)) {
        item.setMarkForDelete(true);
      }
      this.update(product);
      evictAllProductDetailCache(storeId, product);
      this.domainEventPublisherService.publishProduct(product);
    }
  }

  @Override
  public Page<Product> findByBrandLike(String storeId, String brandName, Pageable pageable) {
    return this.repository.findByStoreIdAndBrandStartingWithAndMarkForDeleteFalse(storeId, brandName, pageable);
  }

  @Override
  public Page<ProductCodeResponse> findByStoreIdAndBrandName(String storeId, String brandName,
      Pageable pageable) {
    return this.repository.findByStoreIdAndBrandIgnoreCaseAndMarkForDeleteFalse(storeId, brandName,
        pageable).map(product -> {
      ProductCodeResponse response = new ProductCodeResponse();
      BeanUtils.copyProperties(product, response);
      return response;
    });
  }

  @Override
  public Page<Product> findByCategoryId(String storeId, String categoryId, Pageable pageable) {
    return this.repository.findByStoreIdAndCategoryIdAndMarkForDeleteFalse(storeId, categoryId, pageable);
  }

  @Override
  public Product findById(String id) throws Exception {
    return this.repository.findById(id).orElse(null);
  }

  @Override
  public Page<Product> findByMarkForDelete(String storeId, boolean markForDelete, Pageable pageable) {
    return this.repository.findByStoreIdAndMarkForDeleteOrderByUpdatedDateDesc(storeId, markForDelete, pageable);
  }

  @Override
  public Page<Product> findByName(String storeId, String name, Pageable pageable) {
    return this.repository.findByStoreIdAndNameContainingIgnoreCaseAndMarkForDeleteFalse(storeId, name, pageable);
  }

  @Override
  public Page<Product> findByNameAndCreatedBy(String storeId, String name, String createdBy, Pageable pageable) {
    return this.repository
        .findByStoreIdAndNameContainingIgnoreCaseAndCreatedByContainingIgnoreCaseAndMarkForDeleteFalseOrderByUpdatedDateDesc(
            storeId, name, createdBy, pageable);
  }

  @Override
  public Page<Product> findByNameAndViewableAndActivated(String storeId, String name, boolean viewable,
      boolean activated, Pageable pageable) {
    return this.repository
        .findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndNameContainingIgnoreCaseOrderByUpdatedDateDesc(
            storeId, viewable, activated, name, pageable);
  }

  @Override
  public Page<Product> findByNameAndViewableAndActivatedAndUpdatedBy(String storeId, String name, boolean viewable,
      boolean activated, String updatedBy, Pageable pageable) {
    return this.repository
        .findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndNameContainingIgnoreCaseAndUpdatedByContainingIgnoreCaseOrderByUpdatedDateDesc(
            storeId, viewable, activated, name, updatedBy, pageable);
  }

  @Override
  public Page<Product> findByProductCode(String storeId, String productCode, Pageable pageable) {
    return this.repository.findByStoreIdAndProductCodeContainingIgnoreCaseAndMarkForDeleteFalse(storeId, productCode,
        pageable);
  }

  @Override
  public Page<Product> findByShippingWeightBiggerOrEqualThan(String storeId, Double shippingWeight, Pageable pageable) {
    return this.repository.findByStoreIdAndShippingWeightGreaterThanEqualAndMarkForDeleteFalse(storeId, shippingWeight,
        pageable);
  }

  @Override
  public Page<Product> findByShippingWeightLesserOrEqualThan(String storeId, Double shippingWeight, Pageable pageable) {
    return this.repository.findByStoreIdAndShippingWeightLessThanEqualAndMarkForDeleteFalse(storeId, shippingWeight,
        pageable);
  }

  @Override
  public Page<Product> findByStoreId(String storeId, Pageable pageable) throws Exception {
    return this.repository.findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDateDesc(storeId, pageable);
  }

  @Override
  public Page<Product> findByStoreIdAndCategoriesCode(String storeId, List<String> categoriesCode, Pageable pageable) {
    return this.productRepositoryCustom.findByStoreIdAndCategoriesCode(storeId, categoriesCode, pageable);
  }

  @Override
  public Product getProductDetailsWithoutImagesByProductCodeAndMarkForDeleteFalse(String storeId, String id) throws Exception {
    Product product = getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(storeId, id);
    GdnPreconditions.checkArgument(Objects.nonNull(product),
        ErrorMessage.PRODUCT_NOT_FOUND.getMessage() + id);
    List<ProductItem> productItems =
        productItemService.getProductItemsByStoreIdAndProductIdCached(storeId, product.getId());
    setProductCategoriesWithCategoriesCached(storeId, product, false);
    setProductAttributesWithValuesAndAttributeCached(storeId, product, false);
    product.setProductItems(productItems);
    return product;
  }

  @Override
  public Page<Product> findByStoreIdInitProductCategories(String storeId, Pageable pageable) {
    Page<Product> products =
        this.repository.findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDateDesc(storeId, pageable);
    for (Product product : products.getContent()) {
      GdnPreconditions.checkArgument(product != null, "not found product");
      setProductCategoriesWithCategoriesCached(storeId, product, true);
    }
    return products;
  }

  @Override
  public Page<Product> findByUniqueSellingCodeLike(String storeId, String uniqueSellingCode, Pageable pageable) {
    return this.repository.findByStoreIdAndUniqueSellingPointStartingWithAndMarkForDeleteFalse(storeId,
        uniqueSellingCode, pageable);
  }

  @Override
  public Page<Product> findByViewable(String storeId, boolean viewable, Pageable pageable) {
    return this.repository.findByStoreIdAndViewableAndMarkForDeleteFalse(storeId, viewable, pageable);
  }

  @Override
  public Page<Product> findByViewableAndActivated(String storeId, boolean viewable, boolean activated, Pageable pageable) {
    return this.repository.findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseOrderByUpdatedDateDesc(storeId,
        viewable, activated, pageable);
  }

  @Override
  public Page<Product> findByViewableAndActivatedWithItemsInitialized(String storeId, boolean viewable,
      boolean activated, Pageable pageable) {
    Page<Product> products = this.repository
        .findByStoreIdAndViewableAndActivatedAndMarkForDeleteFalseAndReviewPendingFalseOrderByUpdatedDateDesc(storeId,
        viewable, activated, pageable);
    for (Product product : products){
      productItemServiceWrapper.setProductItemsWithProductItemImagesCached(storeId, product, false);
      setProductCategoriesWithCategoriesCached(storeId, product, false);
      for (ProductItem productItem : product.getProductItems()) {
        productItem.setProductItemImages(productItem.getProductItemImages().stream()
            .filter(productItemImage -> !(Optional.ofNullable(productItemImage.getOriginalImage()).orElse(false)))
            .collect(Collectors.toList()));
      }
    }
    return products;
  }

  @Override
  public Page<Product> findByCategoryIdWithItemsInitialized(String storeId, String categoryId, Pageable pageable) {
    Page<Product> products = this.repository
      .findByStoreIdAndCategoryIdAndMarkForDeleteFalseAndActivatedTrue(storeId, categoryId, pageable);
    for (Product product : products){
      setProductCategoriesWithCategoriesCached(storeId, product, false);
      productItemServiceWrapper.setProductItemsWithProductItemImagesCached(storeId, product, false);
    }
    return products;
  }

  @Override
  public List<CategoryHierarchyResponse> getCategoryHierarchyByUPCCode(String storeId, String upcCode, boolean isOnlyExternal)
      throws Exception {
    Map<String, Long> categoryProductCount =
        productItemService.getCategoryIdsWithProductCountForUPCCode(upcCode, isOnlyExternal);
    Map<String, List<CategoryHierarchyResponse>> categoryHierarchyMap = new LinkedHashMap<>();
    List<CategoryHierarchyResponse> categoryResult = new ArrayList<>();
    if (!CollectionUtils.isEmpty(categoryProductCount.entrySet())) {
      for (String categoryId : categoryProductCount.keySet()) {
        categoryHierarchyMap.put(categoryId, new ArrayList<>());
        List<Category> categories = this.categoryService.findCategoryHierarchyByCategoryId(storeId, categoryId);
        CategoryHierarchyResponse categoryHierarchyResponse = new CategoryHierarchyResponse();
        categoryHierarchyResponse.setCategoryId(categoryId);
        for (Category category : categories) {
          if (category.getId().equals(categoryId)) {
            categoryHierarchyResponse.setCategoryId(category.getId());
          }
        }
        categoryHierarchyResponse.setCategoryHierarchy(populateListResponse(categories));
        categoryHierarchyMap.get(categoryHierarchyResponse.getCategoryId()).add(categoryHierarchyResponse);
      }
      for (List<CategoryHierarchyResponse> categoryHierarchyList : categoryHierarchyMap.values()) {
        categoryResult.addAll(categoryHierarchyList);
      }
    }
    return createCategoryResponse(categoryResult, categoryProductCount);
  }

  private List<CategoryResponse> populateListResponse(List<Category> categories) {
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    for (Category category : categories) {
      CategoryResponse response = new CategoryResponse();
      BeanUtils.copyProperties(category, response);
      CatalogResponse catalog = new CatalogResponse();
      BeanUtils.copyProperties(category.getCatalog(), catalog);
      catalog.setCatalogType(category.getCatalog().getCatalogType().toString());
      response.setCatalog(catalog);
      if (category.getParentCategory() != null) {
        response.setParentCategoryId(category.getParentCategory().getId());
      }
      categoryResponses.add(response);
    }
    return categoryResponses;
  }

  private List<CategoryHierarchyResponse> createCategoryResponse(List<CategoryHierarchyResponse> categoryResult,
      Map<String, Long> productItems) {
    List<CategoryHierarchyResponse> response = new ArrayList<>();
    for (CategoryHierarchyResponse category : categoryResult) {
      CategoryHierarchyResponse categoryResponse = new CategoryHierarchyResponse();
      BeanUtils.copyProperties(category, categoryResponse);
      categoryResponse.setCategoryHierarchy(new ArrayList<>());
      for (CategoryResponse categoryHierarchy : category.getCategoryHierarchy()) {
        CategoryResponse target = new CategoryResponse();
        BeanUtils.copyProperties(categoryHierarchy, target);
        categoryResponse.getCategoryHierarchy().add(target);
      }
      categoryResponse.setProductCount((Long) productItems.get(category.getCategoryId()));
      response.add(categoryResponse);
    }
    return response;
  }

  @Override
  public List<Product> findByWeightBiggerOrEqualThan(String storeId, Double weight) {
    return this.repository.findByStoreIdAndWeightGreaterThanEqualAndMarkForDeleteFalse(storeId, weight);
  }

  @Override
  public Page<Product> findByWeightBiggerOrEqualThan(String storeId, Double weight, Pageable pageable) {
    return this.repository.findByStoreIdAndWeightGreaterThanEqualAndMarkForDeleteFalse(storeId, weight, pageable);
  }

  @Override
  public List<Product> findByWeightLesserOrEqualThan(String storeId, Double weight) {
    return this.repository.findByStoreIdAndWeightLessThanEqualAndMarkForDeleteFalse(storeId, weight);
  }

  @Override
  public Page<Product> findByWeightLesserOrEqualThan(String storeId, Double weight, Pageable pageable) {
    return this.repository.findByStoreIdAndWeightLessThanEqualAndMarkForDeleteFalse(storeId, weight, pageable);
  }

  @Override
  public List<Product> getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(
      String storeId, List<String> productCodes) throws Exception {
    List<Product> productList = new ArrayList<>();
    for (String productCode : productCodes) {
      try {
        Product product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId, productCode);
        setProductCategoriesWithCategoriesCached(storeId, product, false);
        productItemServiceWrapper.setProductItemsCached(storeId, product, false);
        productList.add(product);
      } catch (Exception e) {
        LOG.error("Exception in initializing product productCode: {}", productCode, e);
      }
    }
    return productList;
  }

  @Override
  public List<BasicInfoProductResponse> getProductBasicInfoByProductCodes(String storeId, List<String> productCodes)
      throws Exception {
    List<BasicInfoProductResponse> basicInfoProductResponses = new ArrayList<>();
    ProductUtil.validateProductBasicInfoBatchSize(productCodes, productBasicInfoFetchBatchSize);
    for (String productCode : productCodes) {
      try {
        Product product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId, productCode);
        setProductImagesCached(storeId, product, false);
        List<ProductImage> productImages = product.getProductImages();
        List<Image> imageList =
            productImages.stream().filter(ProductImage::isActive).filter(ProductImage::isCommonImage)
                .filter(Predicate.not(ProductImage::isMarkForDelete))
                .filter(productImage -> ConverterUtil.filterProcessedProductImages(productImage))
                .map(ConverterUtil::convertProductImageToResponse).collect(Collectors.toList());
        BasicInfoProductResponse basicInfoProductResponse =
            ConverterUtil.toBasicInfoProductResponse(imageList, product);
        basicInfoProductResponses.add(basicInfoProductResponse);
      } catch (Exception e) {
        LOG.error("Exception in initializing product productCode: {} ", productCode, e);
      }
    }
    return basicInfoProductResponses;
  }

  private String getAllowedAttributeValueOrDescriptiveValue(ProductAttributeValue productAttributeValue) {
    if (productAttributeValue.getProductAttribute().getAttribute().getAttributeType().equals(AttributeType.DESCRIPTIVE_ATTRIBUTE)) {
      return productAttributeValue.getDescriptiveAttributeValue();
    }
    return productAttributeValue.getAllowedAttributeValue().getValue();
  }

  private String getAllowedAttributeId(ProductAttributeValue productAttributeValue) {
    if (productAttributeValue.getProductAttribute().getAttribute().getAttributeType().equals(AttributeType.DESCRIPTIVE_ATTRIBUTE)) {
      return productAttributeValue.getDescriptiveAttributeValue();
    }
    return productAttributeValue.getAllowedAttributeValue().getId();
  }

  private void generateProductItem(Product product, List<ProductAttribute> productAttributes,
      List<ProductAttribute> descriptiveProductAttributes, int index, int size, String itemName, String result,
      List<ProductItemAttributeValue> productItemAttributeValues) throws Exception {
    if (index >= size) {
      ProductItem productItem =
          new ProductItem(product, null, null, itemName, GdnDigestUtil.getDigestFromString("SHA-256", "UTF-8", result),
              product.getStoreId());

      productItem.setSkuCode(product.getProductCode() + HYPHEN
          + this.productItemService.getSequence(product.getProductCode()));
      for (ProductItemAttributeValue productItemAttributeValue : productItemAttributeValues) {
        ProductItemAttributeValue productItemAttributeValue2 =
            new ProductItemAttributeValue(productItemAttributeValue.getAttribute(),
                productItemAttributeValue.getValue());
        productItemAttributeValue2.setProductItem(productItem);

        if(productItemAttributeValue2.getAttribute().getName().equals("Brand")){
        	productItemAttributeValue2.setValue(product.getBrand());
        }

        if (StringUtils.isEmpty(productItemAttributeValue2.getStoreId())) {
          productItemAttributeValue2.setStoreId(productItem.getStoreId());
        }
          productItem.getProductItemAttributeValues().add(productItemAttributeValue2);
      }
      if(!CollectionUtils.isEmpty(descriptiveProductAttributes)){
        for (ProductAttribute productAttribute : descriptiveProductAttributes) {
          if(!CollectionUtils.isEmpty(productAttribute.getProductAttributeValues())){
            for (ProductAttributeValue productAttributeValue : productAttribute.getProductAttributeValues()) {
              // add MULTIPLE logic here
              ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
              if (productAttributeValue.getDescriptiveAttributeValueType().equals(DescriptiveAttributeValueType.PREDEFINED) && !productAttribute.getAttribute().isSkuValue()) {
                if(Objects.nonNull(productAttributeValue.getPredefinedAllowedAttributeValue())) {
                  productItemAttributeValue.setValue(productAttributeValue.getPredefinedAllowedAttributeValue().getValue());
                }
                if (StringUtils.isEmpty(productItemAttributeValue.getStoreId())) {
                  productItemAttributeValue.setStoreId(product.getStoreId());
                }
                productItemAttributeValue.setAttribute(productAttribute.getAttribute());
                productItemAttributeValue.setProductItem(productItem);
                productItem.getProductItemAttributeValues().add(productItemAttributeValue);
              }
            }
          }
        }
      }
      product.getProductItems().add(productItem);
    } else {
      List<ProductAttributeValue> attrVals = productAttributes.get(index).getProductAttributeValues();
      String attributeId = productAttributes.get(index).getAttribute().getId();
      if(!CollectionUtils.isEmpty(attrVals)){
        for (ProductAttributeValue attrVal : attrVals) {
          if (!attrVal.isMarkForDelete()) {
            productItemAttributeValues.add(new ProductItemAttributeValue(productAttributes.get(index)
                .getAttribute(), getAllowedAttributeValueOrDescriptiveValue(attrVal)));
            this.generateProductItem(product, productAttributes, descriptiveProductAttributes, index + 1, size, itemName
                + SPACE + getAllowedAttributeValueOrDescriptiveValue(attrVal), result + SPACE + attributeId + SPACE
                + getAllowedAttributeId(attrVal), productItemAttributeValues);
            productItemAttributeValues.remove(productItemAttributeValues.size() - 1);
          }
        }
      }
    }
  }

  protected String generateSpecificationDetail(Product entity) throws Exception {
    StringBuilder stringBuilder = new StringBuilder();
    stringBuilder.append("<ul>");
    if(!CollectionUtils.isEmpty(entity.getProductAttributes())){
      for (ProductAttribute productAttribute : entity.getProductAttributes()) {
        Attribute attribute = this.attributeService.findById(productAttribute.getAttribute().getId());
        if (attribute != null && !attribute.isSkuValue()) {
          List<ProductAttributeValue> productAttributeValues = productAttribute.getProductAttributeValues();
          stringBuilder.append("<li>" + attribute.getName() + "<ul>");
          if(!CollectionUtils.isEmpty(productAttributeValues)){
            if(isDefiningAttributeOrVariantCreationTrue(attribute)){
              for (ProductAttributeValue productAttributeValue : productAttributeValues) {
                stringBuilder.append("<li>" + getAllowedAttributeValueOrDescriptiveValue(productAttributeValue) + "</li>");
              }
            } else if (AttributeType.PREDEFINED_ATTRIBUTE.equals(attribute.getAttributeType())) {
              for (ProductAttributeValue productAttributeValue : productAttributeValues) {
                if(Objects.nonNull(productAttributeValue.getPredefinedAllowedAttributeValue())){
                  stringBuilder.append("<li>" + productAttributeValue.getPredefinedAllowedAttributeValue().getValue()
                          + "</li>");
                }
              }
            } else if (AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(attribute.getAttributeType())) {
              for (ProductAttributeValue productAttributeValue : productAttributeValues) {
                stringBuilder.append("<li>" + productAttributeValue.getDescriptiveAttributeValue() + "</li>");
              }
            }
          }
          stringBuilder.append("</ul></li>");
        }
      }
    }
    stringBuilder.append("</ul>");
    return stringBuilder.toString();
  }

  public ApplicationCacheServiceBean getApplicationCacheServiceBean() {
    return this.applicationCacheServiceBean;
  }

  public CategoryService getCategoryService() {
    return this.categoryService;
  }

  protected List<ProductItem> getDeletedProductItem(Product product) throws Exception {
    Product productTemp = new Product();
    BeanUtils.copyProperties(product, productTemp, "productAttributes", "productItems", "productCategories");
    productTemp.setProductAttributes(product.getProductAttributes());
    this.sortAndGenerateProductItem(productTemp);

    List<ProductItem> productItems = new ArrayList<>();
    if (productTemp.getProductItems().size() < product.getProductItems().size()) {
      productItems.addAll(product.getProductItems());
      for (Iterator<ProductItem> iterator = productItems.iterator(); iterator.hasNext();) {
        ProductItem productItem = iterator.next();

        for (ProductItem newProductItem : productTemp.getProductItems()) {
          if (new String(productItem.getHash()).equals(new String(newProductItem.getHash()))) {
            iterator.remove();
            break;
          }
        }
      }
    }
    return productItems;
  }

  public ProductRepository getRepository() {
    return this.repository;
  }
  
  @Override
  @Transactional(readOnly = false)
  public Pair<Product, Set<String>> markForDeleteProduct(String storeId, String productId) throws Exception {
    Product product = this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, productId);
    Set<String> locationPathsForImageDeletion = new HashSet<>();
    if (product == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "Can not perform delete on un exist data : "
          + productId);
    }
    product.setMarkForDelete(true);
    for (ProductItem productItem : product.getProductItems()) {
      productItem.setMarkForDelete(true);
      Hibernate.initialize(productItem.getProductItemImages());
      for(ProductItemImage productItemImage : productItem.getProductItemImages()) {
        if(Boolean.TRUE.equals(productItemImage.getOriginalImage())) {
          locationPathsForImageDeletion.add(productItemImage.getLocationPath());
        }
      }
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      productAttribute.setMarkForDelete(true);
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      productCategory.setMarkForDelete(true);
    }
    this.update(product);
    Hibernate.initialize(product.getProductImages());
    return Pair.of(product, locationPathsForImageDeletion);
  }

  @Override
  @Transactional(readOnly = false)
  public Pair<Map<String, ProductDTO>, Map<ProductItem, String>> regenerateProductItem(String storeId, Product savedProduct, Product product, Boolean pristineCategory,
      boolean onlyVatChanged, boolean scoreUpdated, boolean isProductDetailChanged,
    boolean computecommonImage, boolean resetExtractedAttributeValue,
    boolean combinedUpdateForEditEnabled) throws Exception {
    Pair<Map<String, ProductDTO>, Map<ProductItem, String>> productAndProductItemMap =
      this.adjustProductItem(storeId, savedProduct, product, computecommonImage,
        resetExtractedAttributeValue, combinedUpdateForEditEnabled);
    if (isProductDetailChanged) {
      productAttributeExtractionService.addProductsToProductAttributeExtraction(product.getProductCode(),
          product.getProductCategories().get(0).getCategory());
    }
    return productAndProductItemMap;
  }

  @Override
  public void republishProductByProductCodes(String storeId, List<String> productCodes, String operationType)
      throws Exception {
    for (String productCode : productCodes) {
      if (StringUtils.isNotEmpty(productCode)) {
        Product product = this.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId, productCode);
        setCompleteProductDetailsCached(storeId, product, false);
        if (Constants.MIGRATION.equals(operationType)) {
          domainEventPublisherService.publishProductForMigratedProducts(product, true);
        } else if (ProductPublishEventType.PUBLISH_ITEM_PICKUP_POINT_DATA_CHANGE_EVENT.name().equals(operationType)) {
          domainEventPublisherService.publishProductChangeCategory(product, null, false, true, true, false,
              ImmutableSet.of(ProductPublishEventType.PUBLISH_ITEM_PICKUP_POINT_DATA_CHANGE_EVENT.name()));
        } else {
          domainEventPublisherService.publishProductChangeCategory(product, null, false, true, true, false,
              new HashSet<>());
        }
      }
    }
  }

  @Override
  @Transactional(readOnly = false)
  public String save(Product entity) throws Exception {
    if (StringUtils.isEmpty(entity.getProductCode())) {
      entity.setProductCode("UK-0" + this.categoryService.getSequence("UK"));
    }
    Attribute colourFamily = entity.getProductItems().stream().map(ProductItem::getProductItemAttributeValues)
        .flatMap(List::stream).findFirst().map(ProductItemAttributeValue::getAttribute).orElse(null);
    Map<String, ProductItemAttributeValue> itemNamesWithColourFamily =
        convertProductItemToMapOfItemAndColourFamily(entity);
    Map<String, String> familyColourMap = generateFamilyColourMap(entity, itemNamesWithColourFamily);
    entity.getProductItems().clear();
    entity = this.sortAndGenerateProductItem(entity);
    if (Objects.nonNull(itemNamesWithColourFamily)) {
      updateProductItems(entity, itemNamesWithColourFamily, colourFamily, familyColourMap);
    }
    String productId = ServiceBeanHelper.saveEntity(entity, this.repository);
    publishProduct(entity, true);
    return productId;
  }

  private Map<String, String> generateFamilyColourMap(Product entity,
      Map<String, ProductItemAttributeValue> itemNamesWithColourFamily) {
    Map<String, String> familyColourMap = new HashMap<>();
    for (ProductAttribute productAttribute : entity.getProductAttributes()) {
      if ((WARNA.equalsIgnoreCase(productAttribute.getAttribute().getName()) || COLOR
          .equalsIgnoreCase(productAttribute.getAttribute().getNameEnglish())) && isDefiningAttributeOrVariantCreationTrue(productAttribute.getAttribute())) {
        for (ProductAttributeValue productAttributeValue : productAttribute.getProductAttributeValues()) {
          String warna = getAllowedAttributeValueOrDescriptiveValue(productAttributeValue);
          if (!familyColourMap.containsKey(warna)) {
            String itemName = itemNamesWithColourFamily.keySet().stream()
                .filter(itemGeneratedName -> itemGeneratedName.contains(warna)).findFirst().orElse(StringUtils.EMPTY);
            ProductItemAttributeValue productItemAttributeValue = itemNamesWithColourFamily.get(itemName);
            if (Objects.nonNull(productItemAttributeValue)) {
              familyColourMap.put(warna, productItemAttributeValue.getValue());
            }
          }
        }
      }
    }
    return familyColourMap;
  }

  private Map<String, ProductItemAttributeValue> convertProductItemToMapOfItemAndColourFamily(Product product) {
    if (!CollectionUtils.isEmpty(product.getProductItems())) {
      Map<String, ProductItemAttributeValue> map = new HashMap<>();
      for (ProductItem productItem : product.getProductItems()) {
        if (!CollectionUtils.isEmpty(productItem.getProductItemAttributeValues())) {
          map.put(productItem.getGeneratedItemName(), productItem.getProductItemAttributeValues().get(0));
        }
      }
      return map;
    }
    return null;
  }

  private void updateProductItems(Product entity, Map<String, ProductItemAttributeValue> map, Attribute colourFamily,
      Map<String, String> familyColourMap) {
    for (ProductItem productItem : entity.getProductItems()) {
      ProductItemAttributeValue productItemAttributeValue = map.get(productItem.getGeneratedItemName());
      if (Objects.isNull(productItemAttributeValue)) {
        productItemAttributeValue = new ProductItemAttributeValue();
        productItemAttributeValue.setAttribute(colourFamily);
        String warna = familyColourMap.keySet().stream()
            .filter(warnaValue -> productItem.getGeneratedItemName().contains(warnaValue))
            .findFirst().orElse(StringUtils.EMPTY);
        productItemAttributeValue.setValue(familyColourMap.get(warna));
      }
      productItemAttributeValue.setProductItem(productItem);
      productItem.getProductItemAttributeValues().add(productItemAttributeValue);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public Product saveProduct(Product productToBeSaved, List<Image> commonImages, boolean computeCommonImage) throws Exception {
    ProductImageUtil.setCommonImageFlagForProductAndItemImages(productToBeSaved, computeCommonImage);
    ProductImageUtil.checkCommonImageMaxCount(true, false, false, false, false, productToBeSaved, new HashSet<>());
    ProductImageUtil.setProductMainImageFromCommonImagesRequest(productToBeSaved, commonImages, setCommonImageAsMainImage);
    setMissingProductAttributeData(productToBeSaved);
    Product product = repository.save(productToBeSaved);
    productAttributeExtractionService.addProductsToProductAttributeExtraction(product.getProductCode(),
      fetchCategory(product));
    publishProduct(productToBeSaved, true);
    return product;
  }

  private static Category fetchCategory(Product product) {
    return Optional.ofNullable(product).map(Product::getProductCategories)
      .filter(CollectionUtils::isNotEmpty).map(List::getFirst).map(ProductCategory::getCategory)
      .orElse(new Category());
  }

  private void setMissingProductAttributeData(Product productToBeSaved) {
    if (setMissingAttributesInCreationEnabled) {
      try {
        ProductCategory productCategory =
          productToBeSaved.getProductCategories().stream().filter(Objects::nonNull).findFirst()
            .orElse(null);

        if (Objects.nonNull(productCategory)) {
          String categoryId = productCategory.getCategory().getId();
          List<CategoryAttribute> categoryAttributes =
            categoryService.getCategoryAttributesMarkForDeleteFalse(productToBeSaved.getStoreId(),
              categoryId);
          // fetch must show attributes & filter variant creating attributes
          Set<String> mustShownAttributesIdsMappedToCategory =
            getMustShownAttributesIdsMappedToCategory(categoryAttributes);
          List<String> existingProductAttributeIds =
            getExistingProductAttributeIds(productToBeSaved);
          // get list of must show attributes that should be mapped to product but missing in
          // request
          List<String> missingAttributeIds = mustShownAttributesIdsMappedToCategory.stream()
            .filter(attr -> !existingProductAttributeIds.contains(attr)).distinct().toList();

          if (CollectionUtils.isNotEmpty(missingAttributeIds))
            processMissingAttributesForProduct(productToBeSaved, missingAttributeIds);
        }
      } catch (Exception e) {
        log.error("Error while backfilling missing attributes while creating product [{}]: {}",
          productToBeSaved.getProductCode(), e.getMessage(), e);
      }
    }
  }

  private void processMissingAttributesForProduct(Product productToBeSaved, List<String> missingAttributeIds) {
    List<Attribute> missingAttributes =
      attributeService.findByAttributeIds(productToBeSaved.getStoreId(), missingAttributeIds);
    if (CollectionUtils.isEmpty(missingAttributes)) {
      return;
    }
    Set<Attribute> predefinedMissingAttributes = missingAttributes.stream()
      .filter(attr -> AttributeType.PREDEFINED_ATTRIBUTE.equals(attr.getAttributeType()))
      .collect(Collectors.toSet());

    // Fetch pre-defined allowed attribute values
    Map<String, List<PredefinedAllowedAttributeValue>> preDefinedAllowedAttributeIdAndValueMap =
      fetchMissingPreDefinedAllowedAttributeValueMap(predefinedMissingAttributes, productToBeSaved.getStoreId());

    // Set missing product attributes
    List<ProductAttribute> missingProductAttributes =
      CommonUtil.setMissingProductAttributesAndValues(missingAttributes, productToBeSaved, preDefinedAllowedAttributeIdAndValueMap);

    if (CollectionUtils.isNotEmpty(missingProductAttributes)) {
      productToBeSaved.getProductAttributes().addAll(missingProductAttributes);
    }
  }


  private static List<String> getProductAttributeAndIds(Product product) {
    List<ProductAttribute> productAttributes =
      Optional.ofNullable(product.getProductAttributes()).orElse(new ArrayList<>()).stream()
        .filter(Predicate.not(ProductAttribute::isMarkForDelete)).toList();
    return productAttributes.stream().map(ProductAttribute::getAttribute).map(Attribute::getId)
      .toList();
  }

  private Set<String> getMustShownAttributesIdsMappedToCategory(List<CategoryAttribute> categoryAttributes) {
    return categoryAttributes.stream().filter(Predicate.not(CategoryAttribute::isMarkForDelete))
      .map(CategoryAttribute::getAttribute).filter(Attribute::isMustShowOnCustomerSide)
      .filter(Predicate.not(Attribute::isVariantCreation)).map(Attribute::getId)
      .collect(Collectors.toSet());
  }

  private Map<String, List<PredefinedAllowedAttributeValue>> fetchMissingPreDefinedAllowedAttributeValueMap(
    Set<Attribute> predefinedMissingAttributes, String storeId) {
    return predefinedMissingAttributes.stream().filter(Objects::nonNull)
      .filter(attribute -> Objects.nonNull(attribute.getId())).collect(
        Collectors.toMap(Attribute::getId, attr -> Optional.ofNullable(
          predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueOrderByMarkForDelete(
            storeId, attr, Constants.HYPHEN)).orElseGet(Collections::emptyList)));
  }

  @Override
  @Transactional(readOnly = false)
  public String saveProductWithSpecificationDetailGenaratedBySystem(Product entity) throws Exception {
    entity.setSpecificationDetail(this.generateSpecificationDetail(entity));
    return this.save(entity);
  }

  public void setApplicationCacheServiceBean(ApplicationCacheServiceBean applicationCacheServiceBean) {
    this.applicationCacheServiceBean = applicationCacheServiceBean;
  }

  public void setCategoryService(CategoryService categoryService) {
    this.categoryService = categoryService;
  }

  /**
   * Set mark for delete to sent productAttribute and all productAttributeValues that belongs to
   * sent ProductAttribute
   *
   * @param productAttribute to mark for delete
   */
  private void setMarkForDeleteToThisProductAttributeAndProductAttributeValues(ProductAttribute productAttribute) {
    if (avoidBrandDelete && StringUtils.equalsIgnoreCase(brandAttributeId,
      productAttribute.getAttributeId())) {
      LOG.info("Trying to delete brand attribute for {}", productAttribute);
    } else {
      LOG.info("Deleting product attribute: {} ", productAttribute);
      productAttribute.setMarkForDelete(true);
      for (ProductAttributeValue productAttributeValue : productAttribute.getProductAttributeValues()) {
        LOG.info("Deleting product attribute value : {} ", productAttributeValue);
        productAttributeValue.setMarkForDelete(true);
      }
    }
  }

  public void setRepository(ProductRepository repository) {
    this.repository = repository;
  }

  @Override
  @Transactional
  public Product sortAndGenerateProductItem(Product entity) throws Exception {
    validateVariantCreatingProductAttribute(entity);
    try {
      List<ProductAttribute> definingProductAttributes = new ArrayList<>();
      List<ProductAttribute> descriptiveProductAttributes = new ArrayList<>();
      for (ProductAttribute productAttribute : entity.getProductAttributes()) {
        if (isDefiningAttributeOrVariantCreationTrue(productAttribute.getAttribute())) {
          definingProductAttributes.add(productAttribute);
        } else {
          descriptiveProductAttributes.add(productAttribute);
        }
      }
      definingProductAttributes.sort(Comparator.comparing(productAttribute -> productAttribute.getAttribute().getId()));

      for (ProductAttribute productAttribute : definingProductAttributes) {
        List<ProductAttributeValue> productAttributeValues = productAttribute.getProductAttributeValues();
        if(!CollectionUtils.isEmpty(productAttributeValues)){
          productAttributeValues.sort(Comparator.comparing(this::getAllowedAttributeId));
        }
      }
      this.generateProductItem(entity, definingProductAttributes, descriptiveProductAttributes, 0,
          definingProductAttributes.size(), entity.getName(), entity.getProductCode(), new ArrayList<>());
      return entity;
    } catch (Exception e) {
      // Detected cause: there are some defining product attributes that have null
      // allowedAttributeValue, causing a NullPointerException
      LOG.error("Error when invoking sortAndGenerateProductItem\n {}", entity, e);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "Error when generate product items");
    }
  }

  private void validateVariantCreatingProductAttribute(Product product) {
    List<ProductAttribute> productAttributes =
        Optional.ofNullable(product.getProductAttributes()).orElse(new ArrayList<>());
    for (ProductAttribute productAttribute : productAttributes) {
      if (isDefiningAttributeOrVariantCreationTrue(productAttribute.getAttribute())) {
        Set<String> attributeValueSet = new HashSet<>();
        List<ProductAttributeValue> productAttributeValues =
            Optional.ofNullable(productAttribute.getProductAttributeValues()).orElse(new ArrayList<>());
        for (ProductAttributeValue productAttributeValue : productAttributeValues) {
          String value =
              Optional.ofNullable(productAttributeValue.getAllowedAttributeValue()).map(AllowedAttributeValue::getValue)
                  .orElse(productAttributeValue.getDescriptiveAttributeValue());
          if (attributeValueSet.contains(value)) {
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
                String.format(ErrorMessage.DUPLICATE_ATTRIBUTE_VALUE_PRESENT.getMessage(),
                    productAttribute.getAttribute().getName()));
          } else {
            attributeValueSet.add(value);
          }
        }
      }
    }
  }

  @Override
  @Transactional(readOnly = false)
  public void update(Product entity) throws Exception {
    ServiceBeanHelper.updateEntity(entity, this.repository);
  }

  @Override
  public void updateProductWithSpecificationDetailGeneratedBySystem(String storeId, Product savedProduct,
      Product product) throws Exception {
    savedProduct.setSpecificationDetail(this.generateSpecificationDetail(product));
    this.regenerateProductItem(storeId, savedProduct, product, false, false, true, false, false,
      false, false);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public void updateProductViewable(String storeId, String productCode, boolean viewable) throws Exception {
    Product product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId, productCode);
    productItemServiceWrapper.setProductItemsCached(storeId, product, false);
    product.setViewable(viewable);
    for (ProductItem productItem : product.getProductItems()) {
      productItem.setViewable(viewable);
    }
    this.update(product);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public Product updateProductReviewPending(String storeId, String productCode, boolean reviewPending) throws Exception {
    Product product = repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    product.setReviewPending(reviewPending);
    return repository.saveAndFlush(product);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public Product updateFlagsOnNeedCorrection(String storeId, String productCode,
    NeedRevisionConfigRequest request)
      throws Exception {
    Product product = repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    product.setReviewPending(request.isReviewPending());
    product.setRevised(request.isRevised());
    product.setViewable(request.isViewable());
    return repository.saveAndFlush(product);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public Product updateProductActivated(String storeId, String productCode, boolean activated) throws Exception {
    Product product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId, productCode);
    productItemServiceWrapper.setProductItemsCached(storeId, product, false);
    product.setActivated(activated);
    for (ProductItem productItem : product.getProductItems()) {
      productItem.setActivated(activated);
    }
    this.update(product);
    return product;
  }
  

  @Override
  public Page<Product> findByProductCodeExactMatch(String storeId, String productCode, Pageable
      pageable) {
    return this.repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode,
        pageable);
  }

  @Override
  public Long getProductCountForViewable(String storeId, boolean viewable) {
    return this.repository.countByStoreIdAndViewableAndMarkForDeleteFalse(storeId,viewable);
  }

  @Override
  public void regenerateProductAttributeContent(Product savedProduct, Product product,
      boolean updateFromVendor, boolean categoryUpdated) {
    Map<String, ProductAttribute> oldProductAttributeMap =
        savedProduct.getProductAttributes().stream().filter(productAttribute -> !productAttribute.isMarkForDelete())
            .collect(Collectors.toMap(productAttribute -> productAttribute.getAttribute().getAttributeCode(),
                productAttribute -> productAttribute, (productAttribute1, productAttribute2) -> productAttribute1));
    Map<String, ProductAttribute> newProductAttributeMap =
        product.getProductAttributes().stream().filter(productAttribute -> !productAttribute.isMarkForDelete()).collect(
            Collectors.toMap(productAttribute -> productAttribute.getAttribute().getAttributeCode(),
                productAttribute -> productAttribute, (productAttribute1, productAttribute2) -> productAttribute1));

    for (ProductAttribute oldProductAttribute : savedProduct.getProductAttributes()) {
      if (!oldProductAttribute.isMarkForDelete() && newProductAttributeMap
          .containsKey(oldProductAttribute.getAttribute().getAttributeCode())) {
        if (isNotDefiningAttributeAndVariantCreationFalse(oldProductAttribute.getAttribute())) {
          adjustProductAttributeValue(oldProductAttribute.getStoreId(), oldProductAttribute,
              newProductAttributeMap.get(oldProductAttribute.getAttribute().getAttributeCode()));
        }
      } else if (isNotDefiningAttributeAndVariantCreationFalse(oldProductAttribute.getAttribute())) {
        if(updateFromVendor && !oldProductAttribute.isMarkForDelete() && !newProductAttributeMap
            .containsKey(oldProductAttribute.getAttribute().getAttributeCode()) && oldProductAttribute.getAttribute()
            .isDsExtraction() && !categoryUpdated) {
          continue;
        }
        LOG.info("Deleting product attribute: {} ", oldProductAttribute);
        oldProductAttribute.setMarkForDelete(true);
          oldProductAttribute.getProductAttributeValues()
              .forEach(productAttributeValue -> productAttributeValue.setMarkForDelete(true));
      }
    }

    for (ProductAttribute newProductAttribute : product.getProductAttributes()) {
      if (!newProductAttribute.isMarkForDelete() && !oldProductAttributeMap
          .containsKey(newProductAttribute.getAttribute().getAttributeCode())) {
        newProductAttribute.setProduct(product);
        savedProduct.getProductAttributes().add(newProductAttribute);
      }
    }

    if (removeDuplicateAttributes) {
      Set<String> uniqueAttributeIdList = new HashSet<>();
      for (ProductAttribute productAttribute : savedProduct.getProductAttributes()) {
        Optional<Attribute> optionalAttribute =
          Optional.ofNullable(productAttribute.getAttribute());
        optionalAttribute.ifPresent(attribute -> {
          String attributeCode = attribute.getAttributeCode();
          if (uniqueAttributeIdList.contains(attributeCode)
            && !productAttribute.isMarkForDelete()) {
            log.error("Marking duplicate attribute as MFD true : {} , product code {} ",
              productAttribute, product.getProductCode());
            productAttribute.setMarkForDelete(true);
            if (CollectionUtils.isNotEmpty(productAttribute.getProductAttributeValues())) {
              productAttribute.getProductAttributeValues()
                .forEach(productAttributeValue -> productAttributeValue.setMarkForDelete(true));
            }
          } else if (!productAttribute.isMarkForDelete()) {
            uniqueAttributeIdList.add(attributeCode);
          }
        });
      }
    }
  }

  @Override
  public void regenerateProductItemContent(Product savedProduct, Product product) {
    for (ProductItem productItem : product.getProductItems()) {
      for (ProductItem savedProductItem : savedProduct.getProductItems()) {
        if (!savedProductItem.isMarkForDelete()) {
          if (savedProductItem.getSkuCode().equals(productItem.getSkuCode())) {
            regenerateProductItemAttributeValues(savedProductItem, productItem);
            String productItemName = product.getName();
            for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
              for (ProductItemImage savedProductItemImage : savedProductItem.getProductItemImages()) {
                if (!savedProductItemImage.isMarkForDelete()) {
                  if (savedProductItemImage.getId().equals(productItemImage.getId())) {
                    savedProductItemImage.setHashCode(productItemImage.getHashCode());
                  }
                }
              }
            }
            regenerateAndSetProductItemName(savedProductItem, productItemName);
            savedProductItem.setUpcCode(productItem.getUpcCode());
            savedProductItem.setDangerousGoodsLevel(productItem.getDangerousGoodsLevel());
          }
        }
      }
    }
  }

  private void regenerateAndSetProductItemName(ProductItem productItem, String productItemName) {
    String definingAttributesString = productItem.getProductItemAttributeValues().stream()
        .filter(productItemAttributeValue -> !productItemAttributeValue.isMarkForDelete()).filter(
            productItemAttributeValue -> isDefiningAttributeOrVariantCreationTrue(
                productItemAttributeValue.getAttribute()))
        .sorted(Comparator.comparing(productItemAttributeValue -> productItemAttributeValue.getAttribute().getId()))
        .map(ProductItemAttributeValue::getValue).collect(Collectors.joining(StringUtils.SPACE));
    if (StringUtils.isNotEmpty(definingAttributesString)) {
      productItemName += StringUtils.SPACE + definingAttributesString;
    }
    productItem.setGeneratedItemName(productItemName);
  }

  private void regenerateProductItemAttributeValues(ProductItem savedProductItem, ProductItem productItem) {
    if (!CollectionUtils.isEmpty(productItem.getProductItemAttributeValues())) {
      Map<String, ProductItemAttributeValue> oldProductItemAttributeValuesMap = new HashMap<>();
      for (ProductItemAttributeValue itemAttributeValue : savedProductItem.getProductItemAttributeValues()) {
        if (!itemAttributeValue.isMarkForDelete()) {
          if (!oldProductItemAttributeValuesMap.containsKey(itemAttributeValue.getAttribute().getAttributeCode())) {
            oldProductItemAttributeValuesMap
                .put(itemAttributeValue.getAttribute().getAttributeCode(), itemAttributeValue);
          } else {
            itemAttributeValue.setMarkForDelete(true);
          }
        }
      }
      Map<String, ProductItemAttributeValue> newProductItemAttributeValuesMap =
          productItem.getProductItemAttributeValues().stream()
              .filter(productItemAttributeValue -> !productItemAttributeValue.isMarkForDelete()).collect(Collectors
              .toMap(productItemAttributeValue -> productItemAttributeValue.getAttribute().getAttributeCode(),
                  productItemAttributeValue -> productItemAttributeValue, (a, b) -> a));
      for (ProductItemAttributeValue savedProductItemAttributeValue : savedProductItem
          .getProductItemAttributeValues()) {
        if (!savedProductItemAttributeValue.isMarkForDelete()) {
          ProductItemAttributeValue productItemAttributeValue =
              newProductItemAttributeValuesMap.get(savedProductItemAttributeValue.getAttribute().getAttributeCode());
          if (Objects.nonNull(productItemAttributeValue) && StringUtils
              .isNotEmpty(productItemAttributeValue.getValue())) {
            savedProductItemAttributeValue.setValue(productItemAttributeValue.getValue());
          }
          if (AttributeType.PREDEFINED_ATTRIBUTE
              .equals(savedProductItemAttributeValue.getAttribute().getAttributeType()) && Objects
              .isNull(productItemAttributeValue)) {
            savedProductItemAttributeValue.setMarkForDelete(true);
          }
        }
      }
      for (ProductItemAttributeValue newProductItemAttributeValue : productItem.getProductItemAttributeValues()) {
        if (!newProductItemAttributeValue.isMarkForDelete() && !oldProductItemAttributeValuesMap
            .containsKey(newProductItemAttributeValue.getAttribute().getAttributeCode())) {
          if (AttributeType.PREDEFINED_ATTRIBUTE
              .equals(newProductItemAttributeValue.getAttribute().getAttributeType())) {
            newProductItemAttributeValue.setProductItem(savedProductItem);
            savedProductItem.getProductItemAttributeValues().add(newProductItemAttributeValue);
          }
        }
      }
    }
  }

  @Override
  public void regenerateProductImageContent(Product savedProduct, Product product) {
    for(ProductImage productImage: product.getProductImages()) {
      for(ProductImage savedProductImage : savedProduct.getProductImages()) {
        if(!savedProductImage.isMarkForDelete()) {
          if(savedProductImage.getId().equals(productImage.getId())) {
            savedProductImage.setHashCode(productImage.getHashCode());
          }
        }
      }
    }
  }

  @Override
  public void publishProduct(
      Product product, ProductSalesCategoryMapping productSalesCategoryMapping, boolean isBrandChanged) throws Exception {
    this.domainEventPublisherService.publishProductChangeCategory(product, productSalesCategoryMapping, isBrandChanged, false, false,
        false, new HashSet<>());
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateProductContent(Product product, boolean publishProductEvent) throws Exception {
    Product savedProduct = getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
            product.getStoreId(), product.getProductCode());
    LOG.info("#updateProductContent ProductCode : {}, requestVersion: {}, savedVersion: {}",
        product.getProductCode(), product.getVersion(), savedProduct.getVersion());
    if (product.getVersion() < savedProduct.getVersion()) {
      throw new ApplicationException(ErrorCategory.INVALID_STATE, "Product Code : " + product.getProductCode()
          + ". Error Code : Product content expired");
    }
    setCompleteProductDetailsCached(savedProduct.getStoreId(), savedProduct, true);
    boolean productDetailChanged = ProductUtil.isProductDetailChanged(savedProduct, product);
    boolean isBrandChanged = !StringUtils.equals(savedProduct.getBrand(), product.getBrand());
    BeanUtils.copyProperties(product, savedProduct, "createdMerchant", "productAttributes",
        "productCategories", "productImages", "productItems", "version", "createdBy", "createdDate");
    CategoryChangeDTO categoryChangeDTO = regenerateProductCategories(savedProduct, product);
    ProductSalesCategoryMapping salesCategoryReferenceByMasterCategory = null;
    if (Objects.nonNull(categoryChangeDTO) && (Objects.nonNull(categoryChangeDTO.getNewCategoryId())) && (Objects
        .nonNull(categoryChangeDTO.getOldCategoryId()))) {
      salesCategoryReferenceByMasterCategory = categoryReferenceService
          .getSalesCategoryReferenceByMasterCategory(categoryChangeDTO.getOldCategoryId(),
              categoryChangeDTO.getNewCategoryId(), false);
    }
    regenerateProductAttributeContent(savedProduct, product, false, false);
    regenerateProductItemContent(savedProduct, product);
    regenerateProductImageContent(savedProduct, product);
    savedProduct.getProductItems().forEach(productItem -> {
      productItem.setActivated(product.isActivated());
      productItem.setViewable(product.isViewable());
    });
    repository.saveAndFlush(savedProduct);
    if (productDetailChanged) {
      productAttributeExtractionService.addProductsToProductAttributeExtraction(product.getProductCode(),
          product.getProductCategories().get(0).getCategory());
    }
    evictAllProductDetailCache(savedProduct.getStoreId(), savedProduct);
    if (publishProductEvent) {
      publishProduct(savedProduct, salesCategoryReferenceByMasterCategory, isBrandChanged);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public Product saveAndFlush(Product product) {
    return repository.saveAndFlush(product);
  }

  @Override
  public CategoryChangeDTO regenerateProductCategories(Product savedProduct, Product product) {
    CategoryChangeDTO categoryChangeDTO = new CategoryChangeDTO();
    Map<String, ProductCategory> oldProductCategoryMap = savedProduct.getProductCategories().stream().collect(Collectors
        .toMap(productCategory -> productCategory.getCategory().getCategoryCode(), Function.identity()));
    Map<String, ProductCategory> newProductCategoryMap =
        product.getProductCategories().stream().filter(productCategory -> !productCategory.isMarkForDelete())
            .collect(Collectors.toMap(productCategory -> productCategory.getCategory().getCategoryCode(),
                Function.identity()));
    for (ProductCategory oldProductCategory : savedProduct.getProductCategories()) {
      if (!oldProductCategory.isMarkForDelete() && !newProductCategoryMap
          .containsKey(oldProductCategory.getCategory().getCategoryCode())) {
        oldProductCategory.setMarkForDelete(true);
        categoryChangeDTO.setOldCategoryId(oldProductCategory.getCategory().getId());
      } else if (oldProductCategory.isMarkForDelete() && newProductCategoryMap
          .containsKey(oldProductCategory.getCategory().getCategoryCode())) {
        oldProductCategory.setMarkForDelete(false);
        categoryChangeDTO.setNewCategoryId(oldProductCategory.getCategory().getId());
      }
    }

    for (ProductCategory newProductCategory : product.getProductCategories()) {
      if (!newProductCategory.isMarkForDelete() && !oldProductCategoryMap
          .containsKey(newProductCategory.getCategory().getCategoryCode())) {
        newProductCategory.setProduct(product);
        savedProduct.getProductCategories().add(newProductCategory);
        categoryChangeDTO.setNewCategoryId(newProductCategory.getCategory().getId());
      }
    }
    return categoryChangeDTO;
  }

  private boolean regenerateProductImage(Product savedProduct, Product product) throws Exception {
    Map<String, ProductImage> updatedImages = new HashMap<String, ProductImage>();
    List<ProductImage> newImages = new ArrayList<ProductImage>();
    for (ProductImage productImage : product.getProductImages()) {
      if (StringUtils.isEmpty(productImage.getId())) {
        newImages.add(productImage);
      } else {
        updatedImages.put(productImage.getId(), productImage);
      }
    }
    if (savedProduct.isReviewPending()) {
      return filterUpdatedImagesForPostLiveProduct(savedProduct, updatedImages, newImages);
    } else {
      return filterUpdatedImagesForPreLiveProduct(savedProduct, updatedImages, newImages);
    }
  }

  private boolean filterUpdatedImagesForPreLiveProduct(Product savedProduct, Map<String, ProductImage> updatedImages,
      List<ProductImage> newImages) {
    boolean commonImageUpdated = false;
    for (ProductImage savedProductImage : savedProduct.getProductImages()) {
      ProductImage updatedImage = updatedImages.get(savedProductImage.getId());
      if (Objects.isNull(updatedImage)) {
        savedProductImage.setMarkForDelete(true);
        commonImageUpdated = CommonUtil.commonImageUpdateWithProductImage(Arrays.asList(savedProductImage),
            commonImageUpdated);
      } else {
        BeanUtils.copyProperties(updatedImage, savedProductImage);
      }
    }
    savedProduct.getProductImages().addAll(newImages);
    return CommonUtil.commonImageUpdateWithProductImage(newImages, commonImageUpdated);
  }

  private boolean filterUpdatedImagesForPostLiveProduct(Product savedProduct, Map<String, ProductImage> updatedImages,
      List<ProductImage> newImages) {
    boolean commonImageUpdated = false;
    for (ProductImage savedProductImage : savedProduct.getProductImages()) {
      ProductImage updatedImage = updatedImages.get(savedProductImage.getId());
      if (Objects.isNull(updatedImage) && !savedProductImage.getOriginalImage()) {
        savedProductImage.setMarkForDelete(true);
        commonImageUpdated = CommonUtil.commonImageUpdateWithProductImage(Arrays.asList(savedProductImage),
            commonImageUpdated);
      } else {
        if (Objects.nonNull(updatedImage)) {
          BeanUtils.copyProperties(updatedImage, savedProductImage);
        }
      }
    }
    savedProduct.getProductImages().addAll(newImages);
    return CommonUtil.commonImageUpdateWithProductImage(newImages, commonImageUpdated);
  }

  private Set<String> regenerateProductItemImage(Product savedProduct, Product product) throws Exception {
    Set<String> updatedItemSkuCodes = new HashSet<>();
    for (ProductItem productItem : product.getProductItems()) {
      boolean itemLevelImagesUpdated = false;
      for (ProductItem savedProductItem : savedProduct.getProductItems()) {
        if (!savedProductItem.isMarkForDelete()) {
          if (savedProductItem.getSkuCode().equals(productItem.getSkuCode())) {
            Map<String, ProductItemImage> updatedItemImages = new HashMap<String, ProductItemImage>();
            List<ProductItemImage> newItemImages = new ArrayList<ProductItemImage>();
            for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
              if (StringUtils.isEmpty(productItemImage.getId())) {
                newItemImages.add(productItemImage);
              } else {
                updatedItemImages.put(productItemImage.getId(), productItemImage);
              }
            }
            if (product.isReviewPending()) {
              filterUpdatedItemImagesForPostLiveProduct(savedProductItem, updatedItemImages, itemLevelImagesUpdated);
            } else {
              filterUpdatedItemImagesForPreLiveProduct(savedProductItem, updatedItemImages, itemLevelImagesUpdated);
            }
            newItemImages.forEach(productItemImage -> productItemImage.setProductItemId(savedProductItem.getId()));
            savedProductItem.getProductItemImages().addAll(newItemImages);
            itemLevelImagesUpdated = CommonUtil.itemLevelImagesUpdated(newItemImages, itemLevelImagesUpdated);
          }
        }
      }
      if (itemLevelImagesUpdated) {
        updatedItemSkuCodes.add(productItem.getSkuCode());
      }
    }
    return updatedItemSkuCodes;
  }

  private boolean filterUpdatedItemImagesForPreLiveProduct(ProductItem savedProductItem,
      Map<String, ProductItemImage> updatedItemImages, boolean itemLevelImagesUpdated) {
    for (ProductItemImage savedProductItemImage : savedProductItem.getProductItemImages()) {
      ProductItemImage updatedItemImage = updatedItemImages.get(savedProductItemImage.getId());
      if (Objects.isNull(updatedItemImage)) {
        savedProductItemImage.setMarkForDelete(true);
        itemLevelImagesUpdated = CommonUtil.itemLevelImagesUpdated(Arrays.asList(savedProductItemImage),
            itemLevelImagesUpdated);
      } else {
        BeanUtils.copyProperties(updatedItemImage, savedProductItemImage);
      }
    }
    return itemLevelImagesUpdated;
  }

  private boolean filterUpdatedItemImagesForPostLiveProduct(ProductItem savedProductItem,
      Map<String, ProductItemImage> updatedItemImages, boolean itemLevelImagesUpdated) {
    for (ProductItemImage savedProductItemImage : savedProductItem.getProductItemImages()) {
      ProductItemImage updatedItemImage = updatedItemImages.get(savedProductItemImage.getId());
      if (Objects.isNull(updatedItemImage) && !savedProductItemImage.getOriginalImage()) {
        savedProductItemImage.setMarkForDelete(true);
        itemLevelImagesUpdated = CommonUtil.itemLevelImagesUpdated(Arrays.asList(savedProductItemImage),
            itemLevelImagesUpdated);
      } else {
        if (Objects.nonNull(updatedItemImage)) {
          BeanUtils.copyProperties(updatedItemImage, savedProductItemImage);
        }
      }
    }
    return itemLevelImagesUpdated;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public ProductPublishUpdateDTO updateProductImage(Product product) throws Exception {
    Product savedProduct = getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        product.getStoreId(), product.getProductCode());
    LOG.info("#updateProductImage ProductCode : {}, requestVersion: {}, savedVersion: {}",
        product.getProductCode(), product.getVersion(), savedProduct.getVersion());
    if (product.getVersion() < savedProduct.getVersion()) {
      throw new ApplicationException(ErrorCategory.INVALID_STATE, "Product Code : " + product.getProductCode()
          + ". Error Code : Product content expired");
    }
    setProductImagesCached(savedProduct.getStoreId(), savedProduct, true);
    productItemServiceWrapper.setProductItemsWithProductItemImagesCached(savedProduct.getStoreId(), savedProduct, true);
    BeanUtils.copyProperties(product, savedProduct, "createdMerchant", "productAttributes",
        "productCategories", "productImages", "productItems", "version", "createdBy", "createdDate",
        "video", "distributionInfo", "aiGeneratedFields");
    if (StringUtils.isNotBlank(product.getDistributionInfo())) {
      savedProduct.setDistributionInfo(product.getDistributionInfo());
    }
    boolean commonImageUpdate = regenerateProductImage(savedProduct, product);
    Set<String> updatedItemSkuCodes = regenerateProductItemImage(savedProduct, product);
    log.info("updating product images for productCode : {} and productImages : {} ",
        savedProduct.getProductCode(), savedProduct.getProductImages());
    product = repository.saveAndFlush(savedProduct);
    return new ProductPublishUpdateDTO(product, commonImageUpdate, updatedItemSkuCodes);
  }

  @Override
  public Long getProductCountByBrandName(String storeId, String brandName) {
    return this.repository.countByStoreIdAndBrandIgnoreCase(storeId, brandName);
  }

  @Override
  @Transactional
  public GeneratedProductImagesPathDto replaceProductImages(String storeId, ReplaceProductImagesDTO productImagesReq) throws Exception {
    try{
      GeneratedProductImagesPathDto result = new GeneratedProductImagesPathDto();
      Product productData =
          getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId, productImagesReq.getProductCode());
      
      if(productData.isActivated() || productData.isViewable()){
        throw new ApplicationException(ErrorCategory.INVALID_STATE, "Product activated & viewable must be false, current state is activated="
            + productData.isActivated() + " and viewable=" + productData.isViewable());
      }
      setProductImagesCached(storeId, productData, true);
      result.setProductCode(productData.getProductCode());
      result.setImages(new ArrayList<>());
      result.setProductItems(new ArrayList<>());
      
      this.productImageRepository.deleteAll(productData.getProductImages());
      productData.setProductImages(new ArrayList<>());
      int sequence = 1;
      for(int i = sequence; i <= productImagesReq.getGeneratedImageCount(); i++){
        ImagePathDTO imagePath = ProductImageUtil.generateImagePath(productData.getProductCode(), productData.getBrand(), 
            productData.getName(), sequence, i == 1);
        result.getImages().add(imagePath);
        productData.getProductImages().add(new ProductImage(productData, imagePath.isMainImage(), imagePath.getImagePath(), (i - 1), storeId));
        sequence++;
      }
      productItemServiceWrapper.setProductItemsWithProductItemImagesCached(storeId, productData, true);
      this.replaceProductItemImages(productData, productImagesReq, sequence, result); 
      this.repository.save(productData);
      evictProductCache(storeId, productData);
      applicationCacheServiceBean.evictProductImagesCacheByStoreIdAndProductId(storeId, productData.getId());
      evictProductItemsAndProductItemImagesCache(storeId, productData);
      return result;
    } catch(Exception e){
      throw e;
    }
  }
  
  private void replaceProductItemImages(Product productData, ReplaceProductImagesDTO productImagesReq, int sequence, GeneratedProductImagesPathDto result){
    for(ProductItem item : productData.getProductItems()){
      this.productItemImageRepository.deleteAll(item.getProductItemImages());
      
      GeneratedProductItemImagesPathDto generatedItemImages = new GeneratedProductItemImagesPathDto();
      generatedItemImages.setSkuCode(item.getSkuCode());
      generatedItemImages.setImages(new ArrayList<ImagePathDTO>());
      
      ReplaceProductItemImagesDTO replacedItem = this.matchReplacedItemImage(item.getSkuCode(), productImagesReq.getProductItem());
      item.setProductItemImages(new ArrayList<ProductItemImage>());
      if(replacedItem != null){
        for(int i = 1; i <= replacedItem.getGeneratedImageCount(); i++){
          ImagePathDTO imagePath = ProductImageUtil.generateImagePath(productData.getProductCode(), productData.getBrand(), 
              productData.getName(), sequence, i == 1);
          generatedItemImages.getImages().add(imagePath);
          item.getProductItemImages().add(new ProductItemImage(item, imagePath.isMainImage(), imagePath.getImagePath(), (i-1)));
          sequence++;
        }
      } else {
        generatedItemImages.setImages(result.getImages());
        for(ProductImage productImage : productData.getProductImages()){
          item.getProductItemImages().add(new ProductItemImage(item, productImage.isMainImages(), productImage.getLocationPath(), productImage.getSequence()));
        }
      }
      result.getProductItems().add(generatedItemImages);
    }
  }
  
  private ReplaceProductItemImagesDTO matchReplacedItemImage(String skuCode, List<ReplaceProductItemImagesDTO> replacedItemList){
    if(! CollectionUtils.isEmpty(replacedItemList)){
      for(ReplaceProductItemImagesDTO replacedItem : replacedItemList){
        if(replacedItem.getSkuCode().equals(skuCode)){
          return replacedItem;
        }
      }
    }
    return null;
  }

  @Override
  public Boolean validateProductPromoSku(String productId, String storeId, boolean isPromoSku) {
    Product product = getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(storeId, productId);
    return product.isPromoSKU();
  }

  @Override
  public Page<String> getActiveProductCodes(String storeId, Pageable pageable){
    return this.repository.getAllProductCodes(storeId, pageable);
  }

  @Override
  public Page<Product> findProductsByStoreIdAndUpdatedDateBetween(String storeId, Date startDate,
    Date endDate, Pageable pageable) {
    return repository.findByStoreIdAndReviewPendingFalseAndUpdatedDateBetween(storeId, startDate, endDate, pageable);
  }

  @Override
  public Page<Product> findProductsByStoreIdAndUpdatedDateBetweenWithInitialization(String storeId, Date startDate,
      Date endDate, Pageable pageable) {
    Page<Product> products =
        repository.findByStoreIdAndReviewPendingFalseAndUpdatedDateBetween(storeId, startDate, endDate, pageable);
    for (Product product : products) {
      setProductCategoriesWithCategoriesCached(storeId, product, true);
      productItemServiceWrapper.setProductItemsWithProductItemImagesCached(storeId, product, false);
      for (ProductItem productItem : product.getProductItems()) {
        if (!productItem.isMarkForDelete()) {
          productItem.setProductItemImages(productItem.getProductItemImages().stream()
              .filter(productItemImage -> Optional.ofNullable(productItemImage.getOriginalImage()).orElse(false))
              .collect(Collectors.toList()));
        }
      }
    }
    return products;
  }

  @Async
  @Override
  public void publishProductByStoreIdAndUpdatedBy(String storeId, String updatedBy) {
    LOG.info("Starting to publish product for storeId = {} , updatedBy = {}",
        storeId, updatedBy);
    Pageable pageable = PageRequest.of(0, 100);
    Slice<Product> products;
    int total = 0, success = 0;
    try {
      do {
        products =
            repository.findByStoreIdAndUpdatedByAndMarkForDeleteFalse(storeId, updatedBy, pageable);
        for (Product product : products) {
          total++;
          setProductCategoriesWithCategoriesCached(storeId, product, false);
          productItemServiceWrapper.setProductItemsWithProductItemImagesCached(storeId, product, false);
          try {
            domainEventPublisherService.publishProduct(product);
            success++;
          } catch (Exception ex) {
            LOG.error("Error while publishing product with productCode {} , exception {}",
                product.getProductCode(), ex);
          }
        }
        pageable = products.nextPageable();
      } while (products.hasNext());
    } catch (Exception ex) {
      LOG.error("Error while publishing products ", ex);
    } finally {
      LOG.info("Total Product = {} , Successfully Published = {}", total, success);
    }
  }

  @Override
  public Product getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(String storeId, String productId) {
    Product product = getProductServiceBean().getProductByStoreIdAndProductIdCached(storeId, productId);
    GdnPreconditions.checkArgument(Objects.nonNull(product) && !product.isMarkForDelete(),
        ErrorMessage.PRODUCT_NOT_FOUND.getMessage() + productId);
    return product;
  }

  @Override
  public Product getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(String storeId, String productCode) {
    Product product = getProductServiceBean().getProductByStoreIdAndProductCodeCached(storeId, productCode);
    GdnPreconditions.checkArgument(Objects.nonNull(product) && !product.isMarkForDelete(),
        ErrorMessage.PRODUCT_NOT_FOUND.getMessage() + productCode);
    return product;
  }

  @Override
  public Product getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId, String productCode) {
    Product product = repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    GdnPreconditions.checkArgument(Objects.nonNull(product),
        ErrorMessage.PRODUCT_NOT_FOUND.getMessage() + productCode);
    return product;
  }

  private ProductService getProductServiceBean() {
    return applicationContext.getBean(ProductService.class);
  }

  @Override
  @Transactional(readOnly = true)
  @Cacheable(value = CacheNames.PRODUCT_CACHE, key = "#storeId +'_'+ #productId", unless = "#result == null")
  public Product getProductByStoreIdAndProductIdCached(String storeId, String productId) {
    LOG.debug("Product cache missed for storeId: {}, productId: {}", storeId, productId);
    Product product = repository.findByStoreIdAndId(storeId, productId);
    GdnPreconditions.checkArgument(Objects.nonNull(product),
        ErrorMessage.PRODUCT_NOT_FOUND.getMessage() + productId);
    Product clonedProduct = new Product();
    BeanUtils.copyProperties(product, clonedProduct,
        "productCategories", "productAttributes", "productImages", "productItems");
    return clonedProduct;
  }

  @Override
  @Transactional(readOnly = true)
  @Cacheable(value = CacheNames.PRODUCT_CACHE, key = "#storeId +'_'+ #productCode", unless = "#result == null")
  public Product getProductByStoreIdAndProductCodeCached(String storeId, String productCode) {
    LOG.debug("Product cache missed for storeId: {}, productCode: {}", storeId, productCode);
    Product product = repository.findByStoreIdAndProductCode(storeId, productCode);
    GdnPreconditions.checkArgument(Objects.nonNull(product),
        ErrorMessage.PRODUCT_NOT_FOUND.getMessage() + productCode);
    Product clonedProduct = new Product();
    BeanUtils.copyProperties(product, clonedProduct,
        "productCategories", "productAttributes", "productImages", "productItems");
    return clonedProduct;
  }

  @Override
  public void setCompleteProductDetailsCached(String storeId, Product product, boolean includeMarkForDelete) {
    setProductData(storeId, product, includeMarkForDelete);
    productItemServiceWrapper.setCompleteProductItemDetailsCached(storeId, product, includeMarkForDelete);
  }

  private void setProductData(String storeId, Product product, boolean includeMarkForDelete) {
    setProductCategoriesWithCategoriesCached(storeId, product, includeMarkForDelete);
    setProductAttributesWithValuesAndAttributeCached(storeId, product, includeMarkForDelete);
    setProductImagesCached(storeId, product, includeMarkForDelete);
  }

  @Override
  public void setProductCategoriesWithCategoriesCached(String storeId, Product product, boolean includeMarkForDelete) {
    List<ProductCategory> productCategories =
        productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(storeId, product.getId());
    if (!includeMarkForDelete) {
      productCategories.removeIf(ProductCategory::isMarkForDelete);
    }
    product.setProductCategories(productCategories);
    for (ProductCategory productCategory : product.getProductCategories()) {
      Category category =
          categoryService.getCategoryByStoreIdAndIdCached(storeId, productCategory.getCategoryId());
      productCategory.setCategory(category);
      productCategory.setProduct(product);
    }
  }

  @Override
  public void setProductAttributesWithValuesAndAttributeCached(
      String storeId, Product product, boolean includeMarkForDelete) {
    List<ProductAttribute> productAttributes = getProductAttributes(storeId, product.getId());
    if(!includeMarkForDelete) {
      productAttributes.removeIf(ProductAttribute::isMarkForDelete);
    }
    product.setProductAttributes(productAttributes);
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      productAttribute.getProductAttributeValues().forEach(productAttributeValue ->
          productAttributeValue.setProductAttribute(productAttribute));
      productAttribute.setProduct(product);
    }
  }

  @Override
  public List<ProductAttribute> getProductAttributes(String storeId, String productId) {
    return productAttributeService.getProductAttributesByStoreIdAndProductIdCached(storeId, productId);
  }

  @Override
  public void setProductImagesCached(String storeId, Product product, boolean includeMarkForDelete) {
    List<ProductImage> productImages =
        imageService.getProductImagesByStoreIdAndProductIdCached(storeId, product.getId());
    product.setProductImages(productImages);
    product.getProductImages().forEach(productImage -> productImage.setProduct(product));
    if (!includeMarkForDelete) {
      int pCounter = 0;
      int originalImageCounter = 0;
      int processedImageCounter = 0;
      List<ProductImage> removedProductImages = new ArrayList<>();
      for (ProductImage productImage : product.getProductImages()) {
        if(productImage.isMarkForDelete()) {
          removedProductImages.add(productImage);
        } else if (productImage.isMainImages() && Objects.isNull(productImage.getOriginalImage())) {
          if (pCounter > 0) {
            removedProductImages.add(productImage);
          } else {
            pCounter++;
          }
        } else if (productImage.isMainImages() && productImage.getOriginalImage()) {
          if (originalImageCounter > 0) {
            removedProductImages.add(productImage);
          } else {
            originalImageCounter++;
          }
        } else if (productImage.isMainImages() && !productImage.getOriginalImage()) {
          if (processedImageCounter > 0) {
            removedProductImages.add(productImage);
          } else {
            processedImageCounter++;
          }
        }
      }

      product.getProductImages().removeAll(removedProductImages);

      if (pCounter == 0 && originalImageCounter == 0 && processedImageCounter == 0) {
        product.getProductImages().stream()
            .filter(productImage -> Objects.nonNull(productImage) && productImage.getSequence() == 0)
            .forEach(productImage -> productImage.setMainImages(true));
      }
    }
  }

  @Override
  public void evictAllProductDetailCacheByProductCode(String storeId, String productCode) {
    Product product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId, productCode);
    evictAllProductDetailCache(storeId, product);
  }

  @Override
  public void evictAllProductDetailCache(String storeId, Product product) {
    evictProductCache(storeId, product);
    applicationCacheServiceBean.evictProductItemsCacheByStoreIdAndProductId(storeId, product.getId());
    applicationCacheServiceBean.evictProductImagesCacheByStoreIdAndProductId(storeId, product.getId());
    applicationCacheServiceBean.evictProductAttributesCacheByStoreIdAndProductId(storeId, product.getId());
    applicationCacheServiceBean.evictProductCategoriesCacheByStoreIdAndProductId(storeId, product.getId());
    applicationCacheServiceBean.evictProductItemImagesCacheByStoreIdAndProductId(storeId, product.getId());
    applicationCacheServiceBean.evictProductItemAttributeValuesCacheByStoreIdAndProductId(storeId, product.getId());
  }

  @Async
  @Override
  public void deleteImagesForDeletedProduct(String storeId, int days, int daySpan, int batchSize) {
    Pageable pageable = PageRequest.of(0, batchSize);
    Page<Product> products;
    Date endDate = DateUtils.addDays(new Date(), -days);
    Date startDate = DateUtils.addDays(endDate, -daySpan);
    log.info("Deleting images for deleted products between start date {} and end date {}", startDate, endDate);
    int count = 0;
    try {
      do {
        Set<String> locationPath = new HashSet<>();
        List<String> productCodes = new ArrayList<>();
        if (endDate.compareTo(startDate) == 0) {
          products = this.repository
              .findProductCodeByStoreIdAndMarkForDeleteTrueAndUpdatedDateLessThan(storeId, endDate, pageable);
        } else {
          products = this.repository
              .findProductCodeByStoreIdAndMarkForDeleteTrueAndUpdatedDateBetween(storeId, startDate, endDate, pageable);
        }
        for (Product product : products) {
          Hibernate.initialize(product.getProductImages());
          locationPath.addAll(product.getProductImages().stream()
              .filter(productImage -> StringUtils.isNotBlank(productImage.getLocationPath()))
              .map(productImage -> productImage.getLocationPath()).collect(Collectors.toList()));
          productCodes.add(product.getProductCode());
        }
        deleteImageByImageLocations(locationPath, false);
        deleteImageFolders(productCodes);
        count = count + products.getContent().size();
        pageable = products.nextPageable();
      } while (products.hasNext());
      log.info("Image deletion successful. Total number of products with successful image deletion : {}", count);
    } catch (Exception e) {
      log.error(
          "Error while deleting the deleted product's images. Total number of products with successful image deletion : {}",
          count, e);
    }
  }

  @Async
  @Override
  @Transactional(readOnly = false)
  public void deleteImagesForUpdatedProduct(int days, int batchSize) {
    Pageable pageable = PageRequest.of(0, batchSize);
    Page<String> locationPaths;
    Date updateDate = DateUtils.addDays(new Date(), -days);
    List<String> paths = new ArrayList<>();
    log.info("Deleting images for updated products where updated date less than :  {}", updateDate);
    int count = 0;
    try {
      do {
        locationPaths = this.productImageCleanupRepository.findLocationPathByUpdatedDateLessThan(updateDate, pageable);
        deleteImageByImageLocations(new HashSet<>(locationPaths.getContent()), false);
        count = count + locationPaths.getContent().size();
        paths.addAll(locationPaths.getContent());
        pageable = locationPaths.nextPageable();
      } while (locationPaths.hasNext());
      log.info("Image deletion successful. Total number of products with successful image deletion : {}", count);
      if (CollectionUtils.isNotEmpty(paths)) {
        productImageCleanupRepository.deleteByLocationPath(paths);
      }
    } catch (Exception e) {
      log.error(
          "Error while deleting the updated product's images. Total number of products with successful image deletion : {}",
          count, e);
    }
  }

  public void deleteImageFolders(List<String> productCodes) {
    log.debug("Deleting image folders for product codes : {}", productCodes);
    List<File> files = new ArrayList<>();
    for (String productCode : productCodes) {
      File folderImage = new File(imageSourceDirectory + File.separator + productCode);
      files.add(folderImage);
    }
    deleteDirectory(files);
  }


  public void deleteImageByImageLocations(Set<String> imageLocations, boolean deleteFromSourceLocation) {
    try {
      for (String locationPath : imageLocations) {
        if (!locationPath.startsWith(File.separator)) {
          locationPath = File.separator + locationPath;
        }
        log.info("Deleting image from location: {}", locationPath);
        if (deleteFromSourceLocation) {
          fileStorageService.deleteImages(locationPath);
        }
        log.info("Deleting final full images from location: {}", locationPath);
        fileStorageService.deleteFullFinalImagesFromGcs(locationPath);

        log.info("Deleting final medium images from location: {}", locationPath);
        fileStorageService.deleteFinalMediumImagesFromGcs(locationPath);

        log.info("Deleting final thumbnail images from location: {}", locationPath);
        fileStorageService.deleteFinalThumbnailImagesFromGcs(locationPath);
      }
    } catch (Exception e) {
      log.error("Error while deleting the final images from locations {}", imageLocations, e);
    }
  }

  @Override
  @Async
  public void deleteImageByImageLocations(Set<String> imageLocations) {
    try {
      for (String locationPath : imageLocations) {
        if (!locationPath.startsWith(File.separator)) {
          locationPath = File.separator + locationPath;
        }
        log.info("Deleting source image from the location : {}", locationPath);
         fileStorageService.deleteImages(locationPath);
      }
    } catch (Exception e) {
      log.error("Error while deleting the source images from locations {}", imageLocations, e);
    }
  }

  @Override
  public void evictProductCacheByProductCodes(String storeId, List<String> productCodes) {
    List<Product> products =
        repository.findByStoreIdAndProductCodeInAndMarkForDeleteFalse(storeId, productCodes);
    for (Product product : products) {
      evictProductCache(storeId, product);
    }
  }

  @Override
  public void evictProductCache(String storeId, Product product) {
    applicationCacheServiceBean.evictProductCacheByStoreIdAndProductCode(storeId, product.getProductCode());
    applicationCacheServiceBean.evictProductCacheByStoreIdAndProductCode(storeId, product.getId());
  }

  @Override
  public void clearProductCacheAndProductAttributesCache(String storeId, String productCode,
      String productId) {
    applicationCacheServiceBean.evictProductCacheByStoreIdAndProductCode(storeId, productCode);
    applicationCacheServiceBean.evictProductAttributesCacheByStoreIdAndProductId(storeId,
        productId);
  }

  @Override
  public void evictProductItemsAndProductItemImagesCache(String storeId, Product product) {
    applicationCacheServiceBean.evictProductItemsCacheByStoreIdAndProductId(storeId, product.getId());
    applicationCacheServiceBean.evictProductItemImagesCacheByStoreIdAndProductId(storeId, product.getId());
  }

  @Override
  public void publishProduct(Product product, boolean isNewProduct) throws Exception {
    this.domainEventPublisherService.publishProduct(product, isNewProduct);
  }

  @Override
  public void publishVatUpdateEvent(List<ProductItem> productItems) {
    if (CollectionUtils.isNotEmpty(productItems)) {
      productItems.forEach(productItem -> domainEventPublisherService
          .publishVatApplicableUpdateEvent(productItem.getSkuCode(),
              Boolean.TRUE.equals(productItem.getVatApplicable())));
    }
  }

  private void deleteDirectory(List<File> files) {
    for (File file : files) {
      try {
        FileUtils.deleteDirectory(file);
      } catch (Exception e) {
        log.info("Error deleting the images folder : {}", file.getAbsolutePath(), e);
      }
    }
  }

  @Override
  @Transactional(readOnly = false, propagation = Propagation.REQUIRES_NEW)
  public MasterProductDataUpdateDTO updateProductAndGetDimensionChanged(
      String storeId, SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO) throws Exception {
    Product product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        storeId, simpleMasterProductUpdateDTO.getProductCode());
    GdnPreconditions.checkArgument(Objects.nonNull(product),
        ErrorMessage.PRODUCT_NOT_FOUND + simpleMasterProductUpdateDTO.getProductCode());
    Category category = getCategoryByProductCodeAndId(product);
    setProductAttributesWithValuesAndAttributeCached(storeId, product, false);
    productItemServiceWrapper.setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, product, false);
    boolean isDimensionUpdated = ProductUtil.isDimensionUpdated(product, simpleMasterProductUpdateDTO);
    log.info(
        "DimensionUpdate. category : {} , product : {} , simpleMasterProductUpdateDTO : {} , isDimensionUpdated : {} ",
        category, product, simpleMasterProductUpdateDTO, isDimensionUpdated);
    if (isDimensionUpdated) {
      double shippingWeight =
          categoryShippingService.generateShippingWeight(Constants.DEFAULT_STORE_ID, category.getCategoryCode(),
              simpleMasterProductUpdateDTO.getLength(), simpleMasterProductUpdateDTO.getHeight(),
              simpleMasterProductUpdateDTO.getWeight(), simpleMasterProductUpdateDTO.getWidth());
      simpleMasterProductUpdateDTO.setShippingWeight(shippingWeight);
    } else {
      ProductUtil.getSimpleMasterProductUpdateDTOFromProduct(simpleMasterProductUpdateDTO, product);
    }
    boolean isDimensionOrDgLevelUpdated = ProductUtil.isDimensionOrDgLevelUpdated(product, simpleMasterProductUpdateDTO);
    String productName = product.getName();

    ObjectCopyUtil.copyPropertiesIgnoreNullValues(simpleMasterProductUpdateDTO, product, "brand");
    boolean isBrandUpdated = brandUpdated(storeId, simpleMasterProductUpdateDTO, product);

    if (simpleMasterProductUpdateDTO.isPostLive() && product.isReviewPending() && (
        Objects.isNull(product.getProductItems().get(0).getDangerousGoodsLevel()) || !simpleMasterProductUpdateDTO
            .getDangerousGoodsLevel().equals(product.getProductItems().get(0).getDangerousGoodsLevel()))) {
      product.getProductItems()
          .forEach(productItem -> setDGLevel(productItem, simpleMasterProductUpdateDTO.getDangerousGoodsLevel()));
    } else {
      product.getProductItems().stream()
          .map(item -> setDangerousLevelForItem(item, simpleMasterProductUpdateDTO.getDangerousGoodsLevel()))
          .map(item -> setBrandInProductItemAttributes(item, product.getBrand()))
          .forEach(item -> setGeneratedItemName(item, simpleMasterProductUpdateDTO.getName(), productName));
    }
    update(product);
    return new MasterProductDataUpdateDTO(product,isDimensionOrDgLevelUpdated, isBrandUpdated);
  }

  private boolean brandUpdated(String storeId, SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO,
      Product product) throws Exception {
    if (!product.isReviewPending() && !StringUtils.equals(simpleMasterProductUpdateDTO.getBrand(), product.getBrand())) {
      product.setBrand(simpleMasterProductUpdateDTO.getBrand());
      setBrandInPredefinedAttributeForProduct(simpleMasterProductUpdateDTO, product, storeId);
      return true;
    }
    return false;
  }

  private Category getCategoryByProductCodeAndId(Product product) {
    setProductCategoriesWithCategoriesCached(Constants.DEFAULT_STORE_ID, product, false);
    Category category = product.getProductCategories().stream().findFirst().map(ProductCategory::getCategory)
        .orElseThrow(() -> new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
            ErrorMessage.CATEGORY_NOT_FOUND + product.getProductCode()));
    return category;
  }

  private void setBrandInPredefinedAttributeForProduct(
      SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO, Product product, String storeId) throws Exception{
    Brand brand = brandService.findByBrandName(simpleMasterProductUpdateDTO.getBrand(), false);
    if(Objects.isNull(brand)){
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          ErrorMessage.BRAND_NOT_FOUND_FOR_PRODUCT_CODE.getMessage() + simpleMasterProductUpdateDTO.getProductCode());
    }
    PredefinedAllowedAttributeValue brandPredefinedAttributeValue =
        predefinedAllowedAttributeValueService.findByStoreIdAndPredefinedAllowedAttributeCode(
            storeId, brand.getBrandCode());
    product.getProductAttributes().stream()
        .filter(productAttribute -> Constants.BRAND.equals(productAttribute.getProductAttributeName()))
        .flatMap(productAttribute -> productAttribute.getProductAttributeValues().stream())
        .forEach(productAttributeValue ->
            productAttributeValue.setPredefinedAllowedAttributeValue(brandPredefinedAttributeValue));
  }

  private ProductItem setBrandInProductItemAttributes(ProductItem item, String brand) {
    if(Objects.nonNull(brand)) {
      item.getProductItemAttributeValues().stream()
          .filter(productItemAttributeValue -> Constants.BRAND.equals(productItemAttributeValue.getAttribute().getName()))
          .forEach(productItemAttributeValue -> productItemAttributeValue.setValue(brand));
    }
    return item;
  }

  private ProductItem setDangerousLevelForItem(ProductItem item, Integer dangerousGoodsLevel) {
    if(Objects.nonNull(dangerousGoodsLevel)) {
      item.setDangerousGoodsLevel(dangerousGoodsLevel);
    }
    return item;
  }

  private ProductItem setGeneratedItemName(ProductItem item, String newProductName, String oldProductName) {
    if (!StringUtils.equals(oldProductName, newProductName)) {
      String newGeneratedItemName;
      if (item.getGeneratedItemName().startsWith(oldProductName)) {
        newGeneratedItemName = item.getGeneratedItemName().replace(oldProductName, newProductName);
      } else {
        List<ProductItemAttributeValue> oldProductItemDefiningValues = item.getProductItemAttributeValues().stream()
            .filter(oldProductItemAttributeValue -> AttributeType.DEFINING_ATTRIBUTE
                .equals(oldProductItemAttributeValue.getAttribute().getAttributeType()))
            .sorted(Comparator.comparing(arg0 -> arg0.getAttribute().getId())).collect(Collectors.toList());
        newGeneratedItemName = newProductName;
        for (ProductItemAttributeValue oldProductItemDefiningValue : oldProductItemDefiningValues) {
          newGeneratedItemName = newGeneratedItemName + StringUtils.SPACE + oldProductItemDefiningValue.getValue();
        }
      }
      item.setGeneratedItemName(newGeneratedItemName);
    }
    return item;
  }

  private ProductItem setDGLevel(ProductItem productItem, int dgLevel) {
    productItem.setDangerousGoodsLevel(dgLevel);
    productItem.setInternalUpdate(true);
    return productItem;
  }

  @Transactional(readOnly = false)
  @Override
  public Product updateProductAndItemImagesByProductCode(
      ProductAndItemImageRequest productAndItemImageRequest, String storeId, boolean setDgLevel) throws Exception {
    Product product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        storeId, productAndItemImageRequest.getProductCode());
    setProductImagesCached(storeId, product, false);
    setProductCategoriesWithCategoriesCached(storeId, product, false);
    setProductAttributesWithValuesAndAttributeCached(storeId, product, false);
    productItemServiceWrapper.setCompleteProductItemDetailsCached(storeId, product, false);
    ConverterUtil.getProductToUpdateFromProductAndItemImageRequest(productAndItemImageRequest,
      product, setDgLevel);
    update(product);
    return product;
  }

  @Override
  @Transactional(readOnly = false, propagation = Propagation.REQUIRES_NEW)
  public Pair<Product, CategorySummaryResponse> updateProductCategoryByStoreIdAndProductCode(
      String storeId, String productCode, String categoryCode, boolean b2bSeller) throws Exception {
    Product product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId, productCode);
    Category newCategory = validateProductAndCategoryInput(storeId, categoryCode,b2bSeller);
    try {
      setProductCategoriesWithCategoriesCached(storeId, product, true);
    } catch (Exception e) {
      log.error("Error when updating product category : {} ", productCode, e);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.CATEGORY_NOT_PRESENT.getMessage());
    }
    Optional<ProductCategory> existingProductCategory =
        product.getProductCategories().stream().filter(productCategory -> !productCategory.isMarkForDelete())
            .findFirst();
    if (!existingProductCategory.isPresent()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.NO_ACTIVE_PRODUCT_CATEGORY_FOUND.getMessage());
    }
    if (StringUtils.equals(existingProductCategory.get().getCategory().getId(), newCategory.getId())) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.EXISTING_AND_NEW_CATEGORY_ARE_SAME.getMessage());
    } else {
      mapCategoryToProduct(storeId, product, newCategory, existingProductCategory.get());
      product.setUpdatedDate(new Date());
      update(product);
      productAttributeExtractionService.addProductsToProductAttributeExtraction(productCode, newCategory);
    }
    return Pair.of(product, CategorySummaryResponse.builder().categoryCode(newCategory.getCategoryCode())
        .categoryName(newCategory.getName()).newCategoryId(newCategory.getId())
        .oldCategoryId(existingProductCategory.get().getCategory().getId()).newCategoryBopisEligible(newCategory.isBopisEligible()).build());
  }

  private Category validateProductAndCategoryInput(String storeId, String categoryCode, boolean b2bSeller)
      throws Exception {
    Category category;
    try {
      category = categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(storeId, categoryCode);
    } catch (Exception e) {
      log.error("Error when fetching category data : {} ", categoryCode, e);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessage.CATEGORY_NOT_PRESENT.getMessage());
    }
    if (!category.isActivated()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessage.CATEGORY_STATE_INVALID.getMessage());
    }
    if (!b2bSeller && category.isB2bExclusive()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.PRODUCT_CATEGORY_CANT_BE_UPDATED_BECAUSE_THE_CATEGORY_IS_B2B.getMessage());
    }
    if (Objects.nonNull(category.getCatalog()) && !CatalogType.MASTER_CATALOG.equals(
        category.getCatalog().getCatalogType())) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          String.format(ErrorMessage.CATEGORY_NOT_BELONG_TO_MASTER_CATALOG.getMessage(), category.getCategoryCode()));
    }
    if (!categoryService.validateIsCategoryCn(storeId, categoryCode)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessage.CATEGORY_NOT_CN.getMessage());
    }
    return category;
  }

  private void mapCategoryToProduct(
      String storeId, Product product, Category newCategory, ProductCategory existingProductCategory) {
    existingProductCategory.setMarkForDelete(true);

    Optional<ProductCategory> existingInactiveProductCategory = product.getProductCategories().stream()
        .filter(productCategory -> productCategory.getCategory().getId().equals(newCategory.getId())).findFirst();

    if (existingInactiveProductCategory.isPresent()) {
      existingInactiveProductCategory.get().setMarkForDelete(false);
    } else {
      product.getProductCategories().add((new ProductCategory(product, newCategory, storeId)));
    }
  }

  @Override
  @Transactional(readOnly = false, propagation = Propagation.REQUIRES_NEW)
  public Product updateProductDimensions(WarehouseMasterSKUEvent request, String productId) throws Exception {
    CommonUtil.validateProductDimension(request, validateDimensionSwitch);
    Product product = getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(Constants.DEFAULT_STORE_ID, productId);
    setProductCategoriesWithCategoriesCached(Constants.DEFAULT_STORE_ID, product, false);
    productItemServiceWrapper.setProductItemsCached(Constants.DEFAULT_STORE_ID, product, false);
    CommonUtil.updateDimensions(product, request);
    if (CollectionUtils.isNotEmpty(request.getUpcCodes())) {
      String upcCode = request.getUpcCodes().get(0);
      product.getProductItems().stream()
          .filter(productItem -> request.getItemCode().equalsIgnoreCase(productItem.getSkuCode()))
          .forEach(productItem -> productItem.setUpcCode(upcCode));
    }
    Category category = product.getProductCategories().stream().findFirst().map(ProductCategory::getCategory)
        .orElseThrow(() -> new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
            ErrorMessage.CATEGORY_NOT_FOUND + product.getProductCode()));
    double shippingWeight =
        categoryShippingService.generateShippingWeight(Constants.DEFAULT_STORE_ID, category.getCategoryCode(),
            request.getLength(), request.getHeight(), request.getWeight(), request.getWidth());
    product.setShippingWeight(shippingWeight);
    update(product);
    return product;
  }

  public ProductSalesCategoryMappingResponse getProductSalesCategoryMapping(String storeId, String productCode,
      boolean ignoreHalalCategories) {
    Product product = getProductServiceBean().getProductByStoreIdAndProductCodeCached(storeId, productCode);
    setProductCategoriesWithCategoriesCached(storeId, product, true);
    return ConverterUtil
        .getProductSalesCategoryMappingResponse(productCode, this.getProductSalesCategoryMappingChanges(product, ignoreHalalCategories));
  }

  @Override
  @Transactional(readOnly = false)
  public String copyProduct(String oldProductCode, String newProductCode, String storeId, Product product,
      String createdMerchant) throws Exception {
    Product oldProduct = null;
    if (StringUtils.isNotBlank(oldProductCode)) {
      oldProduct = getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, oldProductCode);
    } else {
      oldProduct = product;
    }

    if (newProductCode.equals(oldProductCode)) {
      return updateProduct(oldProduct, product);
    }

    Product newProduct = new Product();
    BeanUtils.copyProperties(oldProduct, newProduct, "productCategories", "productAttributes", "productImages",
        "productItems", "id", "productCode", "createdMerchant");
    newProduct.setProductCode(newProductCode);
    newProduct.setCreatedMerchant(createdMerchant);
    copyProductCategories(oldProduct, newProduct);
    copyProductAttributes(oldProduct, newProduct);
    copyProductImages(oldProduct, newProduct);
    copyProductItems(oldProduct, newProduct, newProductCode, product);
    newProduct = saveProductWithoutProductPublish(newProduct);
    return newProduct.getId();
  }

  private String generateItemCode(String productCode, String skuCode) {
    if (StringUtils.isBlank(skuCode)) {
      return productCode + HYPHEN + this.productItemService.getSequence(productCode);
    }
    return productCode + skuCode.substring(skuCode.lastIndexOf("-"));
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public Product saveProductWithoutProductPublish(Product entity) throws Exception {
    log.info("Saving product with product code : {} product : {}", entity.getProductCode(), entity);
    Product product = repository.save(entity);
    return product;
  }

  @Override
  @Transactional(readOnly = false)
  public Pair<ProductPublishUpdateDTO, List<LocationPathAndCommonImage>> updateProductItemImagesByProductCode(ProductItemImageUpdateRequest productItemImageUpdateRequest,
      String storeId) throws Exception {
    Product product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId,
        productItemImageUpdateRequest.getProductCode());
    setProductImagesCached(storeId, product, false);
    setProductCategoriesWithCategoriesCached(storeId, product, false);
    productItemServiceWrapper.setProductItemsWithProductItemImagesCached(storeId, product, false);
    SystemParameter systemParameter =
        systemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.ACTIVE_IMAGE_SWITCH);
    if (Objects.isNull(systemParameter) || StringUtils.isBlank(systemParameter.getValue())) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "Switch not present for value : " + Constants.ACTIVE_IMAGE_SWITCH);
    }
    boolean activeSwitch = BooleanUtils.toBoolean(systemParameter.getValue());
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    if (CollectionUtils.isNotEmpty(productItemImageUpdateRequest.getUpdateProductItemImages())) {
      ConverterUtil.regenerateProductItemImageGotUpdated(product,
          productItemImageUpdateRequest.getUpdateProductItemImages(), productPublishUpdateDTO);
    }
    if (CollectionUtils.isNotEmpty(productItemImageUpdateRequest.getNewProductItemImages())) {
      ConverterUtil.regenerateAddNewProductItemImage(product,
          productItemImageUpdateRequest.getNewProductItemImages(), productPublishUpdateDTO,
          uniqueMainImageAtItemLevel, relaxActiveImageCheckInNRImageUpdate,
          productItemImageUpdateRequest.isNeedCorrection());
    }
    if (CollectionUtils.isNotEmpty(productItemImageUpdateRequest.getCopyToAllVariantImages())) {
      ConverterUtil.regenerateCopyToAllVariantImages(product, productItemImageUpdateRequest.getCopyToAllVariantImages(),
          productItemImageUpdateRequest.isNeedCorrection());
    }
    if (CollectionUtils.isNotEmpty(productItemImageUpdateRequest.getUpdateProductItemImages())) {
      ConverterUtil.regenerateProductItemImageGotUpdated(product,
          productItemImageUpdateRequest.getUpdateProductItemImages(), productPublishUpdateDTO);
    }
    Set<String> commonImageLocationPathSet = new HashSet<>();
    if (CollectionUtils.isNotEmpty(productItemImageUpdateRequest.getCopyToAllVariantImages()) || CollectionUtils
        .isNotEmpty(productItemImageUpdateRequest.getNewProductItemImages())) {
      ConverterUtil.regenerateProductImages(product, productItemImageUpdateRequest.getCopyToAllVariantImages(),
          productItemImageUpdateRequest.getNewProductItemImages(), productItemImageUpdateRequest.isNeedCorrection(), commonImageLocationPathSet);
    }
    ConverterUtil.regenerateProductImagesUpdate(product, productItemImageUpdateRequest.getUpdateProductItemImages(),
        activeSwitch, productPublishUpdateDTO, commonImageLocationPathSet);

    String commonMainImage = null;
    for (ProductImage productImage : product.getProductImages()) {
      if (productImage.isMarkForDelete()) {
        continue;
      }
      if (productImage.isMainImages() && productImage.isCommonImage()) {
        commonMainImage = productImage.getLocationPath();
      } else {
        if (productImage.isMainImages()) {
          productImage.setMainImages(false);
        }
      }
    }

    if (StringUtils.isBlank(commonMainImage)) {
      Map<String, ProductItem> productItemMap = product.getProductItems().stream().collect(Collectors.toMap(ProductItem::getSkuCode, Function.identity()));
      List<String> itemCodes = new ArrayList<>(productItemMap.keySet());
      Collections.sort(itemCodes);
      ProductItem productItem = productItemMap.get(itemCodes.get(0));
      log.info("Fetching main images from the item : {}", productItem.getSkuCode());
      Map<String, ProductItemImage> productItemImages = productItem.getProductItemImages().stream()
          .filter(productItemImage -> !productItemImage.isMarkForDelete() && productItemImage.isMainImages())
          .collect(Collectors.toMap(ProductItemImage::getLocationPath, Function.identity(), (image1, image2) -> image1));
      if (MapUtils.isEmpty(productItemImages)) {
        throw new ApplicationException(ErrorCategory.UNSPECIFIED,
            "No main image present in the product item :" + productItem.getSkuCode());
      }
      Set<String> itemImages = productItemImages.keySet();
      for (ProductImage productImage : product.getProductImages()) {
        if (productImage.isMarkForDelete()) {
          continue;
        }
        if (itemImages.contains(productImage.getLocationPath())) {
          if (!productImage.isMainImages()) {
            if (productImage.isCommonImage()) {
              commonImageLocationPathSet.add(productImage.getLocationPath());
            }
            productImage.setMainImages(true);
          }
        }
      }
    }

    ProductImageUtil.setCommonImageFlagForProductAndItemImagesWithProductPublishDto(product, true,
        productPublishUpdateDTO, commonImageLocationPathSet);
    checkMaxCommonImages(productItemImageUpdateRequest, product, commonImageLocationPathSet);
    productPublishUpdateDTO.setProductLevelDataUpdated(
        CollectionUtils.isNotEmpty(commonImageLocationPathSet) || productPublishUpdateDTO.isProductLevelDataUpdated());
    List<LocationPathAndCommonImage> locationPathAndCommonImageList =
        ProductImageUtil.getLocationPathAndCommonImages(product, productItemImageUpdateRequest);
    repository.saveAndFlush(product);
    productPublishUpdateDTO.setProduct(product);
    return Pair.of(productPublishUpdateDTO,locationPathAndCommonImageList);
  }

  public void evictCompleteProductAndItemsCache(String storeId, Product product) {
    evictProductCache(storeId, product);
    applicationCacheServiceBean.evictProductImagesCacheByStoreIdAndProductId(storeId, product.getId());
    evictProductItemsAndProductItemImagesCache(storeId, product);
  }

  @Override
  public void evictCacheForSimpleMasterProductUpdate(String storeId, String productId,
    String productCode) {
    applicationCacheServiceBean.evictProductCacheByStoreIdAndProductCode(storeId, productCode);
    applicationCacheServiceBean.evictProductCacheByStoreIdAndProductCode(storeId,productId);
    applicationCacheServiceBean.evictProductItemsCacheByStoreIdAndProductId(storeId, productId);
    applicationCacheServiceBean.evictProductAttributesCacheByStoreIdAndProductId(storeId,
      productId);
    applicationCacheServiceBean.evictProductItemAttributeValuesCacheByStoreIdAndProductId(storeId,
      productId);
  }

  private void checkMaxCommonImages(ProductItemImageUpdateRequest productItemImageUpdateRequest, Product product, Set<String> commonImageLocationPathSet) {
    if (!productItemImageUpdateRequest.isNeedCorrection()) {
      ProductImageUtil.checkCommonImageMaxCount(false, false, true, false, false, product, commonImageLocationPathSet);
    } else if (productItemImageUpdateRequest.isActivatedBefore()) {
      ProductImageUtil.checkCommonImageMaxCount(false, false, false, true, false, product, commonImageLocationPathSet);
    } else {
      ProductImageUtil.checkCommonImageMaxCount(false, false, false, false, true, product, commonImageLocationPathSet);
    }
  }

  private void copyProductCategories(Product oldProduct, Product newProduct) {
    if (CollectionUtils.isNotEmpty(oldProduct.getProductCategories())) {
      List<ProductCategory> productCategories = new ArrayList<>();
      for (ProductCategory oldProductCategory : oldProduct.getProductCategories()) {
        ProductCategory productCategory = new ProductCategory();
        BeanUtils.copyProperties(oldProductCategory, productCategory, "id", "product", "productId");
        productCategory.setProduct(newProduct);
        productCategories.add(productCategory);
      }
      newProduct.setProductCategories(productCategories);
    }
  }

  private void copyProductAttributes(Product oldProduct, Product newProduct) {
    if (CollectionUtils.isNotEmpty(oldProduct.getProductAttributes())) {
      List<ProductAttribute> productAttributes = new ArrayList<>();
      for (ProductAttribute oldProductAttribute : oldProduct.getProductAttributes()) {
        ProductAttribute productAttribute = new ProductAttribute();
        BeanUtils.copyProperties(oldProductAttribute, productAttribute, "productAttributeValues", "id", "product",
            "productId");
        productAttribute.setProduct(newProduct);

        Attribute attribute = new Attribute();
        BeanUtils.copyProperties(oldProductAttribute.getAttribute(), attribute, "allowedAttributeValues");

        List<AllowedAttributeValue> allowedAttributeValues = new ArrayList<AllowedAttributeValue>();
        if (CollectionUtils.isNotEmpty(oldProductAttribute.getAttribute().getAllowedAttributeValues())) {
          for (AllowedAttributeValue allowedAttributeValueRequest : oldProductAttribute.getAttribute()
              .getAllowedAttributeValues()) {
            AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
            BeanUtils.copyProperties(allowedAttributeValueRequest, allowedAttributeValue);
            allowedAttributeValues.add(allowedAttributeValue);
          }
        }
        attribute.setAllowedAttributeValues(allowedAttributeValues);

        List<ProductAttributeValue> productAttributeValues = new ArrayList<ProductAttributeValue>();
        if (CollectionUtils.isNotEmpty(oldProductAttribute.getProductAttributeValues())) {
          for (ProductAttributeValue productAttributeValueRequest : oldProductAttribute.getProductAttributeValues()) {
            ProductAttributeValue productAttributeValue = new ProductAttributeValue();
            BeanUtils.copyProperties(productAttributeValueRequest, productAttributeValue, "allowedAttributeValue",
                "productAttribute", "productAttributeId", "predefinedAllowedAttributeValue", "id");
            if (Objects.nonNull(productAttributeValueRequest.getAllowedAttributeValue())) {
              AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
              BeanUtils.copyProperties(productAttributeValueRequest.getAllowedAttributeValue(), allowedAttributeValue);
              allowedAttributeValue.setAttribute(attribute);
              productAttributeValue.setAllowedAttributeValue(allowedAttributeValue);
            }
            if (Objects.nonNull(productAttributeValueRequest.getPredefinedAllowedAttributeValue())) {
              PredefinedAllowedAttributeValue allowedAttributeValue = new PredefinedAllowedAttributeValue();
              BeanUtils.copyProperties(productAttributeValueRequest.getPredefinedAllowedAttributeValue(),
                  allowedAttributeValue);
              allowedAttributeValue.setAttribute(attribute);
              productAttributeValue.setPredefinedAllowedAttributeValue(allowedAttributeValue);
            }
            productAttributeValue.setProductAttribute(productAttribute);
            productAttributeValues.add(productAttributeValue);
          }
        }
        productAttribute.setAttribute(attribute);
        productAttribute.setProductAttributeValues(productAttributeValues);
        productAttributes.add(productAttribute);
      }
      newProduct.setProductAttributes(productAttributes);
    }
  }

  private void copyProductImages(Product oldProduct, Product newProduct) {
    if (CollectionUtils.isNotEmpty(oldProduct.getProductImages())) {
      List<ProductImage> productImages = new ArrayList<>();
      for (ProductImage oldProductImage : oldProduct.getProductImages()) {
        ProductImage productImage = new ProductImage();
        BeanUtils.copyProperties(oldProductImage, productImage, "id", "product", "productId");
        productImage.setProduct(newProduct);
        productImages.add(productImage);
      }
      newProduct.setProductImages(productImages);
    }
  }

  private void copyProductItems(Product oldProduct, Product newProduct, String newProductCode, Product product) {
    if (CollectionUtils.isNotEmpty(oldProduct.getProductItems())) {
      List<ProductItem> productItems = new ArrayList<>();
      for (ProductItem oldProductItem : oldProduct.getProductItems()) {
        ProductItem productItem = new ProductItem();
        BeanUtils
            .copyProperties(oldProductItem, productItem, "id", "product", "productId", "productItemAttributeValues",
                "productItemImages", "skuCode", "sourceItemCode");
        productItem.setProduct(newProduct);
        productItem.setSourceItemCode(oldProductItem.getSkuCode());
        productItem.setSkuCode(generateItemCode(newProductCode, oldProductItem.getSkuCode()));
        if (Objects.nonNull(product)) {
          productItem.setContentChanged(true);
        } else {
          productItem.setContentChanged(false);
        }

        copyItemAttributes(oldProductItem, productItem);
        copyItemImages(oldProductItem, productItem);
        productItems.add(productItem);
      }
      newProduct.setProductItems(productItems);
    }
  }

  private void copyItemAttributes(ProductItem oldProductItem, ProductItem productItem) {
    if (CollectionUtils.isNotEmpty(oldProductItem.getProductItemAttributeValues())) {
      List<ProductItemAttributeValue> productItemAttributeValues = new ArrayList<>();
      for (ProductItemAttributeValue oldProductItemAttribute : oldProductItem.getProductItemAttributeValues()) {
        ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
        BeanUtils
            .copyProperties(oldProductItemAttribute, productItemAttributeValue, "id", "productItem", "productItemId");
        productItemAttributeValue.setProductItem(productItem);
        productItemAttributeValues.add(productItemAttributeValue);
      }
      productItem.setProductItemAttributeValues(productItemAttributeValues);
    }
  }

  private void copyItemImages(ProductItem oldProductItem, ProductItem productItem) {
    if (CollectionUtils.isNotEmpty(oldProductItem.getProductItemImages())) {
      List<ProductItemImage> productItemImages = new ArrayList<>();
      for (ProductItemImage oldProductItemImage : oldProductItem.getProductItemImages()) {
        ProductItemImage productItemImage = new ProductItemImage();
        BeanUtils.copyProperties(oldProductItemImage, productItemImage, "id", "productItem", "productItemId");
        productItemImage.setProductItem(productItem);
        productItemImages.add(productItemImage);
      }
      productItem.setProductItemImages(productItemImages);
    }
  }

  private String updateProduct(Product savedProduct, Product product) throws Exception {
    BeanUtils.copyProperties(product, savedProduct, "productCategories", "productAttributes", "productImages",
        "productItems", "id", "productCode");
    updateProductAttributes(savedProduct, product);
    updateProductItems(savedProduct, product);
    savedProduct = saveProductWithoutProductPublish(savedProduct);
    evictAllProductDetailCache(savedProduct.getStoreId(), savedProduct);
    return savedProduct.getId();
  }

  private void updateProductAttributes(Product savedProduct, Product product) {
    Map<String, ProductAttribute> productAttributeMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(product.getProductAttributes())) {
      productAttributeMap = product.getProductAttributes().stream()
          .filter(productAttribute -> isNotDefiningAttributeAndVariantCreationFalse(productAttribute.getAttribute()))
          .collect(Collectors
              .toMap(productAttribute -> productAttribute.getAttribute().getAttributeCode(), Function.identity()));
    }

    if (CollectionUtils.isNotEmpty(savedProduct.getProductAttributes())) {
      for (ProductAttribute productAttribute : savedProduct.getProductAttributes()) {
        if (isDefiningAttributeOrVariantCreationTrue(productAttribute.getAttribute())) {
          continue;
        }

        ProductAttribute newProductAttribute =
            productAttributeMap.get(productAttribute.getAttribute().getAttributeCode());
        if (Objects.isNull(newProductAttribute)) {
          log.warn("Setting MFD true for attribute : {} for product code : {}", productAttribute.getAttribute().getAttributeCode(), savedProduct.getProductCode());
          productAttribute.setMarkForDelete(true);
          continue;
        }

        log.info("Checking for attribute code : {} for product : {}",
            productAttribute.getAttribute().getAttributeCode(), savedProduct.getProductCode());
        List<ProductAttributeValue> productAttributeValues = productAttribute.getProductAttributeValues();
        List<ProductAttributeValue> newProductAttributeValues = newProductAttribute.getProductAttributeValues();
        if (AttributeType.PREDEFINED_ATTRIBUTE.equals(productAttribute.getAttribute().getAttributeType())) {
          for (ProductAttributeValue productAttributeValue : productAttributeValues) {
            productAttributeValue.setPredefinedAllowedAttributeValue(
                newProductAttributeValues.get(0).getPredefinedAllowedAttributeValue());
          }
        } else if (AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(productAttribute.getAttribute().getAttributeType())) {
          for (ProductAttributeValue productAttributeValue : productAttributeValues) {
            productAttributeValue
                .setDescriptiveAttributeValue(newProductAttributeValues.get(0).getDescriptiveAttributeValue());
          }
        }
        productAttributeMap.remove(productAttribute.getAttribute().getAttributeCode());
      }
    }

    if (MapUtils.isNotEmpty(productAttributeMap)) {
      for (Map.Entry<String, ProductAttribute> entry : productAttributeMap.entrySet()) {
        log.info("Adding new attribute {} for product : {}", entry.getKey(), savedProduct.getProductCode());
        ProductAttribute newProductAttribute = entry.getValue();
        newProductAttribute.setProduct(savedProduct);
        savedProduct.getProductAttributes().add(newProductAttribute);
      }
    }
  }

  private void updateProductItems(Product savedProduct, Product product) {
    if (CollectionUtils.isNotEmpty(savedProduct.getProductItems()) && CollectionUtils.isNotEmpty(product.getProductItems())) {
      Map<String, ProductItem> newProductItems = product.getProductItems().stream()
          .collect(Collectors.toMap(productItem -> productItem.getSkuCode(), Function.identity()));
      for (ProductItem oldProductItem : savedProduct.getProductItems()) {
        ProductItem newProductItem = newProductItems.get(oldProductItem.getSkuCode());
        if(Objects.nonNull(newProductItem)) {
          BeanUtils.copyProperties(newProductItem, oldProductItem, "id", "product", "productId",
              "productItemAttributeValues", "productItemImages", "skuCode", "sourceItemCode", "hash", "dangerousGoodsLevel");
          oldProductItem.setContentChanged(true);
        }
      }
    }
  }

  @Override
  public Product getImagesByProductCode(String storeId, String productCode) throws Exception {
    log.info("Fetching images for scaling for product code : {}", productCode);
    Product product = getProductServiceBean().getProductByStoreIdAndProductCodeCached(storeId, productCode);
    productItemServiceWrapper.setProductItemsWithProductItemImagesCached(storeId, product, false);
    return product;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public UpdateNeedRevisionDTO updateAndMarkForNeedRevision(Product product) throws Exception {
    Product savedProduct = getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        product.getStoreId(), product.getProductCode());
    log.info("Update product content, product code : {}", product.getProductCode());
    if (product.getVersion() < savedProduct.getVersion()) {
      log.error("Updated product version : {}, Current product version : {}", product.getVersion(),
          savedProduct.getVersion());
      throw new ApplicationException(ErrorCategory.INVALID_STATE, ErrorMessage.PRODUCT_CONTENT_EXPIRED.getMessage());
    }
    boolean isBrandChanged = !StringUtils.equals(savedProduct.getBrand(), product.getBrand());
    ProductSalesCategoryMapping salesCategoryReferenceByMasterCategory =
        setProductContentDetails(savedProduct, product);
    setProductImagesCached(savedProduct.getStoreId(), savedProduct, true);
    productItemServiceWrapper.setProductItemsWithProductItemImagesCached(savedProduct.getStoreId(), savedProduct, true);
    regenerateProductImageForNeedRevision(savedProduct, product);
    regenerateProductItemImageForNeedRevision(savedProduct, product);
    updateNeedCorrectionProductFlags(savedProduct);
    log.info("Product saving for NR {} ", savedProduct);
    repository.saveAndFlush(savedProduct);
    return UpdateNeedRevisionDTO.builder().isBrandChanged(isBrandChanged)
      .productSalesCategoryMapping(salesCategoryReferenceByMasterCategory).product(savedProduct)
      .build();
  }

  @Override
  public void evictCacheOnMarkForNeedRevision(Product savedProduct,
    boolean isBrandChanged, ProductSalesCategoryMapping salesCategoryReferenceByMasterCategory)
    throws Exception {
    evictAllProductDetailCache(savedProduct.getStoreId(), savedProduct);
    publishProduct(savedProduct, salesCategoryReferenceByMasterCategory, isBrandChanged);
    applicationCacheServiceBean.evictProductImagesCacheByStoreIdAndProductId(
      savedProduct.getStoreId(), savedProduct.getId());
    evictProductItemsAndProductItemImagesCache(savedProduct.getStoreId(), savedProduct);
  }

  private void updateNeedCorrectionProductFlags(Product product) {
    product.setActivated(false);
    product.setViewable(false);
    for (ProductItem productItem : product.getProductItems()) {
      productItem.setActivated(false);
      productItem.setViewable(false);
    }
  }

  private void regenerateProductItemImageForNeedRevision(Product savedProduct, Product product) {
    Map<String, ProductItem> savedProductItemMap =
        savedProduct.getProductItems().stream().collect(Collectors.toMap(ProductItem::getSkuCode, Function.identity()));
    Map<String, ProductItem> newProductItemMap =
        product.getProductItems().stream().collect(Collectors.toMap(ProductItem::getSkuCode, Function.identity()));
    for (Map.Entry<String, ProductItem> savedProductItem : savedProductItemMap.entrySet()) {
      if (newProductItemMap.containsKey(savedProductItem.getValue().getSkuCode())) {
        savedProductItem.getValue().setProductItemImages(getProductItemImageList(savedProductItem.getValue(),
            newProductItemMap.get(savedProductItem.getValue().getSkuCode())));
      }
    }
    savedProduct.getProductImages().forEach(productImage -> productImage.setRevised(false));
    savedProduct.getProductItems().forEach(productItem -> productItem.getProductItemImages()
        .forEach(productItemImage -> productItemImage.setRevised(false)));
  }

  private List<ProductItemImage> getProductItemImageList(ProductItem savedProductItem, ProductItem newProductItem) {
    Map<String, List<ProductItemImage>> imageNameToImageListMapExistingItem =
        ConverterUtil.imageNameToListOfItemImageMap(savedProductItem.getProductItemImages());
    Map<String, List<ProductItemImage>> imageNameToImageListNewItem =
        ConverterUtil.imageNameToListOfItemImageMap(newProductItem.getProductItemImages());
    for (Map.Entry<String, List<ProductItemImage>> existingItemEntry :
        imageNameToImageListMapExistingItem.entrySet()) {
      if (!imageNameToImageListNewItem.containsKey(existingItemEntry.getKey())) {
        existingItemEntry.getValue().forEach(productItemImage -> productItemImage.setMarkForDelete(true));
      } else {
        existingItemEntry.getValue().forEach(productItemImage -> {
          productItemImage.setMainImages(imageNameToImageListNewItem.get(existingItemEntry.getKey()).stream()
              .findFirst().orElse(new ProductItemImage()).isMainImages());
        });
      }
    }
    Map<String, ProductItemImage> locationPathToImageMapExistingItem = savedProductItem.getProductItemImages().stream()
        .collect(Collectors.toMap(ProductItemImage::getLocationPath, Function.identity(),
            (existing, replacement) -> existing.isMarkForDelete() ? replacement : existing));
    Map<String, ProductItemImage> locationPathToImageMapNewItem =
        newProductItem.getProductItemImages().stream().collect(Collectors.toMap(ProductItemImage::getLocationPath,
            Function.identity(), (v1, v2) -> v2));
    for (Map.Entry<String, ProductItemImage> newItemEntry : locationPathToImageMapNewItem.entrySet()) {
      if (!locationPathToImageMapExistingItem.containsKey(newItemEntry.getKey()) && !imageNameToImageListMapExistingItem
          .containsKey(ConverterUtil.imageNameFromLocationPath(newItemEntry.getKey()))) {
        ProductItemImage productItemImage = new ProductItemImage();
        BeanUtils.copyProperties(newItemEntry.getValue(), productItemImage);
        productItemImage.setActive(false);
        productItemImage.setOriginalImage(false);
        productItemImage.setRevised(false);
        locationPathToImageMapExistingItem.put(newItemEntry.getValue().getLocationPath(), productItemImage);
      }
    }
    return locationPathToImageMapExistingItem.values().stream().collect(Collectors.toList());
  }

  private void regenerateProductImageForNeedRevision(Product savedProduct, Product product) {
    Map<String, List<ProductImage>> imageNameToImageListMapExistingProduct =
        ConverterUtil.imageNameToListOfImageMap(savedProduct.getProductImages());
    Map<String, List<ProductImage>> imageNameToImageListNewProduct =
        ConverterUtil.imageNameToListOfImageMap(product.getProductImages());
    for (Map.Entry<String, List<ProductImage>> existingProductEntry :
        imageNameToImageListMapExistingProduct.entrySet()) {
      if (!imageNameToImageListNewProduct.containsKey(existingProductEntry.getKey())) {
        existingProductEntry.getValue().forEach(productImage -> productImage.setMarkForDelete(true));
      } else {
        existingProductEntry.getValue().forEach(productImage -> {
          productImage.setMainImages(
              imageNameToImageListNewProduct.get(existingProductEntry.getKey()).stream().findFirst()
                  .orElse(new ProductImage()).isMainImages());
        });
      }
    }

    Map<String, ProductImage> locationPathToImageMapExistingProduct = savedProduct.getProductImages().stream().collect(
        Collectors.toMap(ProductImage::getLocationPath, Function.identity(),
            (existing, replacement) -> existing.isMarkForDelete() ? replacement : existing));
    Map<String, ProductImage> locationPathToImageMapNewProduct = product.getProductImages().stream()
        .collect(Collectors.toMap(ProductImage::getLocationPath, Function.identity(), (v1, v2) -> v2));
    for (Map.Entry<String, ProductImage> newProductEntry : locationPathToImageMapNewProduct.entrySet()) {
      if (!locationPathToImageMapExistingProduct.containsKey(newProductEntry.getKey())
          && !imageNameToImageListMapExistingProduct
          .containsKey(ConverterUtil.imageNameFromLocationPath(newProductEntry.getKey()))) {
        ProductImage productImage = new ProductImage();
        BeanUtils.copyProperties(newProductEntry.getValue(), productImage);
        productImage.setActive(false);
        productImage.setOriginalImage(false);
        productImage.setRevised(false);
        locationPathToImageMapExistingProduct.put(newProductEntry.getValue().getLocationPath(), productImage);
      }
    }
    savedProduct.setProductImages(new ArrayList<>(locationPathToImageMapExistingProduct.values()));
  }

  private ProductSalesCategoryMapping setProductContentDetails(Product savedProduct, Product product) {
    ProductSalesCategoryMapping salesCategoryReferenceByMasterCategory = null;
    setCompleteProductDetailsCached(savedProduct.getStoreId(), savedProduct, true);
    BeanUtils.copyProperties(product, savedProduct, "createdMerchant", "productAttributes",
        "productCategories", "productImages", "productItems", "version", "createdBy", "createdDate",
        "video", "distributionInfo", "aiGeneratedFields");
    if (StringUtils.isNotBlank(product.getDistributionInfo())) {
      savedProduct.setDistributionInfo(product.getDistributionInfo());
    }
    boolean categoryUpdated = false;
    CategoryChangeDTO categoryChangeDTO = regenerateProductCategories(savedProduct, product);
    if (Objects.nonNull(categoryChangeDTO) && (Objects.nonNull(categoryChangeDTO.getNewCategoryId())) && (Objects
        .nonNull(categoryChangeDTO.getOldCategoryId()))) {
      salesCategoryReferenceByMasterCategory =
          categoryReferenceService.getSalesCategoryReferenceByMasterCategory(categoryChangeDTO.getOldCategoryId(),
              categoryChangeDTO.getNewCategoryId(), false);
      categoryUpdated = true;
    }
    regenerateProductAttributeContent(savedProduct, product, true, categoryUpdated);
    regenerateProductItemContent(savedProduct, product);
    regenerateProductImageContent(savedProduct, product);
    return salesCategoryReferenceByMasterCategory;
  }

  @Override
  @Transactional(readOnly = false)
  // check based on NR flag in ProductImageEditRequest.
  public Pair<ProductPublishUpdateDTO, Map<String, Map<String, String>>> updateImages(String storeId, boolean computeCommonImage, List<ProductImageEditRequest> productImageEditRequestList,
    ProductDTO productDTO, boolean combinedEditRequest)
      throws Exception {
    String productCode =  productImageEditRequestList.get(0).getProductCode();
    Product product = getProductAndProductItemsWithImages(storeId, productCode, productDTO);
    Map<String, ProductItem> productItemMap =
        product.getProductItems().stream().collect(Collectors.toMap(ProductItem::getSkuCode, Function.identity()));
    Map<String, Map<String, String>> result = new HashMap<>();

    String commonMainImageChangedLocation = null;
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false,
      new HashSet<>(), null);

    boolean containsCommonMainImage =
        productImageEditRequestList.stream().map(ProductImageEditRequest::getCopyToAllVariantImages)
            .filter(Objects::nonNull).filter(Predicate.not(CopyImageEditRequest::isMarkForDelete))
            .filter(CopyImageEditRequest::isMainImage).findAny().isPresent();

    for(ProductImageEditRequest productImageEditRequest : productImageEditRequestList) {
      Map<String, String> errorMap = new HashMap<>();
      // Updating item image for single variant
      if (CollectionUtils.isNotEmpty(productImageEditRequest.getProductItems())) {
        Set<String> mainImagesAdded = productImageEditRequest.getProductItems().stream()
            .filter(Predicate.not(ItemImageEditRequest::isMarkForDelete)).filter(CopyImageEditRequest::isMainImage)
            .map(ItemImageEditRequest::getItemCode).collect(Collectors.toSet());
        log.info("Main Image Added For : {} ", mainImagesAdded);
        for (ItemImageEditRequest itemImageEditRequest : productImageEditRequest.getProductItems()) {
          ProductItem productItem = productItemMap.get(itemImageEditRequest.getItemCode());
          ConverterUtil.updateItemImages(productItem, itemImageEditRequest, productImageEditRequest.getImagePath(),
            errorMap, productImageEditRequest.isNeedRevision(), false, productPublishUpdateDTO, mainImagesAdded, containsCommonMainImage);
          productItemMap.put(productItem.getSkuCode(), productItem);
        }
      }

      // Updating item image to all variant for common images
      if (Objects.nonNull(productImageEditRequest.getCopyToAllVariantImages())) {
        Map<String, String> copyErrorMap = new HashMap<>();
        log.info("Main Image Added For : {} ", containsCommonMainImage);
        CopyImageEditRequest copyImageEditRequest = productImageEditRequest.getCopyToAllVariantImages();
        for (Map.Entry<String, ProductItem> entry : productItemMap.entrySet()) {
          ProductItem productItem = entry.getValue();
          ConverterUtil.updateItemImages(productItem, productImageEditRequest.getCopyToAllVariantImages(),
            productImageEditRequest.getImagePath(), copyErrorMap, productImageEditRequest.isNeedRevision(), true,
            productPublishUpdateDTO, new HashSet<>(), containsCommonMainImage);
          productItemMap.put(productItem.getSkuCode(), productItem);
        }
        if (copyErrorMap.size() == 0) {
          errorMap.put(Constants.COPY_ALL_STATUS, Status.SUCCESS.name());
          if (Objects.nonNull(copyImageEditRequest.isMainImage()) && copyImageEditRequest.isMainImage()) {
            commonMainImageChangedLocation = ProductImageUtil.getImageFileName(productImageEditRequest.getImagePath());
          }
        } else if (copyErrorMap.size() < productItemMap.size()) {
          CopyImageEditRequest revertChangedForCopyAll = new CopyImageEditRequest();
          revertChangedForCopyAll.setHashCode(copyImageEditRequest.getHashCode());
          errorMap.put(Constants.COPY_ALL_STATUS, Status.PARTIAL_SUCCESS.name());
          boolean revertChanges = false;
          if (copyImageEditRequest.isAdd() && !copyImageEditRequest.isMainImage()) {
            revertChangedForCopyAll.setMarkForDelete(true);
            revertChanges = true;
          }

          if (copyImageEditRequest.isMarkForDelete()) {
            revertChangedForCopyAll.setAdd(true);
            revertChanges = true;
          }

          Set<String> failedItemCodes = copyErrorMap.keySet();
          if (revertChanges) {
            log.info("Partial update happened for the request {} so reverting the change for location path : {}",
              copyImageEditRequest, productImageEditRequest.getImagePath());
            for (Map.Entry<String, ProductItem> entry : productItemMap.entrySet()) {
              ProductItem productItem = entry.getValue();
              if (failedItemCodes.contains(productItem.getSkuCode())) {
                continue;
              }
              ConverterUtil.updateItemImages(productItem, revertChangedForCopyAll,
                productImageEditRequest.getImagePath(), copyErrorMap, productImageEditRequest.isNeedRevision(), true,
                productPublishUpdateDTO, new HashSet<>(), containsCommonMainImage);
              productItemMap.put(productItem.getSkuCode(), productItem);
            }
          }
        } else {
          errorMap.put(Constants.COPY_ALL_STATUS, Status.FAIL.name());
        }
      }
      result.put(productImageEditRequest.getImagePath(), errorMap);
    }

    List<ProductItem> productItems = new ArrayList<>();
    for (Map.Entry<String, ProductItem> entry : productItemMap.entrySet()) {
      productItems.add(entry.getValue());
    }
    product.setProductItems(productItems);

    Map<String, ProductImage> productImages =
      product.getProductImages().stream().filter(productImage -> !productImage.isMarkForDelete())
        .collect(Collectors.toMap(ProductImage::getLocationPath, Function.identity(), (image1, image2) -> image1));
    Set<String> imageLocationPath = new HashSet<>();

    Set<String> commonImageLocationPathSet = new HashSet<>();
    String firstItemMainImageLocationPath = null;
    for (ProductItem productItem : product.getProductItems()) {
      boolean hasMainImage = false;
      for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
        if (productItemImage.isMarkForDelete()) {
          continue;
        }
        if (productItemImage.isMainImages()) {
          hasMainImage = true;
          if (StringUtils.isBlank(firstItemMainImageLocationPath)) {
            firstItemMainImageLocationPath = ProductImageUtil.getImageFileName(productItemImage.getLocationPath());
          }
        }
        if (!productImages.containsKey(productItemImage.getLocationPath())) {
          ProductImage productImage = new ProductImage();
          BeanUtils.copyProperties(productItemImage, productImage, "id", "product");
          productImage.setProduct(product);
          if (productImage.isCommonImage()) {
            commonImageLocationPathSet.add(productImage.getLocationPath());
          }
          product.getProductImages().add(productImage);
          productImages.put(productImage.getLocationPath(), productImage);
        }
        imageLocationPath.add(productItemImage.getLocationPath());
      }
      if (!hasMainImage) {
        throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "No main image present in the product item :" + productItem.getSkuCode());
      }
    }

    for (ProductImage productImage : product.getProductImages()) {
      if (productImage.isMarkForDelete()) {
        continue;
      }
      if (!imageLocationPath.contains(productImage.getLocationPath())) {
        productImage.setMarkForDelete(true);
        if(productImage.isCommonImage()) {
          productPublishUpdateDTO.setProductLevelDataUpdated(true);
        }
        continue;
      }
      if (productImage.isMainImages() && productImage.isCommonImage()) {
        if (StringUtils.isNotBlank(commonMainImageChangedLocation) && !commonMainImageChangedLocation.equals(
          ProductImageUtil.getImageFileName(productImage.getLocationPath()))) {
          commonImageLocationPathSet.add(productImage.getLocationPath());
          productImage.setMainImages(false);
        }
      } else {
        productImage.setMainImages(false);
      }
    }

    String commonMainImage = null;
    for (ProductImage productImage : product.getProductImages()) {
      if (productImage.isMarkForDelete()) {
        continue;
      }
      if (productImage.isMainImages() && productImage.isCommonImage()) {
        commonMainImage = productImage.getLocationPath();
      }
    }

    if (StringUtils.isBlank(commonMainImage)) {
      for (ProductImage productImage : product.getProductImages()) {
        if (productImage.isMarkForDelete()) {
          continue;
        }
        if (firstItemMainImageLocationPath.equals(ProductImageUtil.getImageFileName(productImage.getLocationPath()))) {
          if (!productImage.isMainImages() && productImage.isCommonImage()) {
            commonImageLocationPathSet.add(productImage.getLocationPath());
          }
          productImage.setMainImages(true);
        }
      }
    }

    ProductImageUtil.setCommonImageFlagForProductAndItemImagesWithProductPublishDto(product, computeCommonImage,
      productPublishUpdateDTO, commonImageLocationPathSet);
    checkMaxCommonImages(productImageEditRequestList.get(0), product, commonImageLocationPathSet);
    productPublishUpdateDTO.setProductLevelDataUpdated(
      CollectionUtils.isNotEmpty(commonImageLocationPathSet) || productPublishUpdateDTO.isProductLevelDataUpdated());
    ProductImageUtil.setActiveFlagInProductAndItemImages(product);
    if(combinedEditRequest){
      productPublishUpdateDTO.setProductDTO(ConverterUtil.convertProductToDTO(product));
      return Pair.of(productPublishUpdateDTO, result);
    }
    repository.saveAndFlush(product);
    productPublishUpdateDTO.setProduct(product);
    return Pair.of(productPublishUpdateDTO, result);
  }

  private Product getProductAndProductItemsWithImages(String storeId, String productCode,
    ProductDTO productDTO) {
    Product product;
    if (Objects.nonNull(productDTO)) {
      log.info("updating product Item images for product : {} with Item Images  {}  ",
        productDTO.getProductCode(), Optional.ofNullable(productDTO.getProductItems()).orElse(Collections.emptyList()).
            stream().map(ProductItem::getProductItemImages).flatMap(List::stream).collect(Collectors.toList()));
      productDTO.getProductItems().forEach(productItem -> productItemService.removeDeletedProductItemImagesWithoutFilteringMainImages(productItem));
      product = ConverterUtil.convertProductDTOToProduct(productDTO);
    } else {
      product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId, productCode);
      setProductImagesCached(storeId, product, false);
      setProductCategoriesWithCategoriesCached(storeId, product, false);
      productItemServiceWrapper.setProductItemsWithProductItemImagesCachedWithoutFilteringMainImages(
        storeId, product, false);
    }
    return product;
  }

  @Override
  public List<String> getListOfProductCodesEligibleForDeletion(String storeId, Date updatedDate, int limit) {
    return repository.findByStoreIdAndUpdatedDateLessThanAndMarkForDeleteTrueAndPickedForDeletionFalse(storeId,
        updatedDate, limit);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updatePickedForDeletionFlagByProductCode(String storeId, List<String> productCodes,
      boolean pickedForDeletion) {
    repository.updatePickedForDeletionByProductCode(storeId, productCodes, pickedForDeletion);
  }

  private void checkMaxCommonImages(ProductImageEditRequest productImageEditRequest, Product product,
      Set<String> commonImageLocationPathSet) {
    if (!productImageEditRequest.isNeedRevision()) {
      ProductImageUtil.checkCommonImageMaxCount(false, false, true, false, false, product, commonImageLocationPathSet);
    } else if (productImageEditRequest.isActivatedBefore()) {
      ProductImageUtil.checkCommonImageMaxCount(false, false, false, true, false, product, commonImageLocationPathSet);
    } else {
      ProductImageUtil.checkCommonImageMaxCount(false, false, false, false, true, product, commonImageLocationPathSet);
    }
  }

  @Override
  public void setProductAndItemImagesCached(String storeId, Product product, boolean includeMarkForDelete) {
    setProductImagesCached(storeId, product, includeMarkForDelete);
    productItemServiceWrapper.setProductItemsWithProductItemImagesCached(storeId, product, includeMarkForDelete);
  }

  @Override
  public void evictProductAndItemImageCache(Product product) {
    applicationCacheServiceBean.evictProductImagesCacheByStoreIdAndProductId(product.getStoreId(), product.getId());
    applicationCacheServiceBean.evictProductItemImagesCacheByStoreIdAndProductId(product.getStoreId(), product.getId());
  }

  @Override
  public void evictProductItemCache(Product product) {
    applicationCacheServiceBean.evictProductItemsCacheByStoreIdAndProductId(product.getStoreId(),
        product.getId());
  }

  @Override
  public void setProductItemIdForNewlyAddedItems(List<NewlySavedItemResponse> newlySavedItemResponseList,
      Product product) {
    if (CollectionUtils.isNotEmpty(newlySavedItemResponseList)) {
      List<ProductItem> productItemList =
          productItemService.getProductItemsByStoreIdAndProductIdCached(product.getStoreId(), product.getId());
      Map<String, ProductItem> itemNameAndItemMap =
          productItemList.stream().filter(Predicate.not(ProductItem::isMarkForDelete)).collect(Collectors.toMap(productItem -> productItem.getGeneratedItemName().trim(),
            Function.identity()));
      for (NewlySavedItemResponse newlySavedItemResponse : newlySavedItemResponseList) {
        String newSavedItemGeneratedItemName =
            Optional.of(newlySavedItemResponse).map(NewlySavedItemResponse::getGeneratedItemName)
                .orElse(StringUtils.EMPTY).trim();
        if (itemNameAndItemMap.containsKey(newSavedItemGeneratedItemName)) {
          ProductItem productItem = itemNameAndItemMap.get(newSavedItemGeneratedItemName);
          newlySavedItemResponse.setProductItemId(productItem.getId());
          newlySavedItemResponse.setItemCode(productItem.getSkuCode());
        }
      }
    }
  }

  @Override
  @Transactional(readOnly = false)
  public Pair<Product,String> updateProductBrand(
      String storeId, ProductBrandUpdateDTO productBrandUpdateDTO) throws Exception {
    Product product = getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        storeId, productBrandUpdateDTO.getProductCode());
    GdnPreconditions.checkArgument(Objects.nonNull(product),
        StringUtils.join(ErrorMessage.PRODUCT_NOT_FOUND, productBrandUpdateDTO.getProductCode()));
    BrandResponse oldBrand = brandService.findByBrandCodeCached(storeId, productBrandUpdateDTO.getOldBrandCode());
    BrandResponse newBrand = brandService.findByBrandCodeCached(storeId, productBrandUpdateDTO.getNewBrandCode());
    GdnPreconditions.checkArgument(Objects.nonNull(oldBrand),
        StringUtils.join(ErrorMessage.BRAND_NOT_FOUND, productBrandUpdateDTO.getOldBrandCode()));
    GdnPreconditions.checkArgument(Objects.nonNull(newBrand),
        StringUtils.join(ErrorMessage.BRAND_NOT_FOUND, productBrandUpdateDTO.getNewBrandCode()));
    boolean brandAuthCheck = true;
    for (String businessPartnerCode : Optional.ofNullable(
        productBrandUpdateDTO.getBusinessPartnerCodes()).orElse(new HashSet<>())) {
      brandAuthCheck &=
          brandAuthorisationServiceBean.checkBrandAuthBySellerCode(storeId, businessPartnerCode,
              productBrandUpdateDTO.getNewBrandCode(), false);
      GdnPreconditions.checkArgument(brandAuthCheck,
          ErrorMessage.BRAND_AUTH_NOT_FOUND_FOR_SELLER_AND_BRAND_CODE.getMessage()
              + businessPartnerCode + " " + productBrandUpdateDTO.getNewBrandCode());
    }
    product.setBrand(newBrand.getBrandName());
    setProductAttributesWithValuesAndAttributeCached(storeId, product, false);
    productItemServiceWrapper.setProductItemsWithProductItemAttributeValuesAndAttributeCached(
        storeId, product, false);
    setBrandInPredefinedAttributeForProduct(product, storeId, newBrand.getBrandCode());
    product.getProductItems().forEach(item -> setBrandInProductItemAttributes(item, newBrand.getBrandName()));
    update(product);
    return Pair.of(product, newBrand.getBrandCode());
  }

  private void setBrandInPredefinedAttributeForProduct(Product product, String storeId, String brandCode)
      throws Exception {
    PredefinedAllowedAttributeValue brandPredefinedAttributeValue =
        predefinedAllowedAttributeValueService.findByStoreIdAndPredefinedAllowedAttributeCode(storeId, brandCode);
    product.getProductAttributes().stream()
        .filter(productAttribute -> Constants.BRAND.equals(productAttribute.getProductAttributeName()))
        .flatMap(productAttribute -> productAttribute.getProductAttributeValues().stream()).forEach(
            productAttributeValue -> productAttributeValue.setPredefinedAllowedAttributeValue(
                brandPredefinedAttributeValue));
  }

  private void setProductForNewlyAddedItems(Product finalProduct) {
    finalProduct.getProductItems().forEach(item -> item.setProductId(finalProduct.getId()));
    finalProduct.getProductAttributes().forEach(productAttribute -> {
      productAttribute.setProduct(finalProduct);
      productAttribute.setProductId(finalProduct.getId());
    });
    finalProduct.getProductAttributes().stream().map(ProductAttribute::getProductAttributeValues)
      .forEach(productAttributeValue -> productAttributeValue.stream()
        .map(ProductAttributeValue::getProductAttribute).forEach(productAttribute -> {
          productAttribute.setProductId(finalProduct.getId());
          productAttribute.setProduct(finalProduct);
        }));
  }

  private static void setProductAndProductItemInFinalProductItemImages(Product finalProduct) {
    finalProduct.getProductItems()
      .forEach(productItem -> productItem.getProductItemImages().forEach(productItemImage -> {
        productItemImage.setProductItem(productItem);
        productItemImage.setProductItemId(productItem.getId());
      }));
    finalProduct.getProductImages().forEach(productImage -> {
      productImage.setProduct(finalProduct);
      productImage.setProductId(finalProduct.getId());
      productImage.setStoreId(finalProduct.getStoreId());
    });
    ProductImageUtil.setCommonImageFlagForProductAndItemImages(finalProduct, true);
  }

  @Override
  public Product getProductByStoreIdAndProductCode(String storeId, String productCode) {
    return repository.findByStoreIdAndProductCode(storeId, productCode);
  }

  @Override
  public Product findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId, String productCode) {
    return repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
  }

  @Override
  @Transactional(rollbackFor = Exception.class, readOnly = false)
  public Pair<Product,List<ProductAttributeValue>> fetchProductAndInsertMissingProductAttributes(
      CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel) {
    Product product = this.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        productAttributeDataBackFillingEventModel.getStoreId(),
        productAttributeDataBackFillingEventModel.getProductCode());

    if (Objects.nonNull(product)) {
      ProductCategory productCategory =
          Optional.ofNullable(product.getProductCategories()).orElse(new ArrayList<>()).stream()
              .filter(Predicate.not(ProductCategory::isMarkForDelete)).findFirst().get();
      List<Attribute> attributes = new ArrayList<>();
      List<String> categoryAttributeIds = new ArrayList<>();
      List<String> productAttributeIds = new ArrayList<>();
      List<String> attributesToBeMapped = new ArrayList<>();
      Map<String, MigrationPayload> attributeIdAndMigrationPayloadMap = new HashMap<>();
      if (CollectionUtils.isNotEmpty(
          productAttributeDataBackFillingEventModel.getMigrationPayloadList())) {
        attributesToBeMapped =
            productAttributeDataBackFillingEventModel.getMigrationPayloadList().stream()
                .map(MigrationPayload::getAttributeId).toList();
        attributeIdAndMigrationPayloadMap =
            productAttributeDataBackFillingEventModel.getMigrationPayloadList().stream()
                .collect(Collectors.toMap(MigrationPayload::getAttributeId, Function.identity()));
      } else {
        categoryAttributeIds =
            getCategoryAttributesAndIds(productAttributeDataBackFillingEventModel.getStoreId(),
                productCategory);
        productAttributeIds = getProductAttributeAndIds(product);
        categoryAttributeIds.removeAll(productAttributeIds);
        attributesToBeMapped = categoryAttributeIds;
      }

      if (CollectionUtils.isNotEmpty(attributesToBeMapped)) {
        attributes = attributeService.findByAttributeIds(
            productAttributeDataBackFillingEventModel.getStoreId(), attributesToBeMapped);
        Map<String, Attribute> onVariantCreatingattributeIdAndAttributeMap =
            attributes.stream().filter(Predicate.not(Attribute::isVariantCreation))
                .collect(Collectors.toMap(Attribute::getId, Function.identity()));
        List<ProductAttribute> productAttributesToBeBackFilled = new ArrayList<>();
        insertMissingProductAttributesAndSave(attributes, product, productAttributesToBeBackFilled);
        productAttributesToBeBackFilled =
            productAttributeService.saveMissedProductAttributes(productAttributesToBeBackFilled);
        List<ProductAttributeValue> productAttributeValues = new ArrayList<>();
        insertMissingProductAttributeValues(productAttributesToBeBackFilled,
            onVariantCreatingattributeIdAndAttributeMap,
            productAttributeDataBackFillingEventModel.getStoreId(), productAttributeValues,
            attributeIdAndMigrationPayloadMap);
        if (CollectionUtils.isNotEmpty(productAttributeValues)) {
          return Pair.of(product,
              productAttributeValueService.saveProductAttributeValues(productAttributeValues));
        }
      } else {
        return Pair.of(product, new ArrayList<>());
      }
    }
    return Pair.of(null, new ArrayList<>());
  }

  private List<String> getCategoryAttributesAndIds(String storeId,
      ProductCategory productCategory) {
    List<CategoryAttribute> categoryAttributes =
        categoryService.getCategoryAttributes(storeId, productCategory.getCategoryId());
    return new ArrayList<>(
        categoryAttributes.stream().filter(Predicate.not(CategoryAttribute::isMarkForDelete))
            .map(CategoryAttribute::getAttribute).filter(Attribute::isMustShowOnCustomerSide)
            .map(Attribute::getId).toList());
  }


  private static List<String> getExistingProductAttributeIds(Product product) {
    List<ProductAttribute> productAttributes =
        Optional.ofNullable(product.getProductAttributes()).orElse(new ArrayList<>()).stream()
            .filter(Predicate.not(ProductAttribute::isMarkForDelete)).toList();
    return productAttributes.stream().map(ProductAttribute::getAttribute).map(Attribute::getId)
        .toList();
  }

  private void insertMissingProductAttributeValues(
      List<ProductAttribute> productAttributesToBeBackFilled,
      Map<String, Attribute> attributeIdAndAttributeMap, String storeId,
      List<ProductAttributeValue> productAttributeValues,
      Map<String, MigrationPayload> migrationPayloadMap) {
    for (ProductAttribute productAttribute : productAttributesToBeBackFilled) {
      Attribute attribute = attributeIdAndAttributeMap.get(productAttribute.getAttributeId());
      if (Objects.nonNull(attribute)) {
        ProductAttributeValue productAttributeValue = new ProductAttributeValue();
        if (AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(
            Optional.ofNullable(attribute.getAttributeType()).orElse(AttributeType.ALL))) {
          productAttributeValue.setDescriptiveAttributeValue(
              Optional.ofNullable(migrationPayloadMap.get(attribute.getId()))
                  .map(MigrationPayload::getAttributeValue).orElse(Constants.HYPHEN));
          productAttributeValue.setDescriptiveAttributeValueType(
              DescriptiveAttributeValueType.SINGLE);
        } else if (AttributeType.PREDEFINED_ATTRIBUTE.equals(
            Optional.ofNullable(attribute.getAttributeType()).orElse(AttributeType.ALL))) {
          if (imeiMigrationEnabled) {
            List<ProductAttributeValue> existingProductAttributeValueList =
                productAttributeValueService.getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(
                    storeId, Set.of(productAttribute.getId()));
            existingProductAttributeValueList.forEach(productAttributeValue1 -> {
              productAttributeValue1.setProductAttribute(productAttribute);
              productAttributeValue1.setMarkForDelete(true);
              productAttributeValue1.setPredefinedAllowedAttributeValue(
                  predefinedAllowedAttributeValueService.findByStoreIdAndId(storeId,
                      productAttributeValue1.getPredefinedAllowedAttributeValueId()));
            });
            productAttributeValues.addAll(existingProductAttributeValueList);
          }
          PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
              getPredefinedAllowedAttributeValue(storeId, attribute,
                  Optional.ofNullable(migrationPayloadMap.get(attribute.getId()))
                      .map(MigrationPayload::getAttributeValue).orElse(Constants.HYPHEN));
          productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
          productAttributeValue.setDescriptiveAttributeValueType(
              DescriptiveAttributeValueType.PREDEFINED);
          productAttributeValue.setPredefinedAllowedAttributeValueId(
              Optional.ofNullable(predefinedAllowedAttributeValue)
                  .map(PredefinedAllowedAttributeValue::getId).orElse(null));
        } else if (AttributeType.DEFINING_ATTRIBUTE.equals(
            Optional.ofNullable(attribute.getAttributeType()).orElse(AttributeType.ALL))) {
          AllowedAttributeValue allowedAttributeValue =
              allowedAttributeValueService.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
                  storeId, attribute,
                  Optional.ofNullable(migrationPayloadMap.get(attribute.getId()))
                      .map(MigrationPayload::getAttributeValue).orElse(Constants.HYPHEN));
          productAttributeValue.setDescriptiveAttributeValueType(
              DescriptiveAttributeValueType.NONE);
          productAttributeValue.setAllowedAttributeValueId(
              Optional.ofNullable(allowedAttributeValue).map(AllowedAttributeValue::getId)
                  .orElse(null));
          productAttributeValue.setAllowedAttributeValue(allowedAttributeValue);
        }
        productAttributeValue.setProductAttributeId(productAttribute.getId());
        productAttributeValue.setStoreId(storeId);
        productAttributeValue.setProductAttribute(productAttribute);
        productAttributeValues.add(productAttributeValue);
      }
    }
  }

  private PredefinedAllowedAttributeValue getPredefinedAllowedAttributeValue(String storeId,
      Attribute attribute, String value) {
    return predefinedAllowedAttributeValueService.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        storeId, attribute, value);
  }

  private List<ProductAttribute> insertMissingProductAttributesAndSave(List<Attribute> attributes,
      Product product, List<ProductAttribute> productAttributesToBeBackFilled) {
    Map<String, ProductAttribute> attributeIdToProductAttributeMap =
        product.getProductAttributes().stream().collect(
            Collectors.toMap(ProductAttribute::getAttributeId, Function.identity(),
                (oldValue, newValue) -> oldValue));

    for (Attribute attribute : attributes) {
      if (imeiMigrationEnabled && attributeIdToProductAttributeMap.containsKey(attribute.getId())) {
        productAttributesToBeBackFilled.add(
            attributeIdToProductAttributeMap.get(attribute.getId()));
      } else {
        ProductAttribute productAttribute = new ProductAttribute();
        productAttribute.setProduct(product);
        productAttribute.setAttribute(attribute);
        productAttribute.setProductId(product.getId());
        productAttribute.setProductAttributeName(attribute.getName());
        productAttribute.setAttributeId(attribute.getId());
        productAttribute.setStoreId(attribute.getStoreId());
        productAttributesToBeBackFilled.add(productAttribute);
      }
    }
    return productAttributesToBeBackFilled;
  }

  @Override
  public Product findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String productId) {
    return repository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, productId);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void processCompressedUpdatedVideo(String productCode, VideoDTO videoDTO)
    throws JsonProcessingException {
    String videoData = objectMapper.writeValueAsString(videoDTO);
    repository.updateVideoByStoreIDAndProductCode(Constants.DEFAULT_STORE_ID, productCode,
      videoData);
    log.info("video data successfully updated for product : {} ", productCode);
  }


  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void deleteMfdTrueRowsFromProduct(Product product) {
    List<String> mfdTrueProductImageIds = getMfdTrueProductImageIds(product);
    List<String> mfdTrueProductItemImageIds = getMfdTrueProductItemImageIds(product);
    List<String> mfdTrueProductCategoryIds = getMfdTrueProductCategoryIds(product);
    List<String> mfdTrueProductAttributeIds = getMfdTrueProductAttributeIds(product);
    List<String> mfdTrueProductItemAttributeIds = getMfdTrueProductItemAttributeIds(product);

    if (CollectionUtils.isNotEmpty(mfdTrueProductCategoryIds)) {
      productCategoryService.deleteByProductCategoryIds(mfdTrueProductCategoryIds);
    }
    if (CollectionUtils.isNotEmpty(mfdTrueProductImageIds)) {
      imageService.deleteProductImagesByIds(mfdTrueProductImageIds);
    }
    if (CollectionUtils.isNotEmpty(mfdTrueProductItemImageIds)) {
      imageService.deleteProductItemImagesByIds(mfdTrueProductItemImageIds);
    }
    if (CollectionUtils.isNotEmpty(mfdTrueProductAttributeIds)) {
      productAttributeService.deleteByProductAttributeIds(mfdTrueProductAttributeIds);
    }
    if (CollectionUtils.isNotEmpty(mfdTrueProductItemAttributeIds)) {
      productItemAttributeValueService.deleteByProductItemAttributeIds(mfdTrueProductItemAttributeIds);
    }
  }

  private static List<String> getMfdTrueProductItemAttributeIds(Product product) {
    return Optional.ofNullable(product.getProductItems()).orElseGet(ArrayList::new).stream().filter(
            productItem -> CollectionUtils.isNotEmpty(productItem.getProductItemAttributeValues()))
        .map(ProductItem::getProductItemAttributeValues).flatMap(List::stream)
        .filter(ProductItemAttributeValue::isMarkForDelete).map(ProductItemAttributeValue::getId)
        .collect(Collectors.toList());
  }

  private static List<String> getMfdTrueProductItemImageIds(Product product) {
    return Optional.ofNullable(product.getProductItems()).orElseGet(ArrayList::new).stream()
        .filter(productItem -> CollectionUtils.isNotEmpty(productItem.getProductItemImages()))
        .map(ProductItem::getProductItemImages).flatMap(List::stream)
        .filter(ProductItemImage::isMarkForDelete).map(ProductItemImage::getId)
        .collect(Collectors.toList());
  }

  private static List<String> getMfdTrueProductAttributeIds(Product product) {
    return Optional.ofNullable(product.getProductAttributes()).orElseGet(ArrayList::new).stream()
        .filter(ProductAttribute::isMarkForDelete).map(ProductAttribute::getId)
        .collect(Collectors.toList());
  }

  private static List<String> getMfdTrueProductCategoryIds(Product product) {
    return Optional.ofNullable(product.getProductCategories()).orElseGet(ArrayList::new).stream()
        .filter(ProductCategory::isMarkForDelete).map(ProductCategory::getId)
        .collect(Collectors.toList());
  }

  private static List<String> getMfdTrueProductImageIds(Product product) {
    return Optional.ofNullable(product.getProductImages()).orElseGet(ArrayList::new).stream()
        .filter(ProductImage::isMarkForDelete).map(ProductImage::getId)
        .collect(Collectors.toList());
  }

  @Override
  public AuditTrailListResponse updateMasterDataAndGenerateHistory(String storeId, String username,
      ProductMasterDataUpdateRequest productMasterDataUpdateRequest, Product savedProduct)
      throws JsonProcessingException {
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    if (!StringUtils.equals(savedProduct.getName(), productMasterDataUpdateRequest.getName())) {
      auditTrailDtoList.add(CommonUtil.getAuditTrailDto(productMasterDataUpdateRequest.getName(),
          savedProduct.getName(), productMasterDataUpdateRequest,
          Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_NAME));
      productItemServiceWrapper.setProductItemsWithProductItemAttributeValuesAndAttributeCached(
          storeId, savedProduct, false);
      savedProduct.getProductItems().forEach(
          items -> setGeneratedItemName(items, productMasterDataUpdateRequest.getName(),
              savedProduct.getName()));
      savedProduct.setName(productMasterDataUpdateRequest.getName());
    }

    String existingDescription = Objects.nonNull(savedProduct.getDescription()) ?
        new String(savedProduct.getDescription()) :
        StringUtils.EMPTY;
    String updatedDescription = Objects.nonNull(productMasterDataUpdateRequest.getDescription()) ?
        new String(productMasterDataUpdateRequest.getDescription()) :
        StringUtils.EMPTY;
    if (!StringUtils.equals(existingDescription, updatedDescription)) {
      auditTrailDtoList.add(CommonUtil.getAuditTrailDto(updatedDescription, existingDescription,
          productMasterDataUpdateRequest, Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_DESCRIPTION));
      savedProduct.setDescription(productMasterDataUpdateRequest.getDescription());
    }
    if (Objects.nonNull(productMasterDataUpdateRequest.getShippingWeight())) {
      updateDimensions(username, productMasterDataUpdateRequest, savedProduct, auditTrailDtoList);
    }

    productMasterDataUpdateRequest.setUrl(
        Optional.ofNullable(productMasterDataUpdateRequest.getUrl()).orElse(StringUtils.EMPTY));
    if (!StringUtils.equals(savedProduct.getUrl(), productMasterDataUpdateRequest.getUrl())) {
      auditTrailDtoList.add(
          CommonUtil.getAuditTrailDto(productMasterDataUpdateRequest.getUrl(),
              savedProduct.getUrl(), productMasterDataUpdateRequest,
              Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_URL));
      savedProduct.setUrl(productMasterDataUpdateRequest.getUrl());
    }

    String updatedVideo = null;
    if (Objects.nonNull(productMasterDataUpdateRequest.getVideoAddEditRequest())
        && StringUtils.isNotBlank(
        productMasterDataUpdateRequest.getVideoAddEditRequest().getVideoId())) {
      updatedVideo = ConverterUtil.convertVideoAddEditRequestToDTO(productMasterDataUpdateRequest.getVideoAddEditRequest());
    }
    if (StringUtils.isNotBlank(updatedVideo) || productMasterDataUpdateRequest.isVideoDelete()) {
      savedProduct.setVideo(updatedVideo);
    }
    if (Objects.nonNull(productMasterDataUpdateRequest.getReviewPending())) {
      savedProduct.setReviewPending(productMasterDataUpdateRequest.getReviewPending());
    }
    AuditTrailListResponse auditTrailListResponse = null;
    auditTrailListResponse =
        getAuditTrailListResponse(username, auditTrailDtoList, auditTrailListResponse);
    return auditTrailListResponse;
  }

  private void updateDimensions(String username,
      ProductMasterDataUpdateRequest productMasterDataUpdateRequest, Product savedProduct,
      List<AuditTrailDto> auditTrailDtoList) {
    validateAndConstructHistoryForDimensions(productMasterDataUpdateRequest.getLength(),
        savedProduct.getLength(), productMasterDataUpdateRequest, auditTrailDtoList,
        Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_LENGTH);
    validateAndConstructHistoryForDimensions(productMasterDataUpdateRequest.getHeight(),
        savedProduct.getHeight(), productMasterDataUpdateRequest, auditTrailDtoList,
        Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_HEIGHT);
    validateAndConstructHistoryForDimensions(productMasterDataUpdateRequest.getWidth(),
        savedProduct.getWidth(), productMasterDataUpdateRequest, auditTrailDtoList,
        Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_WIDTH);
    validateAndConstructHistoryForDimensions(productMasterDataUpdateRequest.getWeight(),
        savedProduct.getWeight(), productMasterDataUpdateRequest, auditTrailDtoList,
        Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_WEIGHT);
    validateAndConstructHistoryForDimensions(productMasterDataUpdateRequest.getShippingWeight(),
        savedProduct.getShippingWeight(), productMasterDataUpdateRequest, auditTrailDtoList,
        Constants.UPDATE_PRODUCT_ACTIVITY_UPDATE_SHIPPING_WEIGHT);
    savedProduct.setLength(productMasterDataUpdateRequest.getLength());
    savedProduct.setHeight(productMasterDataUpdateRequest.getHeight());
    savedProduct.setWidth(productMasterDataUpdateRequest.getWidth());
    savedProduct.setWeight(productMasterDataUpdateRequest.getWeight());
    savedProduct.setShippingWeight(productMasterDataUpdateRequest.getShippingWeight());
  }

  private void validateAndConstructHistoryForDimensions(Double updatedValue, Double existingValue,
      ProductMasterDataUpdateRequest productMasterDataUpdateRequest,
      List<AuditTrailDto> auditTrailDtoList, String activity) {
    if (!Objects.equals(updatedValue, existingValue)) {
      auditTrailDtoList.add(
          CommonUtil.getAuditTrailDto(Double.toString(updatedValue), Double.toString(existingValue),
              productMasterDataUpdateRequest, activity));
    }
  }

  private AuditTrailListResponse getAuditTrailListResponse(String username,
      List<AuditTrailDto> auditTrailDtoList, AuditTrailListResponse auditTrailListResponse) {
    if (CollectionUtils.isNotEmpty(auditTrailDtoList)) {
      auditTrailListResponse = new AuditTrailListResponse();
      auditTrailListResponse.setAccessChannel(Constants.CLIENT_ID);
      auditTrailListResponse.setClientId(Constants.CLIENT_ID);
      auditTrailListResponse.setRequestId(UUID.randomUUID().toString());
      auditTrailListResponse.setUpdateDirectly(true);
      auditTrailListResponse.setUpdateDirectlyToDB(true);
      auditTrailListResponse.setChangedBy(username);
      auditTrailListResponse.setAuditTrailResponseList(auditTrailDtoList);
    }
    return auditTrailListResponse;
  }

  @Override
  public ValidOmniChannelSkuResponse checkOmniChannelSkuExistsInSeller(String storeId, OmniChannelSkuRequest request,
      boolean needUomInfo) {
    ValidOmniChannelSkuResponse response = new ValidOmniChannelSkuResponse();
    List<String> skuList = request.getOmniChannelSkus();
    if (skuList.isEmpty()) {
      return response;
    }
    List<ProductItem> productItems =
        productItemService.findByStoreIdAndCreatedMerchantAndOmniChannelSkuInAndMarkForDeleteFalse(storeId,
            request.getSellerCode(), request.getOmniChannelSkus());
    if (CollectionUtils.isEmpty(productItems)) {
      return response;
    }
    Map<String, ProductItem> skuCodeAndProductItemMap =
        productItems.stream().collect(Collectors.toMap(ProductItem::getSkuCode, Function.identity(), (a, b) -> a));
    Map<String, ProductItemUomInfo> productItemUomInfoMap = new HashMap<>();
    productItemUomInfoMap = fetchUomInfoIfNeeded(storeId, needUomInfo, productItems, productItemUomInfoMap);
    Map<String, ProductL1AndL2CodeResponse> map = productItems.stream().collect(
        Collectors.toMap(ProductItem::getOmniChannelSku,
            dto -> new ProductL1AndL2CodeResponse(CommonUtil.extractProductCodeFromItemCode(dto.getSkuCode()),
                dto.getSkuCode(), dto.getOmniChannelSku(), dto.getGeneratedItemName(), dto.getId())));

    setUomInfo(map, productItemUomInfoMap, skuCodeAndProductItemMap);
    response.setExistingOmniChannelSkusAndProductDetailsMap(map);
    return response;
  }

  private static void setUomInfo(Map<String, ProductL1AndL2CodeResponse> map,
      Map<String, ProductItemUomInfo> productItemUomInfoMap, Map<String, ProductItem> skuCodeAndProductItemMap) {
    for (Map.Entry<String, ProductL1AndL2CodeResponse> entry : map.entrySet()) {
      if (productItemUomInfoMap.containsKey(entry.getValue().getSkuCode())) {
        DistributionInfoPerSkuResponse distributionInfoPerSkuResponse =
            CommonUtil.mapToDistributionInfoPerSkuResponse(productItemUomInfoMap.get(entry.getValue().getSkuCode()),
                new Product(), skuCodeAndProductItemMap);
        entry.getValue().setDimensionsAndUomResponse(distributionInfoPerSkuResponse.getDimensionsAndUomResponse());
        entry.getValue().setDistributionItemInfoResponse(distributionInfoPerSkuResponse.getDistributionItemInfoResponse());
      }
    }
  }

  private Map<String, ProductItemUomInfo> fetchUomInfoIfNeeded(String storeId, boolean needUomInfo,
      List<ProductItem> productItems, Map<String, ProductItemUomInfo> productItemUomInfoMap) {
    if (needUomInfo) {
      List<ProductItemUomInfo> productItemUomInfoList =
          productItemUomInfoService.findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(storeId,
              productItems.stream().map(ProductItem::getSkuCode).collect(Collectors.toList()));
      productItemUomInfoMap = Optional.ofNullable(productItemUomInfoList).orElse(new ArrayList<>()).stream()
          .collect(Collectors.toMap(ProductItemUomInfo::getSkuCode, Function.identity(), (a, b) -> a));
    }
    return productItemUomInfoMap;
  }

  @Transactional
  public Product updateOnlyBrandNameOfProduct(String storeId,
      ProductBrandDataUpdateRequest productBrandDataUpdateRequest) {
    Product product = repository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId,
        productBrandDataUpdateRequest.getProductCode());
    product.setBrand(productBrandDataUpdateRequest.getNewBrandName());
    List<ProductItem> productItems =
        productItemService.getProductItemsByStoreIdAndProductIdCached(storeId, product.getId());
    Attribute attribute =
        attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(storeId, ATTRIBUTE_CODE_FOR_BRAND);
    for (ProductItem productItem : productItems) {
      if (!productItem.isMarkForDelete()) {
        productItemAttributeValueService.updateOnlyValueForProductItemAttributeValuesByAttributeId(storeId,
            attribute.getId(), productBrandDataUpdateRequest.getNewBrandName(), productItem);
      }
    }
    return productRepository.save(product);
  }
}
