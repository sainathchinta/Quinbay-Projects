package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.x.productcategorybase.dto.ProductItemDTO;
import com.gdn.x.productcategorybase.dto.ProductItemImageDTO;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
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
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.entity.GdnBaseEntity;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.repository.ProductItemRepository;
import com.gdn.x.productcategorybase.repository.ProductRepository;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.ImageService;
import com.gdn.x.productcategorybase.service.ProductCategoryService;
import com.gdn.x.productcategorybase.service.ProductItemAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductItemService;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.solr.dao.SolrProductDao;
import com.gdn.x.productcategorybase.util.CommonUtil;
import com.google.common.collect.Iterables;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductItemServiceBean implements ProductItemService {

  public static final String PRODUCT_ITEM_WITH_SAME_UPC_EXIST =
      "Product item with same UPC %s already exists";

  public  static final Logger LOG = LoggerFactory.getLogger(ProductItemServiceBean.class);
  public static final String HYPHEN = "-";
  public static final String NEW_PRODUCT_CODE_PREFIX = "MTA";

  @Autowired
  private SolrProductDao solrProductDao;

  @Autowired
  private ProductItemRepository repository;

  @Autowired
  private ProductRepository productRepository;

  @Lazy
  @Autowired
  private ProductCategoryService productCategoryService;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Autowired
  @Lazy
  private ImageService imageService;

  @Autowired
  @Lazy
  private ProductService productService;

  @Autowired
  private ProductItemAttributeValueService productItemAttributeValueService;

  @Autowired
  private AttributeService attributeService;

  @Autowired
  private ApplicationContext applicationContext;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  private CategoryService categoryService;

  @Value("${consider.any.image.as.main.image.in.case.of.main.image.not.present}")
  private boolean considerAnyImageAsMainImageInCaseOfMainImageNotPresent;

  @Value("${unique.main.image.at.item.level}")
  private boolean uniqueMainImageAtItemLevel;

  @Value("${product.item.images.partition.size}")
  private int productItemImagesPartitionSize;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Value("${distribution.seller.list}")
  private Set<String> distributionSellerList;

  private void activateDeactivateProductItem(String storeId, ProductItem productItem,
      boolean activated) throws Exception {
    productItem.setActivated(activated);
    this.update(productItem);
  }

  @Override
  @Transactional(readOnly = false)
  public void activateProductItem(String storeId, ProductItem productItem) throws Exception {
    if ((productItem.getUpcCode() != null) && (productItem.getSkuCode() != null)
        && (productItem.getUpcCode().trim().length() != 0)
        && (productItem.getSkuCode().trim().length() != 0)) {
      this.activateDeactivateProductItem(storeId, productItem, true);
    }
  }

  @Override
  public Long countBySkuCode(String storeId, String skuCode) {
    return this.repository.countByStoreIdAndSkuCode(storeId, skuCode);
  }

  @Override
  @Transactional(readOnly = false)
  @Deprecated
  public void deactivateProductItem(String storeId, List<ProductItem> productItems)
      throws Exception {
    for (ProductItem productItem : productItems) {
      if (!productItem.isMarkForDelete()) {
        this.deactivateProductItem(storeId, productItem);
      }
    }
  }

  @Override
  @Transactional(readOnly = false)
  @Deprecated
  public void deactivateProductItem(String storeId, ProductItem productItem) throws Exception {
    this.activateDeactivateProductItem(storeId, productItem, false);
  }

  @Override
  @Transactional(readOnly = false)
  public void delete(String id) throws Exception {
    ServiceBeanHelper.deleteEntity(id, this.repository);
  }

  @Override
  public ProductItem findById(String id) throws Exception {
    return this.repository.findById(id).orElse(null);
  }

  @Override
  public Page<ProductItem> findByMultipleUpcCode(String storeId, List<String> upcCodes,
      Pageable pageable) {
    String upcCodesRegex = "";
    for (String upcCode : upcCodes) {
      upcCodesRegex += "(" + upcCode + ")|";
    }
    upcCodesRegex = upcCodesRegex.substring(0, upcCodesRegex.length() - 1);
    Page<ProductItem> productItems =
        this.repository.findByStoreIdAndUpcCodeMatches(storeId, upcCodesRegex, pageable);
    productItems.getContent().forEach(productItem -> setProductCached(storeId, productItem));
    return productItems;
  }

  @Override
  public Page<ProductItem> findByMultipleUpcCodeExcludeOneItem(String storeId,
      List<String> upcCodes, String skuCode, Pageable pageable) {
    String upcCodesRegex = "";
    for (String upcCode : upcCodes) {
      upcCodesRegex += "(" + upcCode + ")|";
    }
    upcCodesRegex = upcCodesRegex.substring(0, upcCodesRegex.length() - 1);
    Page<ProductItem> productItems =
        this.repository.findByStoreIdAndUpcCodeExcludeOneItemMatches(storeId, upcCodesRegex, skuCode, pageable);
    productItems.getContent().forEach(productItem -> setProductCached(storeId, productItem));
    return productItems;
  }

  @Override
  public List<ProductItem> findBySkuCodes(String storeId, List<String> skuCodes, boolean fetchArchived) throws Exception {
    List<ProductItem> productItems;
    if (fetchArchived){
      productItems = this.repository.findByStoreIdAndSkuCodeIn(storeId, skuCodes);
    }
    else {
      productItems = this.repository.findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(storeId, skuCodes);
    }
    for (ProductItem productItem : productItems) {
      if(fetchArchived) {
        Hibernate.initialize(productItem.getProduct());
      } else {
        setProductCached(storeId, productItem);
      }
      Hibernate.initialize(productItem.getProductItemImages());
      Hibernate.initialize(productItem.getProductItemAttributeValues());
      for (ProductItemAttributeValue productItemAttributeValue : productItem.getProductItemAttributeValues()) {
        Hibernate.initialize(productItemAttributeValue.getAttribute());
      }
    }
    return productItems;
  }

  @Override
  public Page<ProductItem> findByStoreId(String storeId, Pageable pageable) throws Exception {
    Page<ProductItem> productItems = this.repository.findByStoreIdAndMarkForDeleteFalse(storeId, pageable);
    productItems.getContent().forEach(productItem -> setProductCached(storeId, productItem));
    return productItems;
  }

  @Override
  public Page<ProductItem> findByStoreIdAndGeneratedItemName(String storeId,
			String generatedItemName, Pageable pageable) throws Exception {
		Page<ProductItem> page = this.repository
				.findByStoreIdAndGeneratedItemNameContainingIgnoreCaseAndMarkForDeleteFalse(
						storeId, generatedItemName, pageable);
		for (ProductItem productItem : page) {
		  setProductCached(storeId, productItem);
      Hibernate.initialize(productItem.getProductItemImages());
		}
		return page;
	}

  @Override
  public ProductItem findByStoreIdAndId(String storeId, String productItemId) throws Exception {
    ProductItem productItem = this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, productItemId);
    setProductCached(storeId, productItem);
    return productItem;
  }

  @Override
  public ProductItem findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String productItemId) {
    return this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, productItemId);
  }

  @Override
  public List<ProductItem> findByStoreIdAndCreatedMerchantAndOmniChannelSkuInAndMarkForDeleteFalse(String storeId,
      String sellerCode, List<String> omniChannelSkus) {
    return this.repository.findByStoreIdAndCreatedMerchantAndOmniChannelSkuInAndMarkForDeleteFalse(storeId, sellerCode,
        omniChannelSkus);
  }

  @Override
  public ProductItem findByStoreIdAndSkuCode(String storeId, String skuCode) throws Exception {
    ProductItem productItem =
        this.repository.findByStoreIdAndSkuCodeAndMarkForDeleteFalse(storeId, skuCode);
    GdnPreconditions.checkArgument(productItem != null,
        "not found product item with skuCode " + skuCode);
    setProductCached(storeId, productItem);
    Hibernate.initialize(productItem.getProductItemImages());
    Hibernate.initialize(productItem.getProductItemAttributeValues());
    for (ProductItemAttributeValue productItemAttributeValue : productItem.getProductItemAttributeValues()) {
      Hibernate.initialize(productItemAttributeValue.getAttribute());
    }
    return productItem;
  }

  @Override
  public Page<ProductItem> findByStoreIdAndViewableAndGeneratedItemNameOrUpcCode(String storeId,
      boolean viewable, boolean isOnlyExternal, String itemNameOrUpcCode, Pageable pageable) {
    Page<ProductItem> productPage;
    if (isOnlyExternal) {
      productPage = this.repository
          .findProductItemList(storeId, viewable, itemNameOrUpcCode.toUpperCase(),
              itemNameOrUpcCode.toUpperCase(), pageable);
    } else {
      productPage = this.repository
          .findAllProductItemList(storeId, viewable, itemNameOrUpcCode.toUpperCase(),
              itemNameOrUpcCode.toUpperCase(), pageable);
    }

    for (ProductItem item : productPage) {
      setProductCached(storeId, item);
      Hibernate.initialize(item.getProductItemImages());
    }
    return productPage;
  }

  @Override
  public Page<ProductItem> findByUpcCode(String storeId, String upcCode, Pageable pageable) {
    Page<ProductItem> productItems =
        this.repository.findByStoreIdAndUpcCodeStartingWithIgnoreCaseAndMarkForDeleteFalse(storeId, upcCode, pageable);
    productItems.getContent().forEach(productItem -> setProductCached(storeId, productItem));
    return productItems;
  }

  @Override
  public List<ProductItem> findProductItemByProductIdInitAttrValue(String storeId, String productId) {
    List<ProductItem> productItems = getProductItemService().getProductItemsByStoreIdAndProductIdCached(storeId, productId);
    for (ProductItem productItem : productItems) {
      setProductCached(storeId, productItem);
      Hibernate.initialize(productItem.getProductItemAttributeValues());
      for (ProductItemAttributeValue productItemAttributeValue : productItem.getProductItemAttributeValues()) {
        Hibernate.initialize(productItemAttributeValue.getAttribute());
      }
    }
    return productItems;
  }

  public ApplicationCacheServiceBean getApplicationCacheServiceBean() {
    return this.applicationCacheServiceBean;
  }

  @Override
  public String getSequence(String productCode) {
    return StringUtils.leftPad("" + this.repository.getSequenceByProductCode(productCode), 5, '0');
  }

  @Override
  @Transactional(readOnly = false)
  public String getSequenceTransaction(String productCode) {
    return StringUtils.leftPad(StringUtils.EMPTY + this.repository.getSequenceByProductCode(productCode), 5, '0');
  }

  @Override
  public boolean isUpcAvailable(String storeId, String upcCode, boolean activated)
      throws Exception {
    if (StringUtils.isEmpty(upcCode) || this.repository
        .findByStoreIdAndUpcCodeIgnoreCaseAndActivatedAndMarkForDeleteFalse(storeId, upcCode, activated)
        .isEmpty()) {
      return true;
    }
    return false;
  }

  @Override
  @Transactional(readOnly = false)
  @Deprecated
  public String save(ProductItem entity) throws Exception {
    return ServiceBeanHelper.saveEntity(entity, this.repository);
  }

  @Override
  @Transactional(readOnly = false)
  @Deprecated
  // TODO:evict
  public Product saveProductItemDetails(String storeId, List<ProductItem> productItemFromRequest, String productCode)
      throws Exception {
    Product parent = null;
    for (ProductItem source : productItemFromRequest) {
      ProductItem target =
          this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, source.getId());
      if (target == null) {
        throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
            "Can not find data with id : " + source.getId());
      }
      Hibernate.initialize(target.getProduct());
      List<ProductItemImage> tempProductItemImages = new ArrayList<ProductItemImage>();
      for (ProductItemImage newProductItemImage : source.getProductItemImages()) {
        boolean same = false;
        for (ProductItemImage oldProductItemImage : target.getProductItemImages()) {
          if (!oldProductItemImage.isMarkForDelete()
              && (oldProductItemImage.getId().equals(newProductItemImage.getId()))) {
            same = true;
            break;
          }
        }
        if (!same) {
          newProductItemImage.setProductItem(target);
          tempProductItemImages.add(newProductItemImage);
        }
      }
      for (ProductItemImage oldProductItemImage : target.getProductItemImages()) {
        ProductItemImage tempProductItemImage = null;
        boolean same = false;
        for (ProductItemImage newProductItemImage : source.getProductItemImages()) {
          if (oldProductItemImage.getId().equals(newProductItemImage.getId())) {
            tempProductItemImage = newProductItemImage;
            same = true;
            break;
          }
        }
        if (!same) {
          oldProductItemImage.setMarkForDelete(true);
        } else {
          oldProductItemImage.setLocationPath(tempProductItemImage.getLocationPath());
          oldProductItemImage.setMainImages(tempProductItemImage.isMainImages());
          oldProductItemImage.setMarkForDelete(tempProductItemImage.isMarkForDelete());
          oldProductItemImage.setSequence(tempProductItemImage.getSequence());
          oldProductItemImage.setHashCode(tempProductItemImage.getHashCode());
          oldProductItemImage.setActive(tempProductItemImage.isActive());
          oldProductItemImage.setOriginalImage(tempProductItemImage.getOriginalImage());
        }
      }
      if (!tempProductItemImages.isEmpty()) {
        target.getProductItemImages().addAll(tempProductItemImages);
      }
      BeanUtils.copyProperties(source, target, "generatedItemName", "product", "activated", "hash",
          "productItemAttributeValues", "productItemImages", "version", "createdDate", "createdBy", "sourceItemCode", "contentChanged");
      if(target.getSkuCode().startsWith(NEW_PRODUCT_CODE_PREFIX)) {
        int lastIndexOf = source.getSkuCode().lastIndexOf(HYPHEN);
        String newSku = null;
        for (int i = 0; i < target.getProduct().getProductItems().size(); i++) {
          String skuForOtherItems =
              target.getProduct().getProductItems().get(i).getSkuCode().substring(0, lastIndexOf);
          if (target.getId().equals(target.getProduct().getProductItems().get(i).getId())
              && !skuForOtherItems.equals(productCode)) {
            newSku = productCode + HYPHEN + this.getSequence(productCode);
            target.getProduct().getProductItems().get(i).setSkuCode(newSku);
            target.setSkuCode(newSku);
          } else if (!skuForOtherItems.equals(productCode)) {
            newSku = productCode + HYPHEN + this.getSequence(productCode);
            target.getProduct().getProductItems().get(i).setSkuCode(newSku);
          }
        }
      }
      parent = target.getProduct();
      ServiceBeanHelper.updateEntity(parent, this.productRepository);
    }
    return parent;
  }

  public void setApplicationCacheServiceBean(
      ApplicationCacheServiceBean applicationCacheServiceBean) {
    this.applicationCacheServiceBean = applicationCacheServiceBean;
  }

  @Override
  @Transactional(readOnly = false)
  @Deprecated
  public void update(ProductItem entity) throws Exception {
    ServiceBeanHelper.updateEntity(entity, this.repository);
  }

  @Override
  public Page<Object[]> findDuplicateProductItemsOrByUpcCode(String storeId, String upcCode,
      String productName, Pageable pageable) throws Exception {
    return this.repository
        .findByStoreIdAndGeneratedItemNameContainingIgnoreCaseAndUpcCodeIsNotNullOrUpcCodeAndMarkForDeleteFalse(
            storeId, productName, upcCode, pageable);
  }

  @Override
  public Page<ProductItem> findByUpcCodeExactMatch(String storeId, String upcCode, Pageable
      pageable) {
    Page<ProductItem> productItems = this.repository.findByStoreIdAndUpcCodeIgnoreCaseAndMarkForDeleteFalse(storeId,
        upcCode, pageable);
    productItems.getContent().forEach(productItem -> setProductCached(storeId, productItem));
    return productItems;
  }

  @Override
  public List<ProductItem> findByListOfProductCode(List<String> productCodes,
      Boolean isOnlyExternal, boolean active) {
    List<ProductItem> productItems = null;
    if (isOnlyExternal) {
      if (active) {
        productItems = this.repository.findByListProductCodeWithActivatedAndViewableTrueAndPromoSkuFalse(productCodes);
      } else {
        productItems = this.repository.findByListProductCodeWithPromoSkuFalseAndMarkForDeleteFalse(productCodes);
      }
    } else {
      if(active) {
        productItems = this.repository.findByListProductCodeWithActivatedTrueAndViewableTrue(productCodes);
      } else {
        productItems = this.repository.findByListProductCodeWithMarkForDeleteFalse(productCodes);
      }
    }
    for (ProductItem productItem : productItems) {
      setProductCached(productItem.getStoreId(), productItem);
      Hibernate.initialize(productItem.getProductItemImages());
    }
    return productItems;
  }

  @Override
  public Map<String, Long> getCategoryIdsWithProductCountForUPCCode(String upcCode,
      Boolean isOnlyExternal) {
    List<String> productIds = null;
    if (isOnlyExternal) {
      productIds = this.repository
          .findProductIdByUPCCodeWithActivatedAndViewableTrueAndReviewPendingFalseAndPromoSkuFalse(upcCode);
    } else {
      productIds = this.repository.findProductIdByUPCCodeWithActivatedTrueAndViewableTrueAndReviewPendingFalse(upcCode);
    }
    if (CollectionUtils.isEmpty(productIds)) {
      return new HashMap<>();
    }
    return findCategoryCountByProduct(productIds);
  }

  private Map<String, Long> findCategoryCountByProduct(List<String> productIds) {
    List<Object[]> result = productCategoryService.findCategoryCountByProductIdInAndMarkForDeleteFalse(productIds);
    if (CollectionUtils.isEmpty(result)) {
      return new HashMap<>();
    }
    Map<String, Long> categoryCountMap = new HashMap<>();
    for (Object[] object : result) {
      Long value = (Long) object[1];
      categoryCountMap.put((String) object[0], value);
    }
    return categoryCountMap;
  }

  @Override
  public List<String> getProductItemsIdsByUpcCodeAndCategoryIds(
      String upcCode, boolean isOnlyExternal, List<String> categoryIds) {
    if (isOnlyExternal) {
      return this.repository.findByUPCCodeWithActivatedAndViewableTrueAndReviewPendingFalseAndPromoSkuFalse(
          upcCode, categoryIds);
    }
    return repository.findByUPCCodeWithActivatedTrueAndViewableTrueAndReviewPendingFalse(
        upcCode, categoryIds);
  }

  @Override
  public Page<ProductItem> getProductItemsByStoreIdAndIds(
      String storeId, List<String> productItemIds, Pageable pageable) {
  return repository.findByStoreIdAndIdIn(storeId, productItemIds, pageable);
  }

  @Override
  public Page<ProductItem> findByStoreIdAndGeneratedItemNameAndCategoryId(String storeId,
      String generatedItemName, String categoryId, Pageable pageable) throws Exception {
    int solrRows = pageable.getPageSize();
    int solrStart = pageable.getPageNumber() * solrRows;
    List<ProductItem> productItemList = solrProductDao
        .findProductItemsWithCategoryIdAndGeneratedItemName(generatedItemName, categoryId,
            solrStart, solrRows);
    Page<ProductItem> page = new PageImpl<>(productItemList);
    return page;
  }

  @Override
  public Page<ProductItem> findListOfItemsByProduct(String storeId, Product product, Pageable pageable) {
    Page<ProductItem> productItems = this.repository
        .findByStoreIdAndProductAndActivatedTrueAndViewableTrueAndMarkForDeleteFalseOrderBySkuCode(storeId, product, pageable);
    productItems.getContent().forEach(productItem -> productItem.setProduct(product));
    return productItems;
  }

  @Override
  public ProductItem getProductItemByItemCode(String storeId, String skuCode) {
    ProductItem productItem = this.repository.findByStoreIdAndSkuCodeAndMarkForDeleteFalse(storeId, skuCode);
    if(Objects.isNull(productItem)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          ErrorMessage.ITEM_NOT_FOUND_FOR_SKU_CODE + skuCode);
    }
    return productItem;
  }

  private ProductItemService getProductItemService() {
    return applicationContext.getBean(ProductItemService.class);
  }

  @Override
  @Cacheable(value = CacheNames.PRODUCT_ITEMS_CACHE, key = "#storeId +'_'+ #productId", unless = "#result == null")
  public List<ProductItem> getProductItemsByStoreIdAndProductIdCached(String storeId, String productId) {
    LOG.debug("ProductItem cache missed for storeId: {}, productId: {}", storeId, productId);
    List<ProductItem> productItems = repository.findByStoreIdAndProductId(storeId, productId);
    List<ProductItem> clonedProductItems = new ArrayList<>();
    for (ProductItem productItem : productItems) {
      ProductItem clonedProductItem = new ProductItem();
      BeanUtils.copyProperties(productItem, clonedProductItem,
          "product", "productItemAttributeValues", "productItemImages");
      clonedProductItems.add(clonedProductItem);
    }
    return clonedProductItems;
  }

  @Override
  public List<ProductItem> getProductItemsByStoreIdAndProductId(String storeId, String productId) {
    return repository.findByStoreIdAndProductId(storeId, productId);
  }

  @Override
  public void setProductCached(String storeId, ProductItem productItem) {
    Product product = productService.getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(
        storeId, productItem.getProductId());
    productItem.setProduct(product);
  }

  @Override
  @Cacheable(value = CacheNames.PRODUCT_ITEM_ATTRIBUTE_VALUES_CACHE, key = "#storeId +'_'+ #product.id", unless = "#result == null")
  public List<ProductItemAttributeValue> getProductItemAttributeValuesWithAttributesCached(
      String storeId, Product product) {
    List<String> productItemIds =
        product.getProductItems().stream().map(ProductItem::getId).collect(Collectors.toList());
    List<ProductItemAttributeValue> productItemAttributeValues =
        productItemAttributeValueService.getDetachedProductItemAttributeValuesByStoreIdAndProductItemIds(
            storeId, productItemIds);
    productItemAttributeValues.removeIf(ProductItemAttributeValue::isMarkForDelete);
    return productItemAttributeValues;
  }

  @Override
  public void removeDeletedProductItemImages(ProductItem productItem) {
    int piCounter = 0;
    int originalImageCounter = 0;
    int processedImageCounter = 0;
    List<ProductItemImage> removedItemImages = new ArrayList<>();
    for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
      if(productItemImage.isMarkForDelete()) {
        removedItemImages.add(productItemImage);
      } else if (productItemImage.isMainImages() && Objects.isNull(productItemImage.getOriginalImage())) {
        if (piCounter > 0) {
          removedItemImages.add(productItemImage);
        } else {
          piCounter++;
        }
      } else if (productItemImage.isMainImages() && productItemImage.getOriginalImage()) {
        if (originalImageCounter > 0) {
          removedItemImages.add(productItemImage);
        } else {
          originalImageCounter++;
        }
      } else if (productItemImage.isMainImages() && !productItemImage.getOriginalImage()) {
        if (processedImageCounter > 0) {
          removedItemImages.remove(productItemImage);
        } else {
          processedImageCounter++;
        }
      }
    }
    productItem.getProductItemImages().removeAll(removedItemImages);
    if (piCounter == 0 && originalImageCounter == 0 && processedImageCounter == 0) {
      if (considerAnyImageAsMainImageInCaseOfMainImageNotPresent) {
        if (CollectionUtils.isNotEmpty(productItem.getProductItemImages())) {
          Integer sequence = productItem.getProductItemImages()
              .stream().filter(productItemImage -> Objects.nonNull(productItemImage.getSequence()))
              .findFirst().orElse(new ProductItemImage()).getSequence();
          if (Objects.nonNull(sequence)) {
            productItem.getProductItemImages().stream()
                .filter(itemImage -> sequence.equals(itemImage.getSequence()))
                .forEach(productItemImage -> productItemImage.setMainImages(true));
          }
        }
      } else {
      productItem.getProductItemImages().stream()
          .filter(productItemImage ->
              Objects.nonNull(productItemImage.getSequence()) && productItemImage.getSequence() == 0)
          .forEach(productItemImage -> productItemImage.setMainImages(true));
      }
    }
    if (uniqueMainImageAtItemLevel) {
      CommonUtil.updateMainImageToUniqueInCaseOfMultipleMainImages(productItem);
    }
  }

  @Override
  public void removeDeletedProductItemImagesWithoutFilteringMainImages(ProductItem productItem) {
    int piCounter = 0;
    int originalImageCounter = 0;
    int processedImageCounter = 0;
    List<ProductItemImage> removedItemImages = new ArrayList<>();
    for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
      if (productItemImage.isMarkForDelete()) {
        removedItemImages.add(productItemImage);
      } else if (productItemImage.isMainImages() && Objects.isNull(productItemImage.getOriginalImage())) {
        piCounter++;
      } else if (productItemImage.isMainImages() && productItemImage.getOriginalImage()) {
        originalImageCounter++;
      } else if (productItemImage.isMainImages()) {
        processedImageCounter++;
      }
    }
    productItem.getProductItemImages().removeAll(removedItemImages);
    if (piCounter == 0 && originalImageCounter == 0 && processedImageCounter == 0) {
      productItem.getProductItemImages().stream().filter(
              productItemImage -> Objects.nonNull(productItemImage.getSequence()) && productItemImage.getSequence() == 0)
          .forEach(productItemImage -> productItemImage.setMainImages(true));
    }
  }

  @Override
  public void removeDeletedAndSetMainImageFlagForProductItemImages(ProductItemDTO productItemDTO) {
    List<ProductItemImageDTO> productItemImageDTOS = productItemDTO.getProductItemImageDTOS();

    productItemImageDTOS.removeIf(ProductItemImageDTO::isMarkForDelete);

    long mainImages = productItemImageDTOS.stream().filter(
      productItemImage -> productItemImage.isMainImages() && Objects.isNull(
        productItemImage.getOriginalImage())).count();

    long originalImages = productItemImageDTOS.stream().filter(
      productItemImage -> productItemImage.isMainImages() && Boolean.TRUE.equals(
        productItemImage.getOriginalImage())).count();

    long processedImages = productItemImageDTOS.stream().filter(
      productItemImage -> productItemImage.isMainImages() && Boolean.FALSE.equals(
        productItemImage.getOriginalImage())).count();

    if (mainImages == 0 && originalImages == 0 && processedImages == 0) {
      productItemImageDTOS.stream().filter(
          productItemImage -> Objects.nonNull(productItemImage.getSequence())
            && productItemImage.getSequence() == 0)
        .forEach(productItemImage -> productItemImage.setMainImages(true));
    }
  }



  @Override
  @Cacheable(value = CacheNames.PRODUCT_ITEM_IMAGES_CACHE, key = "#storeId +'_'+ #product.id", unless = "#result == null")
  public List<ProductItemImage> getProductItemImagesCached(
      String storeId, Product product) {
    Set<String> productItemIds =
        product.getProductItems().stream().map(ProductItem::getId).collect(Collectors.toSet());
    Iterable<List<String>> partitionedProductItemIds = Iterables.partition(productItemIds,
        productItemImagesPartitionSize);
    List<ProductItemImage> productItemImages = new ArrayList<>();
    for (List<String> productItemIdsList : partitionedProductItemIds) {
      productItemImages.addAll(
          imageService.getDetachedProductItemImagesByStoreIdAndProductItemIds(storeId, productItemIdsList));
    }
    return productItemImages;
  }

  @Override
  @Transactional(readOnly = false)
  public List<ProductItem> updateProductItemUpcCode(String storeId,
      List<ProductItemUpcCodeUpdateRequest> productItemUpcCodeUpdateRequests, String productCode) throws Exception {
    List<ProductItem> updatedProductItems = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productItemUpcCodeUpdateRequests)) {
      for (ProductItemUpcCodeUpdateRequest request : productItemUpcCodeUpdateRequests) {
        ProductItem productItem = getProductItemByItemCode(storeId, request.getSkuCode());
        productItem.setUpcCode(request.getUpcCode());
        this.update(productItem);
        updatedProductItems.add(productItem);
      }
    }
    return updatedProductItems;
  }

  @Override
  public List<String> getItemNameByUPCCodeAndProductCode(String upcCode, String productCode, String skuCode) {
    return repository.findItemNameByWithUPCCodeAndProductCodeMarkForDeleteFalse(upcCode, productCode, skuCode);
  }

  @Override
  public Map<String, String> getProductItemIdsBySkuCode(String storeId, List<String> skuCodeList){
    if (CollectionUtils.isEmpty(skuCodeList) || StringUtils.isEmpty(storeId)) {
      return new HashMap<>();
    }
    List<ProductItem>  productItemList = this.repository.findByStoreIdAndSkuCodeIn(storeId, skuCodeList);
    return Optional.ofNullable(productItemList).orElse(new ArrayList<>()).stream().collect(
        Collectors.toMap(ProductItem::getSkuCode, ProductItem::getId));
  }

  @Override
  public List<ProductItem> findItemsBySkuCodesAndMarkForDeleteFalse(String storeId, List<String> skuCodes) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
        ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    if (CollectionUtils.isEmpty(skuCodes)) {
      return Collections.emptyList();
    }
    return this.repository.findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(storeId, skuCodes);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public ProductItem saveProductItem(ProductItem productItem) {
    return this.repository.save(productItem);
  }

  @Override
  public ProductItem getProductItemBySkuCode(String storeId, String skuCode) {
    ProductItem productItem = repository.findByStoreIdAndSkuCode(storeId, skuCode);
    if (Objects.nonNull(productItem)) {
      Hibernate.initialize(productItem.getProductItemImages());
      Hibernate.initialize(productItem.getProductItemAttributeValues());
      for (ProductItemAttributeValue productItemAttributeValue : productItem.getProductItemAttributeValues()) {
        Hibernate.initialize(productItemAttributeValue.getAttribute());
      }
      if (ranchIntegrationEnabled && Optional.ofNullable(distributionSellerList).orElse(new HashSet<>())
          .contains(productItem.getCreatedMerchant())) {
        Hibernate.initialize(productItem.getProductItemUomInfo());
      }
    }
    return productItem;
  }

  @Override
  public List<String> getItemCodeByUPCCodeAndProductCode(List<String> upcCodes, String productCode,
      List<String> skuCodes) {
    if (CollectionUtils.isEmpty(skuCodes)) {
      return repository.findItemCodeByUPCCodeAndProductCodeAndMarkForDeleteFalse(upcCodes, productCode);
    } else {
      return repository.findItemCodeByUPCCodeAndProductCodeAndSkuCodesInMarkForDeleteFalse(upcCodes, productCode,
          skuCodes);
    }
  }

  @Override
  public Map<String, String> getBySkuCodeByProductItemIds(String storeId, List<String> productItemIds) {
    if (CollectionUtils.isEmpty(productItemIds) || StringUtils.isEmpty(storeId)) {
      return new HashMap<>();
    }
    Page<ProductItem> productItems =
        this.repository.findByStoreIdAndIdIn(storeId, new ArrayList<>(new HashSet<>(productItemIds)),
            PageRequest.of(0, productItemIds.size()));
    return productItems.getContent().stream()
        .collect(Collectors.toMap(GdnBaseEntity::getId, ProductItem::getSkuCode, (a, b) -> b));
  }
}
