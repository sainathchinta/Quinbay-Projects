package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.hibernate.Hibernate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.AddProductAttributesDTO;
import com.gdn.x.productcategorybase.dto.CategorySummaryDTO;
import com.gdn.x.productcategorybase.dto.NewAttributeRequestDTO;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.repository.AttributeRepository;
import com.gdn.x.productcategorybase.repository.CategoryRepository;
import com.gdn.x.productcategorybase.repository.PredefinedAllowedAttributeValueRepository;
import com.gdn.x.productcategorybase.repository.ProductAttributeRepository;
import com.gdn.x.productcategorybase.repository.ProductAttributeValueRepository;
import com.gdn.x.productcategorybase.repository.ProductCategoryRepository;
import com.gdn.x.productcategorybase.repository.ProductRepository;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.ProductCategoryService;
import com.gdn.x.productcategorybase.service.ProductService;

@Service
@Transactional(readOnly = true)
public class ProductCategoryServiceBean implements ProductCategoryService {

  private static final String MASTER_CATALOG_CODE = "10001";

  private static final Logger LOG = LoggerFactory.getLogger(ProductCategoryServiceBean.class);

  @Autowired
  private ProductCategoryRepository repository;
  @Autowired
  private ProductRepository productRepository;
  @Autowired
  private CategoryRepository categoryRepostory;
  @Autowired
  private ProductCategoryRepository prdCategoryRepository;
  @Autowired
  private ProductAttributeRepository prdAttributeRepository;
  @Autowired
  private ProductAttributeValueRepository prdAttrValueRepository;
  @Autowired
  private AttributeRepository attrRepository;
  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;
  @Autowired
  private PredefinedAllowedAttributeValueRepository predefAllowedAttrRepository;

  @Lazy
  @Autowired
  private ProductService productService;
  @Autowired
  private CategoryService categoryService;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Override
  public void delete(String id) throws Exception {
    // do nuffin
  }

  @Override
  public ProductCategory findById(String id) throws Exception {
    return this.repository.findById(id).orElse(null);
  }

  @Override
  public Page<ProductCategory> findByStoreId(String storeId, Pageable pageable) throws Exception {
    return this.repository.findByStoreIdAndMarkForDeleteFalse(storeId, pageable);
  }

  @Override
  public ProductCategory findByStoreIdAndId(String storeId, String id) {
    return this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, id);
  }

  @Override
  public String save(ProductCategory entity) throws Exception {
    return ServiceBeanHelper.saveEntity(entity, this.repository);
  }

  @Override
  public void update(ProductCategory entity) throws Exception {
    ServiceBeanHelper.updateEntity(entity, this.repository);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public CategorySummaryDTO movePrdCategoryByProductCode(String storeId, String productCode, String categoryCode)
      throws Exception {
    Product product = productService.getProductByStoreIdAndProductCodeCached(storeId, productCode);
    this.checkProductData(product, productCode, storeId);
    productService.setProductCategoriesWithCategoriesCached(storeId, product, false);

    Category category = categoryRepostory.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(storeId, categoryCode);
    if (category == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Category " + categoryCode + " with storeId " + storeId + " is not found");
    }

    List<ProductCategory> prdCategoryList =
        prdCategoryRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId, product.getId());
    if (CollectionUtils.isEmpty(prdCategoryList)) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Product Category with product code " + productCode + " with storeId " + storeId + " is not found");
    }
    for (ProductCategory data : prdCategoryList) {
      data.setCategory(category);
    }

    prdCategoryRepository.saveAll(prdCategoryList);
    domainEventPublisherService.publishProductChangeCategory(product, null, false,
        false, true, false, new HashSet<>());
    applicationCacheServiceBean.evictProductCategoriesCacheByStoreIdAndProductId(storeId, product.getId());
    CategorySummaryDTO response = new CategorySummaryDTO();
    response.setCategoryCode(category.getCategoryCode());
    response.setCategoryName(category.getName());
    return response;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public List<ProductAttribute> addProductAttributes(String storeId, AddProductAttributesDTO request) throws Exception {
    Product product = productRepository.findByStoreIdAndProductCode(storeId, request.getProductCode());
    this.checkProductData(product, request.getProductCode(), storeId);
    Hibernate.initialize(product.getProductAttributes());

    List<ProductAttribute> result = new ArrayList<>();
    for (NewAttributeRequestDTO attrReq : request.getNewAttributes()) {
      this.validateAddPrdAttrRequest(product, attrReq);
      Attribute attr = attrRepository.findByStoreIdAndAttributeCode(storeId, attrReq.getAttributeCode());
      if (attr == null) {
        throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "Attribute not found for attribute code: " + attrReq.getAttributeCode());
      }

      ProductAttribute prdAttr = new ProductAttribute();
      prdAttr.setProduct(product);
      prdAttr.setOwnByProductItem(request.isOwnByProductItem());
      prdAttr.setSequence(request.getSequence());
      prdAttr.setAttribute(attr);
      prdAttr.setProductAttributeName(attr.getName());
      prdAttributeRepository.save(prdAttr);

      ProductAttributeValue prdAttrValue = new ProductAttributeValue();
      prdAttrValue.setProductAttribute(prdAttr);
      prdAttrValue.setStoreId(storeId);
      if (attr.getAttributeType() == AttributeType.DESCRIPTIVE_ATTRIBUTE) {
        prdAttrValue.setDescriptiveAttributeValue(attrReq.getAttributeValue());
        prdAttrValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
      } else if (attr.getAttributeType() == AttributeType.PREDEFINED_ATTRIBUTE) {
        PredefinedAllowedAttributeValue predefAllowedValue =
            predefAllowedAttrRepository.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(storeId, attr, attrReq.getAttributeValue());
        if (predefAllowedValue == null) {
          throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "Predefine allowed value is not found for attribute code: " + attr.getAttributeCode()
              + ", allowed value: " + attrReq.getAttributeValue() + ", and storeId: " + storeId);
        }
        prdAttrValue.setPredefinedAllowedAttributeValue(predefAllowedValue);
        prdAttrValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
      }
      ProductAttributeValue savedPrdValue = prdAttrValueRepository.save(prdAttrValue);
      prdAttr.setProductAttributeValues(Arrays.asList(savedPrdValue));
      result.add(prdAttr);
    }
    applicationCacheServiceBean.evictProductAttributesCacheByStoreIdAndProductId(storeId, product.getId());
    return result;
  }

  private void validateAddPrdAttrRequest(Product product, NewAttributeRequestDTO attrReq) throws Exception {
    for (ProductAttribute prdAttr : product.getProductAttributes()) {
      Hibernate.initialize(prdAttr.getAttribute());
      if (prdAttr.getAttribute().getAttributeCode().equals(attrReq.getAttributeCode())) {
        throw new ApplicationException(ErrorCategory.INVALID_STATE, "Product's Attribute: " +
            prdAttr.getAttribute().getName() + " already exist");
      }
    }
  }

  private void checkProductData(Product product, String productCode, String storeId) throws Exception {
    if (product == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Product " + productCode + " with storeId " + storeId + " is not found");
    }
  }

  @Override
  public List<Category> getMasterParentCategoryByProductCode(String storeId, String productCode) {
    Product product = productService.getProductByStoreIdAndProductCodeCached(storeId, productCode);
    GdnPreconditions.checkArgument(Objects.nonNull(product), "Product doesn't exist");
    productService.setProductCategoriesWithCategoriesCached(storeId, product, false);
    List<Category> categoryResponses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(product.getProductCategories())) {
      categoryResponses = product.getProductCategories().stream().filter(this::isMasterProductCategory)
          .map(category -> getParentCategoryByCategoryCode(storeId, category.getCategory().getCategoryCode()))
          .collect(Collectors.toList());
    }
    return categoryResponses;
  }

  /**
   * productCategory is from MasterCatalog or not
   *
   * @param productCategory
   * @return
   */
  private boolean isMasterProductCategory(ProductCategory productCategory) {
    return !productCategory.isMarkForDelete() && Objects
        .nonNull(productCategory.getCategory()) && MASTER_CATALOG_CODE
        .equals(productCategory.getCategory().getCatalog().getCatalogCode());
  }

  @Override
  public Category getParentCategoryByCategoryCode(String storeId, String categoryCode) {
    try {
      List<Category> categories =
          this.categoryService.findCategoryHierarchyByCategoryCode(storeId, categoryCode);
      return categories.get(categories.size() - 1);
    } catch (Exception e) {
      LOG.error("Error while getting CategoryHierarchy for categoryCode : {}", categoryCode, e);
    }
    return null;
  }

  @Override
  public List<Object[]> findCategoryCountByProductIdInAndMarkForDeleteFalse(List<String> productIds) {
    return this.repository.findCategoryCountByProductIdInAndMarkForDeleteFalse(productIds);
  }

  @Override
  public List<String> findByProductIdInAndCategoryIdInAndMarkForDeleteFalse(List<String> productIds,
                                                                            List<String> categoryIds) {
    return this.repository.findByProductIdInAndCategoryIdInAndMarkForDeleteFalse(productIds, categoryIds);
  }

  @Override
  @Transactional(readOnly = true)
  @Cacheable(value = CacheNames.PRODUCT_CATEGORIES_CACHE, key = "#storeId +'_'+ #productId", unless = "#result == null")
  public List<ProductCategory> getProductCategoriesByStoreIdAndProductIdCached(String storeId, String productId) {
    LOG.debug("ProductCategory cache missed for storeId: {}, productId: {}", storeId, productId);
    List<ProductCategory> productCategories = repository.findByStoreIdAndProductId(storeId, productId);
    List<ProductCategory> clonedProductCategories = new ArrayList<>();
    for (ProductCategory productCategory : productCategories) {
      ProductCategory cloneProductCategory = new ProductCategory();
      BeanUtils.copyProperties(productCategory, cloneProductCategory, "product", "category");
      clonedProductCategories.add(cloneProductCategory);
    }
    return clonedProductCategories;
  }

  @Override
  public void deleteByProductCategoryIds(List<String> productCategoryIds) {
    repository.deleteAllById(productCategoryIds);
  }
}
