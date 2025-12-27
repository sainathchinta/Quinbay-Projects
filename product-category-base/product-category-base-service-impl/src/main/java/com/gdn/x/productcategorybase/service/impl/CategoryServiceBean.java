package com.gdn.x.productcategorybase.service.impl;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.Hibernate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnCommonUtil;
import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.CategoryChangeEventType;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.customrepository.CategoryDAO;
import com.gdn.x.productcategorybase.dto.CategoryCodeAndNameDTO;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationDTO;
import com.gdn.x.productcategorybase.dto.CategoryTreeDTO;
import com.gdn.x.productcategorybase.dto.CategoryTreeNodeDTO;
import com.gdn.x.productcategorybase.dto.CustomCategoryDto;
import com.gdn.x.productcategorybase.dto.mail.MailRecipientRequest;
import com.gdn.x.productcategorybase.dto.request.OriginalSalesCategoryRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeNodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.CategoryReference;
import com.gdn.x.productcategorybase.entity.CategoryShipping;
import com.gdn.x.productcategorybase.entity.OriginalSalesCategory;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.repository.CatalogRepository;
import com.gdn.x.productcategorybase.repository.CategoryAttributeRepository;
import com.gdn.x.productcategorybase.repository.CategoryConfigurationRepository;
import com.gdn.x.productcategorybase.repository.CategoryReferenceRepository;
import com.gdn.x.productcategorybase.repository.CategoryRepository;
import com.gdn.x.productcategorybase.repository.ProductCategoryRepository;
import com.gdn.x.productcategorybase.service.CatalogService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.CategoryShippingService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.OriginalSalesCategoryService;
import com.gdn.x.productcategorybase.service.mailservice.MailDeliveryService;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class CategoryServiceBean implements CategoryService {

  private static final Logger LOG = LoggerFactory.getLogger(CategoryServiceBean.class);

  private static final String CATEGORY_UPDATE_NOTIFICATION_TEMPLATE =
      "PCB_CATEGORY_UPDATE_NOTIFICATION_TEMPLATE";

  private static final String DATE_PATTERN = "dd/MM/yyyy";
  private static final String CATEGORY_TREE_ROOT = "ROOT";
  private static final int CATEGORY_TREE_PAGE_SIZE = 1000;
  private static final int CATEGORY_TREE_PAGE_NUMBER = 0;
  private static final int PAGE_SIZE = 100;

  @Autowired
  private CategoryAttributeRepository categoryAttributeRepository;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Autowired
  private CategoryReferenceRepository categoryReferenceRepository;

  @PersistenceContext
  private EntityManager entityManager;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  @Autowired
  private CategoryRepository categoryRepository;

  @Autowired
  private CatalogRepository catalogRepository;

  @Autowired
  private CategoryConfigurationRepository categoryConfigurationRepository;

  @Lazy
  @Autowired
  private CategoryShippingService categoryShippingServiceBean;

  @Autowired
  private CatalogService catalogService;

  @Autowired
  @Qualifier(value = "categoryDAO")
  private CategoryDAO categoryDAO;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Autowired
  private ApplicationContext applicationContext;

  @Autowired
  private OriginalSalesCategoryService oscService;

  @Autowired
  private CacheServiceHelperBean cacheServiceHelperBean;

  @Value("${category.update.mail.sender}")
  private String mailSender;

  @Value("${category.update.mail.to}")
  private String mailTo;

  @Value("${category.update.mail.cc}")
  private String mailCc;

  @Value("${caffeine.cache.enabled}")
  private boolean caffeineCacheEnabled;

  @Override
  @Transactional(readOnly = false)
  public void adjustCategory(Category oldCategory, Category newCategory, String
      parentCategoryIdOneLevelAbove, Integer savedInternalActivationInterval)
      throws Exception {
    List<CategoryChangeEventType> categoryChangeEventTypes = new ArrayList<>();
    for (CategoryAttribute oldCategoryAttribute : oldCategory.getCategoryAttributes()) {
      boolean same = false;
      for (CategoryAttribute newCategoryAttribute : newCategory.getCategoryAttributes()) {
        if (oldCategoryAttribute.getId().equals(newCategoryAttribute.getId())) {
          oldCategoryAttribute
              .setMainDefiningAttribute(newCategoryAttribute.isMainDefiningAttribute());
          oldCategoryAttribute.setSequence(newCategoryAttribute.getSequence());
          oldCategoryAttribute.setUSP(newCategoryAttribute.isUSP());
          same = true;
        }
      }

      if (!same) {
        oldCategoryAttribute.setMarkForDelete(true);
      }
    }

    for (CategoryAttribute newCategoryAttribute : newCategory.getCategoryAttributes()) {
      if (newCategoryAttribute.getId() == null) {
        newCategoryAttribute.setCategory(oldCategory);
        oldCategory.getCategoryAttributes().add(newCategoryAttribute);
      }
    }

    for (CategoryReference oldMasterCategoryReference : oldCategory.getMasterCategoryReferences()) {
      boolean same = false;
      for (CategoryReference newMasterCategoryReference : newCategory
          .getMasterCategoryReferences()) {
        if (oldMasterCategoryReference.getId().equals(newMasterCategoryReference.getId())) {
          same = true;
        } else if ((newMasterCategoryReference.getId() == null)
            && oldMasterCategoryReference.getMasterCategory().getId()
                .equals(newMasterCategoryReference.getMasterCategory().getId())) {
          oldMasterCategoryReference.setMarkForDelete(false);
          newMasterCategoryReference.setId(oldMasterCategoryReference.getId());
          same = true;
        }
      }
      if (!same) {
        oldMasterCategoryReference.setMarkForDelete(true);
      }
    }
    for (CategoryReference newMasterCategoryReference : newCategory.getMasterCategoryReferences()) {
      if (newMasterCategoryReference.getId() == null) {
        newMasterCategoryReference.setSalesCategory(oldCategory);
        oldCategory.getMasterCategoryReferences().add(newMasterCategoryReference);
      }
    }
    categoryChangeEventTypes.add(CategoryChangeEventType.DATA_CHANGE);
    ServiceBeanHelper.updateEntity(oldCategory, this.categoryRepository);
    sendEmailOnInternalActivationIntervalUpdate(newCategory, savedInternalActivationInterval);
    oldCategory = this.setParentCategoryId(parentCategoryIdOneLevelAbove, oldCategory);
    evictAllCategoryCache(oldCategory);
    this.domainEventPublisherService.publishCategory(oldCategory, categoryChangeEventTypes, new HashSet<>(), false);
  }

  @Override
  @Transactional(readOnly = false)
  public void saveUpdatedCategory(Category category, Integer previousActivationInterval, List<String> parentCategoryIds,
      List<CategoryChangeEventType> eventTypes) throws Exception {
    ServiceBeanHelper.updateEntity(category, this.categoryRepository);
    sendEmailOnInternalActivationIntervalUpdate(category, previousActivationInterval);
    evictAllCategoryCache(category);
    evictChildCategoryCache(parentCategoryIds);
    evictActiveChildCategoryCache(parentCategoryIds);
    this.domainEventPublisherService.publishCategory(category, eventTypes, new HashSet<>(), false);
  }

  @Override
  @Transactional(readOnly = false)
  public void saveUpdatedCategoryInfo(Category category, Integer previousActivationInterval, List<String> parentCategoryIds,
      List<CategoryChangeEventType> eventTypes, Set<String> categoryChangeEventTypesV2, List<String> masterCategoryIds,
      List<String> salesCategoryIds) throws Exception{
    ServiceBeanHelper.updateEntity(category, this.categoryRepository);
    sendEmailOnInternalActivationIntervalUpdate(category, previousActivationInterval);
    evictAllCategoryCacheAndMasterAndSalesCategoryDetails(category, masterCategoryIds, salesCategoryIds);
    evictChildCategoryCache(parentCategoryIds);
    evictActiveChildCategoryCache(parentCategoryIds);
    this.domainEventPublisherService.publishCategory(category, eventTypes, categoryChangeEventTypesV2, false);
  }

  @Override
  public void evictChildCategoryCache(List<String> parentCategoryIds) {
    if (CollectionUtils.isNotEmpty(parentCategoryIds)) {
      log.info("Evicting child cache for parent category Ids : {}", parentCategoryIds);
      parentCategoryIds.stream().forEach(parentCategoryId -> applicationCacheServiceBean
          .evictChildCategoryCacheByParentCategoryId(Constants.DEFAULT_STORE_ID, parentCategoryId));
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public String saveOSC(OriginalSalesCategoryRequest originalSalesCategoryRequest) throws Exception {
    log.info("Saving original sales category: {}", originalSalesCategoryRequest);
    OriginalSalesCategory originalSalesCategory = new OriginalSalesCategory();
    BeanUtils.copyProperties(originalSalesCategoryRequest, originalSalesCategory);
    if (StringUtils.isBlank(originalSalesCategory.getStoreId())) {
      originalSalesCategory.setStoreId(Constants.DEFAULT_STORE_ID);
    }
    return oscService.save(originalSalesCategory);
  }

  private void evictAllCategoryCacheWithOldCategoryDetails(Category oldCategory) throws Exception {
    getAllChildCategoryCodes(oldCategory).forEach(categoryCode -> applicationCacheServiceBean
        .evictCategoryHierarchyCacheByStoreIdAndCategoryCode(oldCategory.getStoreId(), categoryCode));
    applicationCacheServiceBean.evictCategoriesByCatalogTypeCache(oldCategory.getStoreId(), CatalogType.MASTER_CATALOG);
    applicationCacheServiceBean.evictCategoriesByCatalogTypeCache(oldCategory.getStoreId(), CatalogType.SALES_CATALOG);
    applicationCacheServiceBean
        .evictActiveCategoriesByCatalogIdCache(oldCategory.getStoreId(), CatalogType.MASTER_CATALOG);
    applicationCacheServiceBean
        .evictActiveCategoriesByCatalogIdCache(oldCategory.getStoreId(), CatalogType.SALES_CATALOG);
    applicationCacheServiceBean.evictCategoryDetailCache(oldCategory.getStoreId(), oldCategory.getId());
    applicationCacheServiceBean.evictCategoryCacheByStoreIdAndCategoryId(oldCategory.getStoreId(), oldCategory.getId());
    applicationCacheServiceBean
        .evictCategoryCacheByStoreIdAndCategoryId(oldCategory.getStoreId(), oldCategory.getCategoryCode());
  }

  private void evictAllCategoryCache(Category oldCategory) throws Exception {
    evictAllCategoryCacheWithOldCategoryDetails(oldCategory);
    for (CategoryReference mc : oldCategory.getMasterCategoryReferences()) {
      this.applicationCacheServiceBean.evictCategoryDetailCache(oldCategory.getStoreId(), mc.getMasterCategory().getId());
    }
    for (CategoryReference sc : oldCategory.getAllSalesCategoryReferences()) {
      this.applicationCacheServiceBean.evictCategoryDetailCache(oldCategory.getStoreId(), sc.getSalesCategory().getId());
    }
  }

  private void evictAllCategoryCacheAndMasterAndSalesCategoryDetails(Category oldCategory,
      List<String> masterCategoryIds, List<String> salesCategoryIds) throws Exception {
    evictAllCategoryCacheWithOldCategoryDetails(oldCategory);
    for (String masterCategoryId : Optional.ofNullable(masterCategoryIds).orElse(new ArrayList<>())) {
      this.applicationCacheServiceBean.evictCategoryDetailCache(oldCategory.getStoreId(), masterCategoryId);
    }
    for (String salesCategoryId : Optional.ofNullable(salesCategoryIds).orElse(new ArrayList<>())) {
      this.applicationCacheServiceBean.evictCategoryDetailCache(oldCategory.getStoreId(), salesCategoryId);
    }
  }

  @Override
  @CacheEvict(value = CacheNames.GENERIC_CATEGORY_TREE, key = "#storeId +'_'+ #genericTemplateEligible + '_' + #ignoreB2bExclusive")
  public void evictGenericCategoryTreeCache(String storeId, boolean genericTemplateEligible,
    boolean ignoreB2bExclusive) {
  }

  private List<String> getAllChildCategoryCodes(Category category) {
    List<String> childCategoryCodes = new ArrayList<>();
    childCategoryCodes.add(category.getCategoryCode());
    findChildCategories(category.getStoreId(), category).stream().map(Category::getCategoryCode)
        .collect(Collectors.toCollection(() -> childCategoryCodes));
    return childCategoryCodes;
  }

  private void sendEmailOnInternalActivationIntervalUpdate(Category newCategory,
      Integer savedInternalActivationInterval) {
    if (savedInternalActivationInterval != null && !savedInternalActivationInterval
        .equals(newCategory.getInternalActivationInterval())) {
      Map<String, Object> emailParameters = new HashMap<>();
      DateFormat dateFormat = new SimpleDateFormat(DATE_PATTERN);
      emailParameters.put("name", newCategory.getUpdatedBy());
      emailParameters.put("date", dateFormat.format(newCategory.getUpdatedDate()));
      emailParameters.put("categoryName", newCategory.getName());
      emailParameters.put("oldValue", savedInternalActivationInterval);
      emailParameters.put("newValue", newCategory.getInternalActivationInterval());
      MailRecipientRequest recipientRequest = new MailRecipientRequest(mailCc, mailTo);
      mailDeliveryService
          .sendMail(CATEGORY_UPDATE_NOTIFICATION_TEMPLATE, mailSender, null, emailParameters, "userName",
              newCategory.getUpdatedBy(), recipientRequest);
    }
  }

  @Override
  public Category findById(String id) throws Exception {
    return this.categoryRepository.findById(id).orElse(null);
  }

  @Override
  public Page<Category> findByName(String storeId, String name, Pageable pageable, String state,
      String documentFilterType) {
    return this.categoryRepository
        .findCategoryByStoreIdAndNameAndStateAndDocumentFilterType(storeId, name, state, documentFilterType, pageable);
  }

  @Override
  public Page<Category> findByStoreId(String storeId, Pageable pageable) throws Exception {
    return this.categoryRepository.findByStoreIdAndMarkForDeleteFalse(storeId, pageable);
  }

  @Override
  public List<CategoryShipping> findShippingInfoByStoreIdCategoryCode(String storeId, String categoryCode) {
    return this.categoryShippingServiceBean.findByCategoryCode(storeId, categoryCode);
  }

  @Override
  public Category findByStoreIdAndCategoryCode(String storeId, String categoryCode) {
    Category category = categoryRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
        storeId, categoryCode);
    GdnPreconditions.checkArgument(category != null,
        "not found category with code " + categoryCode);
    Hibernate.initialize(category.getCategoryAttributes());
    category.getCategoryAttributes().removeIf(CategoryAttribute::isMarkForDelete);
    Hibernate.initialize(category.getMasterCategoryReferences());
    Hibernate.initialize(category.getAllSalesCategoryReferences());
    return category;
  }

  @Override
  public Category findBasicInfoByStoreIdAndCategoryCode(String storeId, String categoryCode) throws Exception {
    Category category = categoryRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
        storeId, categoryCode);
    GdnPreconditions.checkArgument(Objects.nonNull(category),
        "not found category with code " + categoryCode);
    return category;
  }

  @Override
  public Category findByStoreIdAndId(String storeId, String id) {
    return this.categoryRepository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, id);
  }

  @Override
  public long findCountByParentCategoryAndHalalCategory(String storeId, Category parentCategory, boolean halalCategory) {
    return this.categoryRepository.countByStoreIdAndParentCategoryAndHalalCategoryAndMarkForDeleteFalse(storeId,
        parentCategory, halalCategory);
  }

  @Override
  @Cacheable(value = CacheNames.CATEGORY_DETAIL, key = "#storeId +'_'+ #id", unless = "#result == null")
  public Category findByStoreIdAndIdInitCategoryAttribute(String storeId, String id) {
    log.info("executing query: {}", id);
    Category category = categoryRepository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, id);
    log.info("test_response:: {}", category);
    if (Objects.nonNull(category)) {
      Hibernate.initialize(category.getCategoryAttributes());
      category.getCategoryAttributes().removeIf(CategoryAttribute::isMarkForDelete);
      Hibernate.initialize(category.getMasterCategoryReferences());
      Hibernate.initialize(category.getAllSalesCategoryReferences());
    } else {
      LOG.error("findByStoreIdAndIdInitCategoryAttribute not found category with id : {}", id);
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "not found category with id : " + id);
    }
    log.info("category_re: {}", category);
    return category;
  }

  @Override
  public List<Category> getCategoryAttributesByStoreIdAndCategoryCodes(String storeId,
      List<String> categoryCodes) {
    List<Category> categories =
        this.categoryRepository.findByStoreIdAndCategoryCodeInAndMarkForDeleteFalse(storeId,
            categoryCodes);
    for (Category category : categories) {
      Hibernate.initialize(category.getCategoryAttributes());
      List<CategoryAttribute> categoryAttributes = category.getCategoryAttributes().stream()
          .filter(Predicate.not(CategoryAttribute::isMarkForDelete)).collect(Collectors.toList());
      category.setCategoryAttributes(categoryAttributes);
      for (CategoryAttribute categoryAttribute : categoryAttributes) {
        Hibernate.initialize(categoryAttribute.getAttribute());
      }
    }
    return categories;
  }

  @Override
  public Category findByStoreIdAndIdInitAllCategoryAttribute(String storeId, String id) {
    Category category =
        this.categoryRepository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, id);
    if (Objects.nonNull(category)) {
      Hibernate.initialize(category.getCategoryAttributes());
      Hibernate.initialize(category.getMasterCategoryReferences());
      Hibernate.initialize(category.getAllSalesCategoryReferences());
    } else {
      LOG.error("findByStoreIdAndIdInitCategoryAttribute not found category with id : {}", id);
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "not found category with id : " + id);
    }
    return category;
  }

  private void findCategoryHierarchy(Category category, List<Category> categories)
      throws Exception {
    if (category.getParentCategory() != null) {
      category = category.getParentCategory();
      categories.add(category);
      findCategoryHierarchy(category, categories);
    }
  }

  @Override
  public List<Category> findCategoryHierarchyByCategoryCode(String storeId, String categoryCode) throws Exception {
    if (caffeineCacheEnabled) {
      return getCategoryService().findCategoryHierarchyByCategoryCodeFromCaffeine(storeId, categoryCode);
    }
    return getCategoryService().findCategoryHierarchyByCategoryCodeFromRedis(storeId, categoryCode);
  }

  @Override
  @Cacheable(cacheManager = Constants.CAFFEINE_CACHE_MANAGER, value =
      CacheNames.CATEGORY_HIERARCHY_CACHE, key = "#storeId +'_'+ #categoryCode", unless =
      "#result == null")
  public List<Category> findCategoryHierarchyByCategoryCodeFromCaffeine(String storeId,
      String categoryCode) throws Exception {
    return getCategoryService().findCategoryHierarchyByCategoryCodeFromRedis(storeId, categoryCode);
  }

  @Override
  @Cacheable(value = CacheNames.CATEGORY_HIERARCHY_CACHE, key = "#storeId +'_'+ #categoryCode", unless = "#result == null")
  public List<Category> findCategoryHierarchyByCategoryCodeFromRedis(String storeId,
      String categoryCode)
      throws Exception {
    List<Category> categories = new ArrayList<Category>();
    Category currentCategory =
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(storeId, categoryCode);
    if (currentCategory != null) {
      categories.add(currentCategory);
      findCategoryHierarchy(currentCategory, categories);
    }
    return categories;
  }

  @Override
  public List<Category> findCategoryHierarchyByCategoryId(String storeId, String categoryId) throws Exception {
    List<Category> categories = new ArrayList<Category>();
    Category currentCategory = getCategoryService().getCategoryByStoreIdAndIdCached(storeId, categoryId);
    if (Objects.nonNull(currentCategory) && !currentCategory.isMarkForDelete()) {
      categories.add(currentCategory);
      findCategoryHierarchy(currentCategory, categories);
    }
    return categories;
  }

  @Override
  public List<Category> findChildForParent(String storeId, Category parentCategory) {
    return this.categoryRepository.findByStoreIdAndParentCategoryAndMarkForDeleteFalse(storeId,
        parentCategory);
  }

  @Override
  public Page<Category> findChildForParent(String storeId, Category parentCategory,
      Pageable pageable) {
    return this.categoryRepository.findByStoreIdAndParentCategoryAndMarkForDeleteFalse(storeId,
        parentCategory, pageable);
  }

  @Override
  public long findActiveChildCountForParent(String storeId, Category parentCategory) {
    return this.categoryRepository.countByStoreIdAndParentCategoryAndActivatedTrueAndMarkForDeleteFalse(storeId,
        parentCategory);
  }

  @Override
  public long findInActiveChildCountForParent(String storeId, Category parentCategory) {
    return this.categoryRepository.countByStoreIdAndParentCategoryAndActivatedFalseAndMarkForDeleteFalse(storeId,
        parentCategory);
  }

  @Override
  public long findOverAllChildCountForParent(String storeId, Category parentCategory) {
    return this.categoryRepository.countByStoreIdAndParentCategoryAndMarkForDeleteFalse(storeId,
        parentCategory);
  }

  @Override
  public Page<Category> findChildForParentByCatalogId(String storeId, String parentCategoryId, String catalogId,
      String documentFilterType, boolean ignoreB2bExclusive, boolean filterHalalCategory, Pageable pageable) {
    return this.categoryRepository.findByStoreIdAndParentCategoryIdAndCatalogIdAndDocumentFilterTypeAndB2bExclusiveAndHalalCategoryAndMarkForDeleteFalse(
        storeId, parentCategoryId, catalogId, documentFilterType, ignoreB2bExclusive, filterHalalCategory, pageable);
  }

  @Override
  public List<Category> findChildForParentWithCatalogType(String storeId, Category parentCategory,
      CatalogType catalogType, Boolean activated) {
    if (activated != null)
      return this.categoryRepository
          .findByStoreIdAndParentCategoryAndCatalog_CatalogTypeAndActivatedAndMarkForDeleteFalse(
              storeId, parentCategory, catalogType, activated);
    else
      return this.categoryRepository
          .findByStoreIdAndParentCategoryAndCatalog_CatalogTypeAndMarkForDeleteFalse(storeId,
              parentCategory, catalogType);
  }

  @Override
  public Page<Category> findChildForParentWithCatalogType(String storeId, Category parentCategory,
      CatalogType catalogType, Boolean activated, Pageable pageable) {
    if (activated != null)
      return this.categoryRepository
          .findByStoreIdAndParentCategoryAndCatalog_CatalogTypeAndActivatedAndMarkForDeleteFalse(
              storeId, parentCategory, catalogType, activated, pageable);
    else
      return this.categoryRepository
          .findByStoreIdAndParentCategoryAndCatalog_CatalogTypeAndMarkForDeleteFalse(storeId,
              parentCategory, catalogType, pageable);
  }

  /**
   * use prefix 0 for product-code 1 for category-code 2 for attribute-code
   */
  @Override
  public String getSequence(String categoryCode) {
    return StringUtils.leftPad("" + this.categoryRepository.getSequenceByCategoryCode(categoryCode),
        6, '0');
  }

  @Override
  @Transactional(readOnly = false)
  public void markForDeleteCategory(String storeId, String categoryId) throws Exception {
    List<CategoryChangeEventType> categoryChangeEventTypes = new ArrayList<>();
    Category savedCategory = findByStoreIdAndId(storeId, categoryId);
    if (savedCategory == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Can not perform delete on un " + "exist data : " + categoryId);
    }
    savedCategory.setMarkForDelete(true);
    this.update(savedCategory);

    Hibernate.initialize(savedCategory.getProductCategories());
    Hibernate.initialize(savedCategory.getCategoryAttributes());
    Hibernate.initialize(savedCategory.getAllSalesCategoryReferences());
    this.markForDeleteProductCategory(storeId, savedCategory.getProductCategories());
    this.markForDeleteCategoryAttribute(savedCategory.getCategoryAttributes());
    if (CollectionUtils.isNotEmpty(savedCategory.getAllSalesCategoryReferences())) {
      this.markForDeleteCategoryReference(storeId, savedCategory.getAllSalesCategoryReferences());
    }
    if (savedCategory.getMasterCategoryReferences() != null) {
      this.markForDeleteCategoryReference(storeId, savedCategory.getMasterCategoryReferences());
    }
    evictAllCategoryCache(savedCategory);
    categoryChangeEventTypes.add(CategoryChangeEventType.DATA_CHANGE);
    this.domainEventPublisherService.publishCategory(savedCategory, categoryChangeEventTypes, new HashSet<>(), false);
  }

  @Override
  @Transactional(readOnly = false)
  public void markForDeleteCategoryAttribute(List<CategoryAttribute> categoryAttributes)
      throws Exception {
    for (CategoryAttribute categoryAttribute : categoryAttributes) {
      this.markForDeleteCategoryAttribute(categoryAttribute.getId());
    }
  }

  @Override
  @Transactional(readOnly = false)
  public void markForDeleteCategoryAttribute(String categoryAttributeId) throws Exception {
    CategoryAttribute savedCategoryAttribute =
        this.categoryAttributeRepository.findByIdAndMarkForDeleteFalse(categoryAttributeId);
    if (savedCategoryAttribute == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Can not perform delete on un " + "exist data : " + categoryAttributeId);
    }
    savedCategoryAttribute.setMarkForDelete(true);
    ServiceBeanHelper.updateEntity(savedCategoryAttribute, this.categoryAttributeRepository);
  }

  @Override
  @Transactional(readOnly = false)
  public void markForDeleteCategoryReference(String storeId,
      List<CategoryReference> categoryReferences) throws Exception {
    for (CategoryReference categoryReference : categoryReferences) {
      this.markForDeleteCategoryReference(storeId, categoryReference.getId());
    }
  }

  @Override
  @Transactional(readOnly = false)
  public void markForDeleteCategoryReference(String storeId, String categoryReferenceId)
      throws Exception {
    CategoryReference categoryReference = this.categoryReferenceRepository
        .findByStoreIdAndIdAndMarkForDeleteFalse(storeId, categoryReferenceId);
    if (categoryReference == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Can not perform delete on un " + "exist data : " + categoryReferenceId);
    }
    categoryReference.setMarkForDelete(true);
    ServiceBeanHelper.updateEntity(categoryReference, this.categoryReferenceRepository);
  }

  @Override
  @Transactional(readOnly = false)
  public void markForDeleteProductCategory(String storeId, List<ProductCategory> productCategories)
      throws Exception {
    for (ProductCategory productCategory : productCategories) {
      this.markForDeleteProductCategory(storeId, productCategory.getId());
    }
  }

  @Override
  @Transactional(readOnly = false)
  public void markForDeleteProductCategory(String storeId, String productCategoryId)
      throws Exception {
    ProductCategory savedProductCategory = this.productCategoryRepository
        .findByStoreIdAndIdAndMarkForDeleteFalse(storeId, productCategoryId);
    if (savedProductCategory == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Can not perform delete on un " + "exist data : " + productCategoryId);
    }
    savedProductCategory.setMarkForDelete(true);
    ServiceBeanHelper.updateEntity(savedProductCategory, this.productCategoryRepository);
    applicationCacheServiceBean.evictProductCategoriesCacheByStoreIdAndProductId(
        storeId, savedProductCategory.getProductId());
  }

  @Override
  @Transactional(readOnly = false)
  public Category save(Category entity) throws Exception {
    if (StringUtils.isEmpty(entity.getCategoryCode())) {
      String prefixCode = StringUtils.left(entity.getName(), 2).toUpperCase();
      entity.setCategoryCode(prefixCode + "-1" + this.getSequence(prefixCode));
    }
    Category category = categoryRepository.save(entity);
    applicationCacheServiceBean.evictCategoriesByCatalogTypeCache(entity.getStoreId(),
        CatalogType.MASTER_CATALOG);
    applicationCacheServiceBean.evictCategoriesByCatalogTypeCache(entity.getStoreId(),
        CatalogType.SALES_CATALOG);
    applicationCacheServiceBean.evictActiveCategoriesByCatalogIdCache(
        entity.getStoreId(), CatalogType.MASTER_CATALOG);
    applicationCacheServiceBean.evictActiveCategoriesByCatalogIdCache(
        entity.getStoreId(), CatalogType.SALES_CATALOG);
    applicationCacheServiceBean.evictCategoryCacheByStoreIdAndCategoryId(entity.getStoreId(), category.getId());
    applicationCacheServiceBean.evictCategoryCacheByStoreIdAndCategoryId(entity.getStoreId(), entity.getCategoryCode());
    return category;
  }

  @Override
  @Transactional(readOnly = false)
  public Category saveAndUpdateProductCategory(String storeId, Category category) throws Exception {
    if (StringUtils.isEmpty(category.getCategoryCode())) {
      String prefixCode = StringUtils.left(category.getName(), 2).toUpperCase();
      category.setCategoryCode(prefixCode + "-1" + this.getSequence(prefixCode));
    }
    String categoryId = ServiceBeanHelper.saveEntity(category, this.categoryRepository);
    if (Objects.nonNull(category.getCatalog()) && CatalogType.MASTER_CATALOG
        .equals(category.getCatalog().getCatalogType())) {
      for (CategoryReference masterCategoryReference : category.getMasterCategoryReferences()) {
        List<ProductCategory> productCategories =
            this.productCategoryRepository.findByStoreIdAndCategoryIdAndMarkForDeleteFalse(storeId,
                masterCategoryReference.getMasterCategory().getId());
        for (ProductCategory productCategory : productCategories) {
          Hibernate.initialize(productCategory.getProduct());
          ProductCategory newProductCategory =
              new ProductCategory(productCategory.getProduct(), category, storeId);
          ServiceBeanHelper.saveEntity(newProductCategory, this.productCategoryRepository);
          applicationCacheServiceBean.evictProductCategoriesCacheByStoreIdAndProductId(
              storeId, productCategory.getProductId());
        }
      }
    }
    applicationCacheServiceBean.evictCategoriesByCatalogTypeCache(storeId, CatalogType.MASTER_CATALOG);
    applicationCacheServiceBean.evictCategoriesByCatalogTypeCache(storeId, CatalogType.SALES_CATALOG);
    applicationCacheServiceBean.evictActiveCategoriesByCatalogIdCache(storeId, CatalogType.MASTER_CATALOG);
    applicationCacheServiceBean.evictActiveCategoriesByCatalogIdCache(storeId, CatalogType.SALES_CATALOG);
    return category;
  }

  @Override
  @Transactional(readOnly = false)
  public void setCategoryDisplayable(String categoryId, boolean isAble) throws Exception {
    List<CategoryChangeEventType> categoryChangeEventTypes = new ArrayList<>();
    Category category = getCategoryService().getCategoryByStoreIdAndIdCached(
        Constants.DEFAULT_STORE_ID, categoryId);
    category.setDisplay(isAble);
    categoryRepository.save(category);
    categoryChangeEventTypes.add(CategoryChangeEventType.DATA_CHANGE);
    evictAllCategoryCache(category);
    domainEventPublisherService.publishCategory(category, categoryChangeEventTypes, new HashSet<>(), false);
  }

  @Override
  public Map<String, String> getCategoryToFinalParent() {
    List<Object[]> categoriesList = this.categoryRepository.getCategoryAndParentCategories();
    Map<String, String> categoryToParent = new HashMap<>();
    for (Object product : categoriesList) {
      Object parentCategoryId = ((Object[]) product)[1];
      String parentCategoryIdStr = StringUtils.EMPTY;
      if (parentCategoryId != null) {
        parentCategoryIdStr = parentCategoryId.toString();
      }

      categoryToParent.put(((Object[]) product)[0].toString(), parentCategoryIdStr);
    }
    Map<String, String> categoryToFinalParentMap = new HashMap<>();

    for (String key : categoryToParent.keySet()) {
      categoryToFinalParentMap.put(key, getParent(key, categoryToParent));
    }

    return categoryToFinalParentMap;
  }

  private Category setParentCategoryId(String parentCategoryIdOneLevelAbove, Category oldCategory) {
    if (!StringUtils.isBlank(parentCategoryIdOneLevelAbove)) {
      this.categoryRepository.setParentCategoryId(parentCategoryIdOneLevelAbove,
          oldCategory.getId());
      Category category = new Category();
      category.setId(parentCategoryIdOneLevelAbove);
      oldCategory.setParentCategory(category);
    } else {
      oldCategory.setParentCategory(null);
    }

    return oldCategory;
  }

  @Override
  @Transactional(readOnly = false)
  public void update(Category entity) throws Exception {
    ServiceBeanHelper.updateEntity(entity, this.categoryRepository);
  }

  @Override
  public Page<Category> findByStoreIdAndCategoryCodes(String storeId, List<String> categoryCodes,
      Boolean activated, Pageable pageable) {
    Page<Category> result;
    if (activated != null)
      result =
          this.categoryRepository.findByStoreIdAndCategoryCodesAndActivatedAndMarkForDeleteFalse(
              storeId, categoryCodes, activated, pageable);
    else
      result = this.categoryRepository.findByStoreIdAndCategoryCodesAndMarkForDeleteFalse(storeId,
          categoryCodes, pageable);
    GdnPreconditions.checkArgument(result.getContent() != null && !result.getContent().isEmpty(),
        "not found category with codes : "
            + GdnCommonUtil.generateStringFromStringCollection(categoryCodes));
    return result;
  }

  @Override
  public List<Category> findByStoreIdAndCategoryCodes(String storeId, List<String> categoryCodes) {
    List<Category> result = null;
    if (CollectionUtils.isNotEmpty(categoryCodes)) {
      result = this.categoryRepository.findByStoreIdAndCategoryCodeInAndMarkForDeleteFalse(storeId, categoryCodes);
    }
    GdnPreconditions.checkArgument(Objects.nonNull(result) && CollectionUtils.isNotEmpty(result),
        ErrorMessage.CATEGORY_NOT_FOUND.getMessage() + categoryCodes);
    return result;
  }

  private String getParent(String key, Map<String, String> categoryToParentMap) {
    if (StringUtils.isBlank(categoryToParentMap.get(key))) {
      return key;
    } else {
      return getParent(categoryToParentMap.get(key), categoryToParentMap);
    }
  }

  @Override
  public Page<Category> findByCategoryNameAndCatalogType(String storeId, String categoryName,
      CatalogType catalogType, Pageable pageable) throws Exception {
    return this.categoryRepository.findByStoreIdAndCategoryNameAndCatalogTypeAndMarkForDeleteFalse(
        storeId, categoryName, catalogType, pageable);
  }

  @Override
  public String getFinalParentCategory(String categoryId) throws Exception {
    return this.categoryRepository.getFinalParentCategoryId(categoryId);
  }

  @Override
  public List<String> getParentCategoryHierarchyByCategoryId(String categoryId) throws Exception {
    return this.categoryRepository.getParentCategoryHierarchyByCategoryId(categoryId);
  }

  @Override
  public List<Category> findByStoreId(String storeId) {
    return this.categoryRepository.findByStoreIdAndMarkForDeleteFalse(storeId);
  }

  @Override
  @Cacheable(value = CacheNames.CATEGORIES_BY_CATALOGTYPE,
      key = "#storeId + '_' + #catalogType" + ".toString()", unless = "#result == null")
  public List<CustomCategoryDto> getCategoriesFromCatalogType(String storeId,
      CatalogType catalogType) throws Exception {
    return this.categoryDAO.getCategoriesFromCatalogType(storeId, catalogType);
  }

  @Override
  public List<String> getParentCategories() {
    List<String> parentCategories = this.categoryRepository.getParentCategories();
    if (parentCategories == null) {
      return new ArrayList<>();
    } else {
      return parentCategories;
    }
  }

  private void getStreamedAllCategoryTree(String catalogName, String storeId)
      throws Exception {
    String catalogId = getCatalogIdByName(storeId, catalogName);
    try(Stream<CategoryTreeDTO> catList =
        this.categoryRepository.getStreamedAllCategoryByCatalogId(storeId, catalogId)) {
      publishCategoriesEvent(catList,storeId);
    } catch (Exception e) {
      throw new Exception("Service error: " + e.getMessage());
    }
  }

  @Override
  public List<CategoryTreeDTO> getAllCategoryTree(String catalogName, String storeId, boolean ignoreB2bExclusive)
      throws Exception {
    try {
      String catalogId = getCatalogIdByName(storeId, catalogName);

      List<CategoryTreeDTO> catList =
          this.categoryRepository.getAllCategoryByCatalogName(storeId, catalogId);
      if (ignoreB2bExclusive) {
        catList = catList.stream().filter(Predicate.not(CategoryTreeDTO::isB2bExclusive)).collect(Collectors.toList());
      }
      return buildCategoryTree(catList, "");
    } catch (Exception e) {
      throw new Exception("Service error: " + e.getMessage());
    }
  }

  @Override
  public Boolean publishAllCategories(String catalogName, String storeId) throws Exception {

    getStreamedAllCategoryTree(catalogName, storeId);
    return Boolean.TRUE;
  }

  private void publishCategoriesEvent(Stream<CategoryTreeDTO> categoryTreeDTOS, String storeId) {
    categoryTreeDTOS.forEach(categoryTreeDTO -> {
      publishCategoryEvent(categoryTreeDTO.getCategoryCode(), storeId);
      if (categoryTreeDTO.getChildren() != null) {
        publishCategoriesEvent(categoryTreeDTO.getChildren().stream(), storeId);
      }
    });
  }

  private void publishCategoryEvent(String categoryCode, String storeId) {
    Category category = getCategoryService().getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
        storeId, categoryCode);
    try {
      this.domainEventPublisherService.publishAllCategory(category);
    } catch (Exception e) {
      LOG.error("error mapping publish event with errors:{}", e.getMessage(), e);
    }
  }

  @Override
  @Cacheable(value = CacheNames.ACTIVE_CATEGORY_TREE, key = "#storeId +'_'+ #catalogName", unless = "#result == null")
  public List<CategoryTreeDTO> getActiveCategoryTree(String catalogName, String storeId) {
    String catalogId = getCatalogIdByName(storeId, catalogName);
    return this.categoryRepository.getAllCategoryByCatalogId(storeId, catalogId);
  }

  private String getCatalogIdByName(String storeId, String catalogName) {
    if (StringUtils.isEmpty(catalogName)) {
      catalogName = CatalogType.MASTER_CATALOG.toString();
    }

    List<Catalog> catalogList = this.catalogRepository
        .findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(storeId, catalogName);
    return catalogList.get(0).getId();
  }

  @Override
  public List<CategoryTreeDTO> buildCategoryTree(List<CategoryTreeDTO> source,
      String parentCategoryId) {
    List<CategoryTreeDTO> catList = new ArrayList<>();

    for (CategoryTreeDTO data : source) {
      if ((data.getParentCategory().equals(parentCategoryId) && !data.getParentCategory().isEmpty())
          || (data.getParentCategory().isEmpty() && parentCategoryId.isEmpty())) {

        if (!isLastChild(source, data.getId())) {
          data.setChildren(buildCategoryTree(source, data.getId()));
        }
        catList.add(data);
      }
    }

    return catList;
  }

  private List<CategoryTreeDTO> buildCategoryTreeForGenericTemplate(List<CategoryTreeDTO> source,
      String parentCategoryId, boolean genericTemplateEligible) {
    List<CategoryTreeDTO> catList = new ArrayList<>();
    for (CategoryTreeDTO data : source) {
      if ((data.getParentCategory().equals(parentCategoryId) && !data.getParentCategory().isEmpty()) || (
          data.getParentCategory().isEmpty() && parentCategoryId.isEmpty())) {
        boolean isLastChild = isLastChild(source, data.getId());
        if (!isLastChild) {
          data.setChildren(buildCategoryTreeForGenericTemplate(source, data.getId(), genericTemplateEligible));
        }
        if ((data.isGenericTemplateEligible() == genericTemplateEligible && isLastChild) || (!isLastChild
            && CollectionUtils.isNotEmpty(data.getChildren()))) {
          catList.add(data);
        }
      }
    }
    return catList;
  }

  private List<CategoryTreeDTO> buildCategoryTreeForGenericTemplateAndBfbInclusive(List<CategoryTreeDTO> source,
    String parentCategoryId, boolean genericTemplateEligible) {
    List<CategoryTreeDTO> catList = new ArrayList<>();
    for (CategoryTreeDTO data : source) {
      if ((data.getParentCategory().equals(parentCategoryId) && !data.getParentCategory().isEmpty()) || (
        data.getParentCategory().isEmpty() && parentCategoryId.isEmpty())) {
        boolean isLastChild = isLastChild(source, data.getId());
        if (!isLastChild) {
          data.setChildren(buildCategoryTreeForGenericTemplateAndBfbInclusive(source, data.getId(), genericTemplateEligible));
        }
        if ((data.isGenericTemplateEligible() == genericTemplateEligible && !data.isB2bExclusive() && isLastChild) || (!isLastChild
          && CollectionUtils.isNotEmpty(data.getChildren()))) {
          catList.add(data);
        }
      }
    }
    return catList;
  }


  private Boolean isLastChild(List<CategoryTreeDTO> source, String categoryId) {
    int i = 0;
    for (CategoryTreeDTO data : source) {
      if (data.getParentCategory().equals(categoryId)) {
        i++;
      }
    }
    return i == 0;
  }

  @Override
  public List<CategoryTreeDTO> getCategoryTree(List<String> categoryCodes, String catalogName,
      String storeId) throws Exception {
    try {
      List<CategoryTreeDTO> categoryResponseList = new ArrayList<>();
      String catalogId = getCatalogIdByName(storeId, catalogName);

      List<Object[]> catList =
          this.categoryRepository.getCategoryByParentIdNative(storeId, catalogId, categoryCodes);
      for (Object[] data : catList) {
        CategoryTreeDTO cat = new CategoryTreeDTO(data[0].toString(), data[1].toString(),
            data[2].toString(), data[3] != null ? data[3].toString() : "");
        categoryResponseList.add(cat);
      }

      return buildCategoryTree(categoryResponseList, "");
    } catch (Exception e) {
      throw new Exception("Service error: " + e.getMessage());
    }
  }

  @Override
  public Page<Category> findCategorySummaryByStoreIdAndCatalogIdAndDisplay(String storeId,
      String catalogId, Boolean display, Pageable pageable) {
    return this.categoryRepository.findCategorySummaryByStoreIdAndCatalogIdAndDisplay(storeId,
        catalogId, display, pageable);
  }

  @Override
  public Map<String, String> findCategoryNamesByCategoryCodes(String storeId,
      List<String> categoryCodeList, Pageable pageable) throws Exception {
    Map<String, String> categoryMap = new HashMap<>();
    Page<Category> categoryList = this.categoryRepository
        .findByStoreIdAndCategoryCodesAndMarkForDeleteFalse(storeId, categoryCodeList, pageable);
    for (Category category : categoryList) {
      if (StringUtils.isNotEmpty(category.getName()))
        categoryMap.put(category.getCategoryCode(), category.getName());
    }
    return categoryMap;
  }

  @Override
  public List<String> findAllChildForC1CategoryCodes(String storeId, List<String> categoryCodes,
    boolean filterOutInactiveCn) throws Exception {
    Pageable pageable =  PageRequest.of(0, PAGE_SIZE);
    List<String> childCategoryCodes = new ArrayList<>();
    Page<Object[]> categoryPage;
    do {
      categoryPage = this.categoryRepository.findByStoreIdAndCategoryCodes(storeId, categoryCodes, pageable);
      if (CollectionUtils.isNotEmpty(categoryPage.getContent())) {
        for (Object[] record : categoryPage.getContent()) {
          List<String> partialChildCategoryCodes = filterOutInactiveCn ?
            cacheServiceHelperBean.findChildCategoriesActiveOnly(storeId, (String) record[1],
              (String) record[0]) :
            cacheServiceHelperBean.findChildCategoriesIncludingInactive(storeId, (String) record[1],
              (String) record[0]);
          childCategoryCodes.addAll(partialChildCategoryCodes);
        }
      }
      pageable = categoryPage.nextPageable();
    } while (categoryPage.hasNext());
    childCategoryCodes.removeAll(categoryCodes);
    return childCategoryCodes;
  }

  @Override
  public boolean validateIsCategoryCn(String storeId, String parentCategoryCode) throws Exception {
    List<Category> categoryList =
        this.categoryRepository.findByStoreIdAndParentIdAndMarkForDeleteFalse(storeId, parentCategoryCode);
    if (CollectionUtils.isEmpty(categoryList)) {
      return true;
    } else if (categoryList.stream().anyMatch(Category::isActivated)) {
      return false;
    }
    return true;
  }

  @Override
  public List<Category> findByCategoryIds(String storeId, List<String> categoryIds) {
    return categoryRepository.findByStoreIdAndIdInAndMarkForDeleteFalse(storeId, categoryIds);
  }

  @Override
  public List<Category> findAllParentCategories(String storeId) {
    return categoryRepository.findByStoreIdAndParentCategoryAndMarkForDeleteFalse(storeId, null);
  }

  @Override
  public List<CategoryTreeNodeResponse> getCategoryTree(String storeId) {
    List<CategoryConfigurationDTO> categoryConfigurationList =
        this.categoryConfigurationRepository.findAllCategoryConfigurationDTO(storeId);
    Map<String, String> categoryConfigurationMap = getCategoryConfigurationMap(categoryConfigurationList);
    List<Catalog> catalogList = this.catalogService.findByCatalogType(storeId, CatalogType.MASTER_CATALOG);
    List<CategoryTreeNodeResponse> result = new ArrayList<>();
    Map<String, List<CategoryTreeNodeResponse>> categoryChildrenMap = new HashMap<>();
    categoryChildrenMap.put(CATEGORY_TREE_ROOT, new ArrayList<>());
    Pageable pageable = PageRequest.of(CATEGORY_TREE_PAGE_NUMBER, CATEGORY_TREE_PAGE_SIZE);
    Page<CategoryTreeNodeDTO> categoryPage;
    do {
      categoryPage = this.categoryRepository
          .findByActivatedTrueAndMarkForDeleteFalseAndCatalogIdOrderByCategoryCode(catalogList.get(0).getId(), pageable);
      getCategoryConfigurationTree(categoryPage, categoryConfigurationMap, categoryChildrenMap);
      pageable = categoryPage.nextPageable();
    } while(categoryPage.hasNext());
    addChildren(CATEGORY_TREE_ROOT, categoryChildrenMap, result);
    return result;
  }

  private Map<String, String> getCategoryConfigurationMap(
      List<CategoryConfigurationDTO> categoryConfigurationDTOList) {
    return Optional.ofNullable(categoryConfigurationDTOList).orElse(new ArrayList<>()).stream()
        .collect(Collectors
            .toMap(categoryConfigurationDTO -> categoryConfigurationDTO.getCategoryId(),
                categoryConfigurationDTO -> categoryConfigurationDTO.getReviewConfig()));
  }

  private void getCategoryConfigurationTree(Page<CategoryTreeNodeDTO> categoryPage,
      Map<String, String> categoryConfigurationMap, Map<String, List<CategoryTreeNodeResponse>> categoryChildrenMap) {
    List<CategoryTreeNodeDTO> categoryTreeNodeDTOList = categoryPage.getContent();
    for(CategoryTreeNodeDTO categoryTreeNodeDTO : categoryTreeNodeDTOList) {
      String reviewConfig = Optional.ofNullable(categoryConfigurationMap.get(categoryTreeNodeDTO.getId()))
          .orElse(Constants.PRE_LIVE_STATUS);
      CategoryTreeNodeResponse categoryTreeNodeResponse;
      if(Constants.PRE_LIVE_STATUS.equals(reviewConfig)) {
        categoryTreeNodeResponse = new CategoryTreeNodeResponse(categoryTreeNodeDTO.getId(), categoryTreeNodeDTO.getName(),
            categoryTreeNodeDTO.getCategoryCode(), false);
      } else {
        categoryTreeNodeResponse = new CategoryTreeNodeResponse(categoryTreeNodeDTO.getId(), categoryTreeNodeDTO.getName(),
            categoryTreeNodeDTO.getCategoryCode(), true);
      }
      if (StringUtils.isEmpty(categoryTreeNodeDTO.getParentCategoryId())) {
        categoryChildrenMap.get(CATEGORY_TREE_ROOT).add(categoryTreeNodeResponse);
      } else {
        if(!categoryChildrenMap.containsKey(categoryTreeNodeDTO.getParentCategoryId())) {
          categoryChildrenMap.put(categoryTreeNodeDTO.getParentCategoryId(), new ArrayList<>());
        }
        categoryChildrenMap.get(categoryTreeNodeDTO.getParentCategoryId()).add(categoryTreeNodeResponse);
      }
    }
  }

  private void addChildren(String id, Map<String, List<CategoryTreeNodeResponse>> categoryTreeNodeMap,
      List<CategoryTreeNodeResponse> categoryTreeNodeResponseList) {
    categoryTreeNodeResponseList.addAll(categoryTreeNodeMap.get(id));
    for(CategoryTreeNodeResponse categoryTreeNodeResponse : categoryTreeNodeResponseList) {
      addChildrenToParentNode(categoryTreeNodeResponse, categoryTreeNodeMap);
      categoryTreeNodeResponse.setId(null);
    }
  }

  private void addChildrenToParentNode(CategoryTreeNodeResponse categoryTreeNodeResponse,
      Map<String, List<CategoryTreeNodeResponse>> categoryTreeNodeMap) {
    if(CollectionUtils.isNotEmpty(categoryTreeNodeMap.get(categoryTreeNodeResponse.getId()))) {
      categoryTreeNodeResponse.getChild().addAll(categoryTreeNodeMap.get(categoryTreeNodeResponse.getId()));
      for (CategoryTreeNodeResponse childCategory : categoryTreeNodeMap.get(categoryTreeNodeResponse.getId())) {
        addChildrenToParentNode(childCategory, categoryTreeNodeMap);
        childCategory.setId(null);
      }
    }
  }

  private CategoryService getCategoryService() {
    return applicationContext.getBean(CategoryService.class);
  }

  @Override
  @Transactional(readOnly = true)
  @Cacheable(value = CacheNames.CATEGORY_CACHE, key = "#storeId +'_'+ #categoryId", unless = "#result == null")
  public Category getCategoryByStoreIdAndIdCached(String storeId, String categoryId) {
    Category category = this.categoryRepository.findByStoreIdAndId(storeId, categoryId);
    GdnPreconditions.checkArgument(Objects.nonNull(category),
        ErrorMessage.CATEGORY_NOT_FOUND.getMessage() + categoryId);
    Category clonedCategory = new Category();
    BeanUtils.copyProperties(category, clonedCategory);
    return clonedCategory;
  }

  @Override
  @Transactional(readOnly = true)
  @Cacheable(value = CacheNames.CATEGORY_CACHE, key = "#storeId +'_'+ #categoryCode", unless = "#result == null")
  public Category getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(String storeId, String categoryCode) {
    Category category =
        this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(storeId, categoryCode);
    GdnPreconditions.checkArgument(Objects.nonNull(category),
        ErrorMessage.CATEGORY_NOT_FOUND.getMessage() + categoryCode);
    Category clonedCategory = new Category();
    BeanUtils.copyProperties(category, clonedCategory);
    return clonedCategory;
  }

  @Override
  public List<Category> findAllChildForC1CategoryCodesTree(String storeId, String categoryCode)
      throws Exception {
    List<Category> allCategories = new ArrayList<>();
    Category category =
        this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(storeId, categoryCode);
    allCategories.add(category);
    allCategories.addAll(findChildCategories(storeId, category));
    return allCategories;
  }

  @Override
  public List<String> findIdsByStoreIdAndCategoryCodes(String storeId, List<String> categoryCodes) {
    GdnPreconditions
        .checkArgument(CollectionUtils.isNotEmpty(categoryCodes), ErrorMessage.CATEGORY_MUST_NOT_BE_BLANK.getMessage());
    return categoryRepository.findIdsByStoreIdAndCategoryCodes(storeId, categoryCodes);
  }

  @Override
  @Cacheable(value = CacheNames.GENERIC_CATEGORY_TREE, key = "#storeId +'_'+ #genericTemplateEligible + '_' + #ignoreB2bExclusive", unless = "#result == null")
  public List<CategoryTreeResponse> getAllCategoryTreeforGenericTemplate(String storeId,
      boolean genericTemplateEligible, boolean ignoreB2bExclusive) throws Exception {
    List<CategoryTreeResponse> categoryTreeResponseList = new ArrayList<>();
    String catalogId = getCatalogIdByName(storeId, CatalogType.MASTER_CATALOG.name());
    List<CategoryTreeDTO> categoryTreeDTOList = this.categoryRepository.getAllCategoryByCatalogName(storeId, catalogId);
    if (ignoreB2bExclusive) {
      categoryTreeDTOList =
        buildCategoryTreeForGenericTemplateAndBfbInclusive(categoryTreeDTOList, StringUtils.EMPTY,
          genericTemplateEligible);
    } else {
      categoryTreeDTOList =
        buildCategoryTreeForGenericTemplate(categoryTreeDTOList, StringUtils.EMPTY, genericTemplateEligible);
    }

    for (CategoryTreeDTO data : categoryTreeDTOList) {
      CategoryTreeResponse categoryTreeResponse = new CategoryTreeResponse();
      BeanUtils.copyProperties(data, categoryTreeResponse);
      categoryTreeResponseList.add(categoryTreeResponse);
    }
    return categoryTreeResponseList;
  }

  private List<Category> findChildCategories(String storeId, Category category) {
    List<Category> allCategories = new ArrayList<>();
    List<Category> childCategories =
        this.categoryRepository.findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(storeId, category.getId());
    for (Category childCategory : childCategories) {
        allCategories.add(childCategory);
        allCategories.addAll(findChildCategories(storeId, childCategory));
    }
    return allCategories;
  }

  @Override
  public void evictActiveChildCategoryCache(List<String> parentCategoryIds) {
    if (CollectionUtils.isNotEmpty(parentCategoryIds)) {
      log.info("Evicting child cache for Active Categories with parent category Ids : {}",
        parentCategoryIds);
      parentCategoryIds.forEach(
        parentCategoryId -> applicationCacheServiceBean.evictActiveChildCategoryCacheByParentCategoryId(
          Constants.DEFAULT_STORE_ID, parentCategoryId));
    }
  }

  @Override
  public List<CategoryCodeAndNameDTO> findNameByStoreIdAndCategoryCodes(String storeId, List<String> categoryCodes) {
    return categoryRepository.findNameByStoreIdAndCategoryCodes(storeId, categoryCodes);
  }

  @Override
  public List<CategoryAttribute> getCategoryAttributes(String storeId, String categoryId) {
    List<CategoryAttribute> categoryAttributes =
        categoryAttributeRepository.findByStoreIdAndCategoryId(storeId, categoryId);
    for (CategoryAttribute categoryAttribute : categoryAttributes) {
      Hibernate.initialize(categoryAttribute.getAttribute());
    }
    return categoryAttributes;
  }

  @Override
  public List<Category> findCategoriesByStoreIdAndCategoryCodes(String storeId, List<String> categoryCodes) {
    return categoryRepository.findByStoreIdAndCategoryCodeInAndMarkForDeleteFalse(storeId, categoryCodes);
  }

  @Override
  public List<String> findCategoryCodesByAttributeCode(String storeId, String attributeCode) {
    return categoryRepository.getCategoryCodesByAttributeCode(storeId, attributeCode);
  }

  @Override
  @Async
  @Transactional(readOnly = false)
  public void updateB2bExclusiveOrHalalCategoryFlagForChildCategories(String storeId, Category category, boolean b2bExclusive,
      boolean halalCategory) throws Exception {
    List<Category> allChildCategories =
        new LinkedList<>(findAllChildForC1CategoryCodesTree(storeId, category.getCategoryCode()));
    allChildCategories.remove(category);
    if (CollectionUtils.isNotEmpty(allChildCategories)) {
      log.info("updating b2b exclusive flag for all the child's of category code : {}", category.getCategoryCode());
      for (Category childCategory : allChildCategories) {
        try {
          childCategory.setB2bExclusive(b2bExclusive);
          childCategory.setHalalCategory(halalCategory);
          categoryRepository.save(childCategory);
        } catch (Exception e) {
          log.error("Failed to update b2b exclusive flag for all the child's of category code : {} ",
              category.getCategoryCode(), e);
        }
      }
    }
  }

  @Override
  public List<CategoryAttribute> getCategoryAttributesMarkForDeleteFalse(String storeId,
    String categoryId) {
    return categoryAttributeRepository.findByStoreIdAndCategoryIdAndMarkForDeleteFalse(storeId, categoryId);
  }

}
