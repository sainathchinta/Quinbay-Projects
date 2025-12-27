package com.gdn.x.productcategorybase.service.impl;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.domain.event.model.CategoryHistoryEventModel;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.x.productcategorybase.config.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.dto.CategoryUpdateHistoryDTO;
import com.gdn.x.productcategorybase.dto.CategoryAttributeMappingUpdateHistoryEventDTO;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import com.gdn.x.productcategorybase.enums.CategoryUpdateHistoryActivity;
import com.gdn.x.productcategorybase.util.CommonUtil;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.CategoryChangeEventType;
import com.gdn.x.productcategorybase.CategoryErrorResponse;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.config.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.domain.event.model.CategoryHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import com.gdn.x.productcategorybase.dto.CategoryAndHierarchyDto;
import com.gdn.x.productcategorybase.dto.CategoryAttributeMappingUpdateHistoryEventDTO;
import com.gdn.x.productcategorybase.dto.CategoryAttributeUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryCodeAndNameDTO;
import com.gdn.x.productcategorybase.dto.CategoryDetailDTO;
import com.gdn.x.productcategorybase.dto.CategoryErrorDto;
import com.gdn.x.productcategorybase.dto.CategoryInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateListDTO;
import com.gdn.x.productcategorybase.dto.CategoryMappingsUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryServiceDTO;
import com.gdn.x.productcategorybase.dto.CategoryTreeDTO;
import com.gdn.x.productcategorybase.dto.CategoryUpdateHistoryDTO;
import com.gdn.x.productcategorybase.dto.WholesaleMappingDTO;
import com.gdn.x.productcategorybase.dto.request.CategoryRestrictedKeywordsRequest;
import com.gdn.x.productcategorybase.dto.request.ShippingRequest;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsMappedToCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.CategoryReference;
import com.gdn.x.productcategorybase.entity.CategoryRestrictedKeyword;
import com.gdn.x.productcategorybase.entity.CategoryShipping;
import com.gdn.x.productcategorybase.entity.OriginalSalesCategory;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;
import com.gdn.x.productcategorybase.entity.WholesalePriceConfiguration;
import com.gdn.x.productcategorybase.enums.CategoryUpdateHistoryActivity;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.CatalogService;
import com.gdn.x.productcategorybase.service.CategoryReferenceService;
import com.gdn.x.productcategorybase.service.CategoryRestrictedKeywordService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.CategoryServiceWrapper;
import com.gdn.x.productcategorybase.service.CategoryShippingService;
import com.gdn.x.productcategorybase.service.CategoryWholesaleConfigService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.OriginalSalesCategoryService;
import com.gdn.x.productcategorybase.service.RestrictedKeywordService;
import com.gdn.x.productcategorybase.util.CommonUtil;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class CategoryServiceWrapperImpl implements CategoryServiceWrapper {

  private static final String WARNA = "Warna";
  private static final String UKURAN = "Ukuran";
  private static final String VARIASI = "Variasi";
  private static final String BRAND = "Brand";

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private CategoryShippingService categoryShippingService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private AttributeService attributeService;

  @Autowired
  private CatalogService catalogService;

  @Autowired
  private RestrictedKeywordService restrictedKeywordService;

  @Autowired
  private CategoryRestrictedKeywordService categoryRestrictedKeywordService;

  @Autowired
  private CategoryWholesaleConfigService categoryWholesaleConfigService;

  @Autowired
  private OriginalSalesCategoryService oscService;

  @Autowired
  private CategoryReferenceService categoryReferenceService;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private ApplicationContext applicationContext;

  @Value("${default.internal.activation.period}")
  private int defaultInternalActivationPeriod;

  @Value("${add.hierarchy.change.type.in.category.change.event}")
  private boolean addHierarchyChangeTypeInCategoryChangeEvent;

  @Value("${mark.restricted.keyword.enable}")
  private boolean markRestrictedKeywordEnable;

  @Value("${remove.deleted.keywords.from.add.list}")
  private boolean removeDeletedKeywordsFromAddList;

  @Value("${category.history.enable}")
  private boolean categoryHistoryEnable;

  @Value("${osc.change.check.switch}")
  private boolean oscChangeCheckSwitch;

  @Value("${restricted.keyword.list.change}")
  private boolean restrictedKeywordListChange;

  private static final String CATEGORY_NOT_FOUND = "Category not found with id: ";
  private static final String PARENT_CATEGORY_NOT_FOUND = "Parent category not found with id: ";
  private static final String PARENT_CATEGORY_NOT_VALID = "Parent category not valid";
  private static final String ATTRIBUTES_NOT_FOUND = "Attributes not found with ids: ";
  private static final String CATEGORIES_NOT_FOUND = "Categories not found with ids: ";
  private static final String KEYWORDS_NOT_FOUND = "Keywords not found with ids: ";
  private static final String CATEGORY_NOT_CREATED = "Category not created";
  private static final String IN_ACTIVE = "INACTIVE";
  private static final String ACTIVE = "ACTIVE";
  private static final String ALL = "ALL";

  private CategoryServiceWrapper getCategoryServiceWrapper() {
    return applicationContext.getBean(CategoryServiceWrapper.class);
  }

  @Override
  @Transactional(readOnly = false)
  public List<CategoryHistoryEventModel> updateCategoryInfo(String storeId,
    CategoryInfoUpdateDTO categoryInfoUpdateDTO, boolean statusChangeFlag) throws Exception {
    List<CategoryHistoryEventModel> categoryHistoryEventModelList = new ArrayList<>();
    String categoryCode = categoryInfoUpdateDTO.getCategoryCode();
    String username = GdnMandatoryParameterUtil.getUsername();
    boolean isParentCategoryChanged = false;
    boolean nameChanged = false;
    boolean genericTemplateEligibleChanged = false;
    List<CategoryChangeEventType> categoryChangeEventTypes = new ArrayList<>();
    Set<String> categoryChangeEventTypesV2 = new HashSet<>();
    if(categoryInfoUpdateDTO.getId().equals(categoryInfoUpdateDTO.getParentCategoryId())){
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, PARENT_CATEGORY_NOT_VALID);
    }

    Category savedCategory =
        categoryService.findByStoreIdAndId(storeId, categoryInfoUpdateDTO.getId());

    populateDisplayAndNameChange(savedCategory, categoryInfoUpdateDTO, categoryChangeEventTypesV2,
      statusChangeFlag);

    populateCategoryHistoryChange(savedCategory, categoryInfoUpdateDTO,
      categoryHistoryEventModelList, categoryCode, storeId, username, statusChangeFlag);

    populateHistoryForWholeSaleConfigToggle(savedCategory, categoryInfoUpdateDTO,
        categoryHistoryEventModelList, categoryCode, storeId, username);

    List<String> masterCategoryIds = new ArrayList<>();
    List<String> salesCategoryIds = new ArrayList<>();

    if (CatalogType.MASTER_CATALOG.equals(savedCategory.getCatalog().getCatalogType())) {
      checkAndUpdateOSC(storeId, categoryInfoUpdateDTO, categoryChangeEventTypes, savedCategory,
        username, categoryHistoryEventModelList, categoryChangeEventTypesV2, oscChangeCheckSwitch);
      salesCategoryIds = categoryReferenceService.getSalesCategoryIdByMasterCategoryId(storeId, savedCategory.getId());
    }else {
      masterCategoryIds = categoryReferenceService.getMasterCategoryIdBySalesCategoryId(storeId, savedCategory.getId());
    }

    List<String> parentCategoriesIds = new ArrayList<>();
    String newCategoryParentId = categoryInfoUpdateDTO.getParentCategoryId();
    if (!((Objects.isNull(savedCategory.getParentCategory()) && StringUtils.isBlank(newCategoryParentId)) || (
        Objects.nonNull(savedCategory.getParentCategory()) && savedCategory.getParentCategory().getId()
            .equals(newCategoryParentId)))) {
      String previousParentCategory = fetchParentCategoryCode(savedCategory);
      isParentCategoryChanged = true;
      parentCategoriesIds = setParentCategoryByCategoryInfoUpdateDTO(storeId, newCategoryParentId, savedCategory);
      String newParentCategory = fetchParentCategoryCode(savedCategory);
      if (addHierarchyChangeTypeInCategoryChangeEvent) {
        categoryChangeEventTypes.add(CategoryChangeEventType.CATEGORY_HIERARCHY_CHANGE);
      }
      categoryChangeEventTypesV2.add(CategoryChangeEventType.CATEGORY_HIERARCHY_CHANGE.name());
      populateParentCategoryChangeHistory(savedCategory.getCategoryCode(), previousParentCategory,
        newParentCategory, username, storeId, categoryHistoryEventModelList);
    }

    Integer savedInternalActivationInterval = savedCategory.getInternalActivationInterval();
    setInternalActivationPeriodByCategoryInfoUpdateDTO(categoryInfoUpdateDTO, savedCategory);

    if ((StringUtils.isNotBlank(categoryInfoUpdateDTO.getName()) && !categoryInfoUpdateDTO.getName()
        .equals(savedCategory.getName())) || (StringUtils.isNotBlank(categoryInfoUpdateDTO.getNameEnglish())
        && !categoryInfoUpdateDTO.getNameEnglish().equals(savedCategory.getNameEnglish()))) {
      savedCategory.setName(categoryInfoUpdateDTO.getName());
      savedCategory.setNameEnglish(categoryInfoUpdateDTO.getNameEnglish());
      nameChanged = true;
      categoryChangeEventTypes.add(CategoryChangeEventType.CATEGORY_NAME_CHANGE);
    }
    savedCategory.setSequence(categoryInfoUpdateDTO.getSequence());
    savedCategory.setDefaultDescription(categoryInfoUpdateDTO.getDefaultDescription());
    savedCategory.setDescriptionEnglish(categoryInfoUpdateDTO.getDescriptionEnglish());
    savedCategory.setDisplay(categoryInfoUpdateDTO.isDisplay());
    savedCategory.setLogisticAdjustment(categoryInfoUpdateDTO.getLogisticAdjustment());
    savedCategory.setWarranty(categoryInfoUpdateDTO.isWarranty());
    savedCategory.setNeedIdentity(categoryInfoUpdateDTO.isNeedIdentity());
    savedCategory.setUpdatedBy(categoryInfoUpdateDTO.getUpdatedBy());
    savedCategory.setUpdatedDate(categoryInfoUpdateDTO.getUpdatedDate());
    savedCategory.setDangerousGoodsLevel(categoryInfoUpdateDTO.getDangerousGoodsLevel());
    savedCategory.setUmkm(categoryInfoUpdateDTO.isUmkm());
    savedCategory.setDocumentType(categoryInfoUpdateDTO.getDocumentType());
    savedCategory.setBopisEligible(categoryInfoUpdateDTO.isBopisEligible());
    boolean b2bExclusivePreviousValue = savedCategory.isB2bExclusive();
    boolean halalCategoryPreviousValue = savedCategory.isHalalCategory();
    populateHalalAndB2BFlags(categoryInfoUpdateDTO, savedCategory, halalCategoryPreviousValue, storeId);
    boolean existingGenericTemplateFlag = savedCategory.isGenericTemplateEligible();
    savedCategory.setGenericTemplateEligible(categoryInfoUpdateDTO.isGenericTemplateEligible());
    setGenericTemplateEligibleFlag(savedCategory);
    if(existingGenericTemplateFlag != savedCategory.isGenericTemplateEligible()) {
      genericTemplateEligibleChanged = true;
    }
    savedCategory.setWholesalePriceConfigEnabled(categoryInfoUpdateDTO.isWholesalePriceConfigEnabled());
    populateChangedFields(statusChangeFlag, storeId, categoryInfoUpdateDTO, savedCategory,
      categoryChangeEventTypes, username, categoryHistoryEventModelList);
    categoryService.saveUpdatedCategoryInfo(savedCategory, savedInternalActivationInterval,
        parentCategoriesIds, categoryChangeEventTypes, categoryChangeEventTypesV2, masterCategoryIds, salesCategoryIds);
    log.info("genericTemplateEligibleChanged : {} nameChanged : {} isParentCategoryChanged : {} statusChangeFlag : {}",
        genericTemplateEligibleChanged, nameChanged, isParentCategoryChanged, statusChangeFlag);
    boolean clearCacheTree =
      ConverterUtil.doCacheClearCategoryTree(genericTemplateEligibleChanged, nameChanged,
        isParentCategoryChanged, statusChangeFlag);
    updateChildCategoryAndClearCache(savedCategory, b2bExclusivePreviousValue,
      halalCategoryPreviousValue, clearCacheTree);
    return categoryHistoryEventModelList;
  }

  private String fetchParentCategoryCode(Category category) {
    return Objects.nonNull(category.getParentCategory()) ?
      category.getParentCategory().getName() :
      StringUtils.EMPTY;
  }

  private String fetchDescription(byte[] description) {
    return Objects.isNull(description) ? Constants.HYPHEN :
      new String(description, StandardCharsets.UTF_8);
  }

  private void populateDisplayAndNameChange(Category savedCategory,
    CategoryInfoUpdateDTO categoryInfoUpdateDTO, Set<String> categoryChangeEventTypesV2,
    boolean statusChangeFlag) {
    if (Objects.isNull(savedCategory)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
        CATEGORY_NOT_FOUND + categoryInfoUpdateDTO.getId());
    }

    if (savedCategory.isDisplay() != categoryInfoUpdateDTO.isDisplay()) {
      categoryChangeEventTypesV2.add(CategoryChangeEventType.DISPLAY_CHANGE.name());
    }

    if (statusChangeFlag && StringUtils.isBlank(categoryInfoUpdateDTO.getOscId())
      && Objects.nonNull(savedCategory.getOriginalSalesCategory())) {
      categoryInfoUpdateDTO.setOscId(savedCategory.getOriginalSalesCategory().getId());
    }
  }
  private void populateHalalAndB2BFlags(CategoryInfoUpdateDTO categoryInfoUpdateDTO,
    Category savedCategory, boolean halalCategoryPreviousValue, String storeId) {
    if (Objects.nonNull(categoryInfoUpdateDTO.getB2bExclusive())) {
      updateB2bExclusiveFlag(savedCategory, savedCategory.getParentCategory(),
        categoryInfoUpdateDTO.getB2bExclusive());
    }
    if (Objects.nonNull(categoryInfoUpdateDTO.getHalalCategory())
      && halalCategoryPreviousValue != categoryInfoUpdateDTO.getHalalCategory()) {
      updateHalalFlag(storeId, savedCategory, savedCategory.getParentCategory(),
        categoryInfoUpdateDTO.getHalalCategory(), false);
    }
  }

  private void populateChangedFields(boolean statusChangeFlag, String storeId,
    CategoryInfoUpdateDTO categoryInfoUpdateDTO, Category savedCategory,
    List<CategoryChangeEventType> categoryChangeEventTypes, String username,
    List<CategoryHistoryEventModel> historyEventModelList) throws Exception {
    if (!statusChangeFlag) {
      updateCategoryShippingByCategoryInfoUpdateDTO(storeId, categoryInfoUpdateDTO, savedCategory,
        username, historyEventModelList);
    } else {
      savedCategory.setActivated(categoryInfoUpdateDTO.isActivated());
      categoryChangeEventTypes.add(CategoryChangeEventType.ACTIVATE_DEACTIVATE_CHANGE);
    }
    if (StringUtils.isEmpty(savedCategory.getStoreId())) {
      savedCategory.setStoreId(storeId);
    }
    if (!(categoryChangeEventTypes.size() == 1 && categoryChangeEventTypes.contains(
      CategoryChangeEventType.CATEGORY_OSC_MAPPING_CHANGE))) {
      categoryChangeEventTypes.add(CategoryChangeEventType.DATA_CHANGE);
    }
  }

  private void updateChildCategoryAndClearCache(Category savedCategory,
    boolean b2bExclusivePreviousValue, boolean halalCategoryPreviousValue, boolean clearCacheTree)
    throws Exception {
    if ((savedCategory.isB2bExclusive() && !b2bExclusivePreviousValue)
      || halalCategoryPreviousValue != savedCategory.isHalalCategory()) {
      categoryService.updateB2bExclusiveOrHalalCategoryFlagForChildCategories(
        savedCategory.getStoreId(), savedCategory, savedCategory.isB2bExclusive(),
        savedCategory.isHalalCategory());
    }
    if (clearCacheTree) {
      evictGenericCategoryTreeCache();
    }
  }

  private void checkAndUpdateOSC(String storeId, CategoryInfoUpdateDTO categoryInfoUpdateDTO,
    List<CategoryChangeEventType> categoryChangeEventTypes, Category savedCategory, String username,
      List<CategoryHistoryEventModel> historyEventModelList, Set<String> categoryChangeEventTypesV2,
      boolean oscChangeCheckSwitch) {
    if (StringUtils.isBlank(categoryInfoUpdateDTO.getOscId())) {
      log.error(
          "Error when updating master category details : {} " + ErrorMessage.OSC_ID_MUST_NOT_BE_BLANK.getMessage(),
          categoryInfoUpdateDTO.getCategoryCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.OSC_ID_MUST_NOT_BE_BLANK.getMessage());
    }

    OriginalSalesCategory originalSalesCategory =
        oscService.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(storeId, categoryInfoUpdateDTO.getOscId());

    if (Objects.isNull(originalSalesCategory)) {
      log.error("Error when updating master category details : {} " + ErrorMessage.OSC_NOT_FOUND.getMessage(),
          categoryInfoUpdateDTO.getOscId());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessage.OSC_NOT_FOUND.getMessage());
    }

    if (Objects.isNull(savedCategory.getOriginalSalesCategory()) || !StringUtils
        .equals(savedCategory.getOriginalSalesCategory().getId(), categoryInfoUpdateDTO.getOscId())) {
      log.info("OSC updated for category : {} osc id : {}", categoryInfoUpdateDTO.getCategoryCode(),
          categoryInfoUpdateDTO.getOscId());
      if (oscChangeCheckSwitch && savedCategory.getDangerousGoodsLevel()
          != categoryInfoUpdateDTO.getDangerousGoodsLevel()) {
        categoryChangeEventTypesV2.add(CategoryChangeEventType.DATA_CHANGE.name());
      }
      if (categoryHistoryEnable) {
        String prevOsc = Objects.nonNull(savedCategory.getOriginalSalesCategory()) ?
          savedCategory.getOriginalSalesCategory().getOscLongText() :
          StringUtils.EMPTY;
        populateOscChangeHistory(savedCategory.getCategoryCode(), prevOsc,
          originalSalesCategory.getOscLongText(), username, savedCategory.getStoreId(),
          historyEventModelList);
      }
      savedCategory.setOriginalSalesCategory(originalSalesCategory);
      savedCategory.setOscUpdatedBy(categoryInfoUpdateDTO.getUpdatedBy());
      savedCategory.setOscUpdatedDate(new Date());
      categoryChangeEventTypes.add(CategoryChangeEventType.CATEGORY_OSC_MAPPING_CHANGE);
    }
  }

  /**
   * Update CategoryShipping
   *
   * @param storeId
   * @param categoryInfoUpdateDTO
   * @param savedCategory
   * @throws Exception
   */
  private void updateCategoryShippingByCategoryInfoUpdateDTO(String storeId,
    CategoryInfoUpdateDTO categoryInfoUpdateDTO, Category savedCategory, String username,
    List<CategoryHistoryEventModel> historyEventModelList)
      throws Exception {
    List<CategoryShipping> categoryShippingList =
        categoryShippingService.findByCategoryCode(storeId, savedCategory.getCategoryCode());
      for (CategoryShipping categoryShipping : categoryShippingList) {
        if (categoryHistoryEnable) {
          try {
            ShippingRequest existingShippingRequest =
              objectMapper.readValue(categoryShipping.getShippingCode(), ShippingRequest.class);
            if (Objects.nonNull(existingShippingRequest)) {
              populateShippingChangeHistory(existingShippingRequest, categoryInfoUpdateDTO, storeId,
                username, historyEventModelList);
            }
          } catch (Exception e) {
            log.error("Exception in mapping request {}", categoryShipping.getShippingCode(), e);
          }
        }
      ShippingRequest shippingRequest =
          new ShippingRequest(categoryInfoUpdateDTO.isDeliveredByMerchant(), categoryInfoUpdateDTO.isSpecialHandling(),
              categoryInfoUpdateDTO.isDirectFlight());
      shippingRequest.setAgeLimit(categoryInfoUpdateDTO.getAgeLimit());
      shippingRequest.setSizeChartRequired(categoryInfoUpdateDTO.isSizeChartRequired());
      categoryShipping.setShippingCode(objectMapper.writeValueAsString(shippingRequest));
      if (StringUtils.isEmpty(categoryShipping.getStoreId())) {
        categoryShipping.setStoreId(storeId);
      }
      categoryShippingService.update(categoryShipping);
    }
  }

  /**
   * set InternalActivationPeriod based on Request
   *
   * @param categoryInfoUpdateDTO
   * @param savedCategory
   */
  private void setInternalActivationPeriodByCategoryInfoUpdateDTO(CategoryInfoUpdateDTO categoryInfoUpdateDTO, Category savedCategory) {
    if (StringUtils.isBlank(categoryInfoUpdateDTO.getParentCategoryId()) && Objects.isNull(categoryInfoUpdateDTO.getInternalActivationInterval())) {
      savedCategory.setInternalActivationInterval(defaultInternalActivationPeriod);
    } else if (StringUtils.isNotBlank(categoryInfoUpdateDTO.getParentCategoryId())) {
      savedCategory.setInternalActivationInterval(null);
    } else {
      savedCategory.setInternalActivationInterval(categoryInfoUpdateDTO.getInternalActivationInterval());
    }
  }

  /**
   * Set parent category for category
   *
   * @param storeId
   * @param parentCategoryId
   * @param savedCategory
   */
  private List<String> setParentCategoryByCategoryInfoUpdateDTO(String storeId, String parentCategoryId,
      Category savedCategory) throws Exception {
    Set<String> parentCategoriesIds = new HashSet<>();
    List<String> existingParentCategoryHierarchy =
        categoryService.getParentCategoryHierarchyByCategoryId(savedCategory.getId());
    parentCategoriesIds.addAll(existingParentCategoryHierarchy);
    if (StringUtils.isEmpty(parentCategoryId)) {
      savedCategory.setParentCategory(null);
    } else if (Objects.isNull(savedCategory.getParentCategory()) || !parentCategoryId
        .equals(savedCategory.getParentCategory().getId())) {
      List<String> newParentCategoryHierarchy =
          categoryService.getParentCategoryHierarchyByCategoryId(parentCategoryId);
      newParentCategoryHierarchy.add(parentCategoryId);
      parentCategoriesIds.addAll(newParentCategoryHierarchy);
      savedCategory.setParentCategory(getParentCategory(storeId, parentCategoryId));
    }
    return new ArrayList<>(parentCategoriesIds);
  }

  /**
   * Get Parent Category by parentCategoryId
   * @param storeId
   * @param parentCategoryId
   * @return
   */
  private Category getParentCategory(String storeId, String parentCategoryId){
    Category parentCategory =
        categoryService.getCategoryByStoreIdAndIdCached(storeId, parentCategoryId);
    if (Objects.isNull(parentCategory) || parentCategory.isMarkForDelete()) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          PARENT_CATEGORY_NOT_FOUND + parentCategoryId);
    }
    return parentCategory;
  }

  @Override
  public List<CategoryServiceDTO> setChildCountByFilterType(String storeId,
      Page<Category> categoryPage, String filterType) throws Exception {
    List<CategoryServiceDTO> categoryDTOs = new ArrayList<CategoryServiceDTO>();
    for (Category category : categoryPage.getContent()) {
      CategoryServiceDTO responseDTO = new CategoryServiceDTO();
      responseDTO.setCategory(category);
      categoryDTOs.add(responseDTO);
    }
    setChildCountByFilterType(storeId, filterType, categoryDTOs);
    return categoryDTOs;
  }

  @Override
  @Async
  public void publishAllCategories(String catalogName, String storeId) {
    try {
      this.categoryService.publishAllCategories(catalogName, storeId);
    } catch (Exception e) {
      log.error("Error while publishing categories for catalog name : {}", catalogName, e);
    }
  }

  /**
   * get child count by filter type
   *
   * @param storeId
   * @param filterType
   * @param categoryDTOs
   */
  private void setChildCountByFilterType(String storeId, String filterType, List<CategoryServiceDTO> categoryDTOs) {
    if (ALL.equals(filterType)) {
      categoryDTOs.forEach(
          categoryDTO -> categoryDTO.setChildCount(this.categoryService.findOverAllChildCountForParent(storeId, categoryDTO.getCategory())));
    } else if (ACTIVE.equals(filterType)) {
      categoryDTOs.forEach(
          categoryDTO -> categoryDTO.setChildCount(this.categoryService.findActiveChildCountForParent(storeId, categoryDTO.getCategory())));
    } else if (IN_ACTIVE.equals(filterType)) {
      categoryDTOs.forEach(categoryDTO -> categoryDTO
          .setChildCount(this.categoryService.findInActiveChildCountForParent(storeId, categoryDTO.getCategory())));
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public Pair<CategoryAndHierarchyDto, List<RestrictedKeywordHistoryEventModel>> createCategoryWithoutEventPublish(
    String storeId, CategoryDetailDTO categoryDetailDTO) throws Exception {
    CategoryMappingsUpdateDTO categoryMappingsUpdateDTO =
        categoryDetailDTO.getCategoryMappingsDetail();
    CategoryInfoUpdateDTO categoryInfoUpdateDTO = categoryDetailDTO.getCategoryInfoDetail();
    Category newCategory = new Category();
    List<String> parentCategoryHierarchy = new ArrayList<>();
    if (StringUtils.isNotEmpty(categoryInfoUpdateDTO.getParentCategoryId())) {
      newCategory.setParentCategory(getParentCategory(storeId, categoryInfoUpdateDTO.getParentCategoryId()));
      List<String> categoryHierarchy =
          categoryService.getParentCategoryHierarchyByCategoryId(categoryInfoUpdateDTO.getParentCategoryId());
      if (CollectionUtils.isNotEmpty(categoryHierarchy)) {
        parentCategoryHierarchy.addAll(categoryHierarchy);
      }
      parentCategoryHierarchy.add(categoryInfoUpdateDTO.getParentCategoryId());
    }
    Catalog catalog = this.catalogService.findByStoreIdAndId(storeId, categoryDetailDTO.getCatalogId());
    if (Objects.isNull(catalog)) {
      log.error("No catalog found while saving new category:{}", newCategory);
      throw new ApplicationRuntimeException(ErrorCategory.DATA_ACCESS, CATEGORY_NOT_CREATED);
    }
    newCategory.setCatalog(catalog);
    if (CatalogType.MASTER_CATALOG.equals(catalog.getCatalogType())) {
      validateForMasterCatalog(categoryInfoUpdateDTO, storeId, newCategory);
    }
    setInternalActivationPeriodByCategoryInfoUpdateDTO(categoryInfoUpdateDTO, newCategory);
    updateCategoryInfoForNewCategory(storeId, categoryInfoUpdateDTO, newCategory);
    updateCategoryAttributesForNewCategory(storeId, categoryMappingsUpdateDTO, newCategory);
    updateCategoryReferenceForNewCategory(storeId, categoryMappingsUpdateDTO, newCategory);
    setGenericTemplateEligibleFlag(newCategory);
    Category category;
    try {
      if (CollectionUtils.isNotEmpty(newCategory.getMasterCategoryReferences())) {
        category = categoryService.saveAndUpdateProductCategory(storeId, newCategory);
      } else {
        category = categoryService.save(newCategory);
      }
    } catch (Exception ex) {
      log.error("Error while saving new category:{}", newCategory, ex);
      throw new ApplicationRuntimeException(ErrorCategory.DATA_ACCESS, CATEGORY_NOT_CREATED);
    }
    List<RestrictedKeywordHistoryEventModel> historyEventModelList =
      updateCategoryRestrictedKeywordsForNewCategory(storeId, categoryMappingsUpdateDTO,
        newCategory);
    if(Objects.nonNull(categoryMappingsUpdateDTO.getWholesaleMapping()) && Objects
        .nonNull(categoryMappingsUpdateDTO.getWholesaleMapping().getConfigurationType())){
      updateWholesalePriceConfigForNewCategory(storeId, categoryMappingsUpdateDTO, newCategory, category.getId());
    }
    saveCategoryShipping(categoryInfoUpdateDTO, newCategory);
    return Pair.of(new CategoryAndHierarchyDto(category, parentCategoryHierarchy),
      historyEventModelList);
  }

  private void validateForMasterCatalog(CategoryInfoUpdateDTO categoryInfoUpdateDTO, String storeId,
    Category newCategory) {
    if (StringUtils.isNotEmpty(categoryInfoUpdateDTO.getOscId())) {
      OriginalSalesCategory originalSalesCategory =
        oscService.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(storeId,
          categoryInfoUpdateDTO.getOscId());
      if (Objects.isNull(originalSalesCategory)) {
        log.error("No OSC found while saving new category:{}", newCategory);
        throw new ApplicationRuntimeException(ErrorCategory.DATA_ACCESS, CATEGORY_NOT_CREATED);
      } else {
        newCategory.setOriginalSalesCategory(originalSalesCategory);
        newCategory.setOscUpdatedDate(new Date());
        newCategory.setOscUpdatedBy(GdnMandatoryParameterUtil.getUsername());
      }
    } else {
      log.error("No OSC found while saving new category:{}", newCategory);
      throw new ApplicationRuntimeException(ErrorCategory.DATA_ACCESS, CATEGORY_NOT_CREATED);
    }
  }

  @Override
  public void publishCategoryChangeAndClearCache(CategoryAndHierarchyDto categoryAndHierarchyDetails) throws Exception {
    categoryService.evictChildCategoryCache(categoryAndHierarchyDetails.getParentCategoryHierarchy());
    categoryService.evictActiveChildCategoryCache(categoryAndHierarchyDetails.getParentCategoryHierarchy());
    evictGenericCategoryTreeCache();
    domainEventPublisherService.publishCategory(categoryAndHierarchyDetails.getCategory(), null, new HashSet<>(), true);
  }

  public void evictGenericCategoryTreeCache() {
    categoryService.evictGenericCategoryTreeCache(Constants.DEFAULT_STORE_ID, true, true);
    categoryService.evictGenericCategoryTreeCache(Constants.DEFAULT_STORE_ID, true, false);
    categoryService.evictGenericCategoryTreeCache(Constants.DEFAULT_STORE_ID, false, true);
    categoryService.evictGenericCategoryTreeCache(Constants.DEFAULT_STORE_ID, false, false);
  }

  /**
   * Updating Category Info for new category
   * @param storeId
   * @param categoryInfoUpdateDTO
   * @param newCategory
   */
  private void updateCategoryInfoForNewCategory(String storeId,
      CategoryInfoUpdateDTO categoryInfoUpdateDTO, Category newCategory){
    newCategory.setName(categoryInfoUpdateDTO.getName());
    newCategory.setNameEnglish(categoryInfoUpdateDTO.getNameEnglish());
    newCategory.setSequence(categoryInfoUpdateDTO.getSequence());
    newCategory.setDefaultDescription(categoryInfoUpdateDTO.getDefaultDescription());
    newCategory.setDescriptionEnglish(categoryInfoUpdateDTO.getDescriptionEnglish());
    newCategory.setDisplay(categoryInfoUpdateDTO.isDisplay());
    newCategory.setLogisticAdjustment(categoryInfoUpdateDTO.getLogisticAdjustment());
    newCategory.setWarranty(categoryInfoUpdateDTO.isWarranty());
    newCategory.setNeedIdentity(categoryInfoUpdateDTO.isNeedIdentity());
    newCategory.setUpdatedBy(categoryInfoUpdateDTO.getCreatedBy());
    newCategory.setUpdatedDate(categoryInfoUpdateDTO.getCreatedDate());
    newCategory.setCreatedBy(categoryInfoUpdateDTO.getCreatedBy());
    newCategory.setCreatedDate(categoryInfoUpdateDTO.getCreatedDate());
    newCategory.setDangerousGoodsLevel(categoryInfoUpdateDTO.getDangerousGoodsLevel());
    newCategory.setUmkm(categoryInfoUpdateDTO.isUmkm());
    newCategory.setWholesalePriceConfigEnabled(categoryInfoUpdateDTO.isWholesalePriceConfigEnabled());
    newCategory.setStoreId(storeId);
    newCategory.setGenericTemplateEligible(categoryInfoUpdateDTO.isGenericTemplateEligible());
    newCategory.setDocumentType(categoryInfoUpdateDTO.getDocumentType());
    if (Objects.nonNull(categoryInfoUpdateDTO.getB2bExclusive())) {
      updateB2bExclusiveFlag(newCategory, newCategory.getParentCategory(), categoryInfoUpdateDTO.getB2bExclusive());
    }
    if (Objects.nonNull(categoryInfoUpdateDTO.getHalalCategory())) {
      updateHalalFlag(storeId, newCategory, newCategory.getParentCategory(), categoryInfoUpdateDTO.getHalalCategory(), true);
    }
    newCategory.setBopisEligible(categoryInfoUpdateDTO.isBopisEligible());
  }

  /**
   * updating category attributes for new category
   * @param storeId
   * @param categoryMappingsUpdateDTO
   * @param newCategory
   */
  private void updateCategoryAttributesForNewCategory(String storeId,
      CategoryMappingsUpdateDTO categoryMappingsUpdateDTO, Category newCategory) {
    if (CollectionUtils.isNotEmpty(categoryMappingsUpdateDTO.getAddedAttributes())) {
      List<String> allAttributeIds = categoryMappingsUpdateDTO.getAddedAttributes().stream()
          .map(CategoryAttributeUpdateDTO::getAttributeId).distinct().collect(Collectors.toList());
      Map<String, Attribute> attributeIdAttributeMap =
          getAttributeIdAttributeMapByAttributeIds(storeId, allAttributeIds);
      categoryMappingsUpdateDTO.getAddedAttributes().stream().map(
          categoryAttributeUpdateDTO -> ConverterUtil
              .toCategoryAttribute(storeId, categoryAttributeUpdateDTO, newCategory,
                  attributeIdAttributeMap.get(categoryAttributeUpdateDTO.getAttributeId()),
                  categoryMappingsUpdateDTO.getUpdatedBy(),
                  categoryMappingsUpdateDTO.getUpdatedDate()))
          .collect(Collectors.toCollection(newCategory::getCategoryAttributes));
    }
  }

  /**
   * Updating Master category reference for new sale category
   * @param storeId
   * @param categoryMappingsUpdateDTO
   * @param newCategory
   */
  private void updateCategoryReferenceForNewCategory(String storeId,
      CategoryMappingsUpdateDTO categoryMappingsUpdateDTO, Category newCategory) {
    if (CollectionUtils.isNotEmpty(categoryMappingsUpdateDTO.getAddedMasterCategoryIds())) {
      Map<String, Category> categoryIdCategoryMap = getCategoryIdCategoryMapByCategoryIds(storeId,
          categoryMappingsUpdateDTO.getAddedMasterCategoryIds());
      categoryMappingsUpdateDTO.getAddedMasterCategoryIds().stream().map(categoryId -> ConverterUtil
          .toCategoryReference(storeId, categoryIdCategoryMap.get(categoryId), newCategory,
              categoryMappingsUpdateDTO.getUpdatedBy(), categoryMappingsUpdateDTO.getUpdatedDate()))
          .collect(Collectors.toCollection(newCategory::getMasterCategoryReferences));
    }
  }

  /**
   * Save categoryShipping for new category
   * @param categoryInfoUpdateDTO
   * @param newCategory
   * @throws Exception
   */
  private void saveCategoryShipping(CategoryInfoUpdateDTO categoryInfoUpdateDTO,
    Category newCategory) throws Exception {
    CategoryShipping categoryShipping = new CategoryShipping();
    categoryShipping.setCategoryCode(newCategory.getCategoryCode());
    ShippingRequest shippingRequest =
        new ShippingRequest(categoryInfoUpdateDTO.isDeliveredByMerchant(),
            categoryInfoUpdateDTO.isSpecialHandling(), categoryInfoUpdateDTO.isDirectFlight());
    shippingRequest.setAgeLimit(categoryInfoUpdateDTO.getAgeLimit());
    shippingRequest.setSizeChartRequired(categoryInfoUpdateDTO.isSizeChartRequired());
    categoryShipping.setShippingCode(objectMapper.writeValueAsString(shippingRequest));
    categoryShipping.setStoreId(newCategory.getStoreId());
    categoryShippingService.save(categoryShipping);
  }

  @Transactional(readOnly = false)
  @Override
  public CategoryUpdateHistoryDTO updateCategoryMappings(String storeId,
      CategoryMappingsUpdateDTO categoryMappingsUpdateDTO) throws Exception {
    List<CategoryChangeEventType> categoryChangeEventTypes = new ArrayList<>();
    Category savedCategory =
        categoryService.findByStoreIdAndIdInitAllCategoryAttribute(storeId, categoryMappingsUpdateDTO.getId());
    if(Objects.isNull(savedCategory)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          CATEGORY_NOT_FOUND + categoryMappingsUpdateDTO.getId());
    }

    CategoryUpdateHistoryDTO categoryUpdateHistoryDTO = null;

    if(CollectionUtils.isNotEmpty(categoryMappingsUpdateDTO.getAddedAttributes()) ||
        CollectionUtils.isNotEmpty(categoryMappingsUpdateDTO.getDeletedAttributes())) {
      categoryUpdateHistoryDTO =
          adjustAttributeMappings(storeId, categoryMappingsUpdateDTO, savedCategory);
      categoryUpdateHistoryDTO.setCategory(savedCategory);
    }

    if(CollectionUtils.isNotEmpty(categoryMappingsUpdateDTO.getAddedMasterCategoryIds()) ||
        CollectionUtils.isNotEmpty(categoryMappingsUpdateDTO.getDeletedMasterCategoryIds())){
      adjustCategoryMappings(storeId, categoryMappingsUpdateDTO, savedCategory);
    }
    categoryChangeEventTypes.add(CategoryChangeEventType.CATEGORY_MAPPING_CHANGE);
    setGenericTemplateEligibleFlag(savedCategory);
    categoryService.saveUpdatedCategory(savedCategory, savedCategory.getInternalActivationInterval(), null,
        categoryChangeEventTypes);
    return categoryUpdateHistoryDTO;
  }

  @Override
  public void updateCategoryMappingsAndPublishHistory(String storeId,
      CategoryMappingsUpdateDTO categoryMappingsUpdateDTO) throws Exception {
    CategoryUpdateHistoryDTO categoryUpdateHistoryDTO =
        getCategoryServiceWrapper().updateCategoryMappings(storeId, categoryMappingsUpdateDTO);
    if (Objects.nonNull(categoryUpdateHistoryDTO)) {
      domainEventPublisherService.publishCategoryUpdateHistory(Collections.singletonList(
          ConverterUtil.constructCategoryHistoryEventModel(
              categoryUpdateHistoryDTO.getPreviousValue(),
              categoryUpdateHistoryDTO.getCurrentValue(),
              categoryUpdateHistoryDTO.getCategory().getCategoryCode(), storeId,
              mandatoryParameterHelper.getUsername(),
              CategoryUpdateHistoryActivity.ATTRIBUTE_MAPPING.name())));
    }
  }

  private void setGenericTemplateEligibleFlag(Category category) {
    category.getCategoryAttributes().stream().filter(categoryAttribute -> !categoryAttribute.isMarkForDelete())
        .map(CategoryAttribute::getAttribute).filter(attribute -> !StringUtils.equals(BRAND, attribute.getName()))
        .filter(attribute -> !isEligibleForGenericTemplate(attribute)).findFirst()
        .ifPresent(attribute -> category.setGenericTemplateEligible(false));
  }

  private boolean isEligibleForGenericTemplate(Attribute attribute) {
    if (isDefiningOrVariantCreationTrue(attribute)) {
      return WARNA.equalsIgnoreCase(attribute.getName()) || UKURAN.equalsIgnoreCase(attribute.getName()) || VARIASI
          .equalsIgnoreCase(attribute.getName());
    } else {
      return !attribute.isMandatory();
    }
  }

  private boolean isDefiningOrVariantCreationTrue(Attribute attribute) {
    return attribute.getAttributeType().equals(AttributeType.DEFINING_ATTRIBUTE) || attribute.isVariantCreation();
  }

  /**
   * update Attribute Mapping for a master category
   * @param storeId
   * @param categoryMappingsUpdateDTO
   * @param savedCategory
   */
  private CategoryUpdateHistoryDTO adjustAttributeMappings(
      String storeId,
      CategoryMappingsUpdateDTO categoryMappingsUpdateDTO, Category savedCategory)
      throws JsonProcessingException {
    List<String> addedAttributeIds = categoryMappingsUpdateDTO.getAddedAttributes().stream()
        .map(CategoryAttributeUpdateDTO::getAttributeId).collect(Collectors.toList());
    List<String> deletedAttributeIds = categoryMappingsUpdateDTO.getDeletedAttributes().stream()
        .map(CategoryAttributeUpdateDTO::getAttributeId).collect(Collectors.toList());
    List<String> allAttributeIds = Stream.concat(addedAttributeIds.stream(),
        deletedAttributeIds.stream()).distinct().collect(Collectors.toList());

    Map<String, Attribute> attributeIdAttributeMap =
        getAttributeIdAttributeMapByAttributeIds(storeId, allAttributeIds);
    Map<String, CategoryAttribute> existingAttributeIdCategoryAttributeMap = savedCategory
        .getCategoryAttributes().stream().collect(Collectors.toMap(
            categoryAttribute -> categoryAttribute.getAttribute().getId(), Function.identity(),
            (attributeId1, attributeId2) -> attributeId1));

    List<CategoryAttributeMappingUpdateHistoryEventDTO> previousCategoryAttributeHistory =
        new ArrayList<>();

    List<CategoryAttributeMappingUpdateHistoryEventDTO> updatedCategoryAttributeHistory =
        new ArrayList<>();

    for (CategoryAttributeUpdateDTO categoryAttributeUpdateDTO: categoryMappingsUpdateDTO.getAddedAttributes()) {
      CategoryAttribute categoryAttribute =
          existingAttributeIdCategoryAttributeMap.get(categoryAttributeUpdateDTO.getAttributeId());
      if(Objects.nonNull(categoryAttribute)){
        if (!categoryAttribute.isMarkForDelete()) {
          previousCategoryAttributeHistory.add(
              ConverterUtil.convertToCategoryAttributeMappingUpdateEventModel(categoryAttribute));
        }
        existingAttributeIdCategoryAttributeMap.get(categoryAttributeUpdateDTO.getAttributeId())
            .setUSP(categoryAttributeUpdateDTO.isUsp());
        existingAttributeIdCategoryAttributeMap.get(categoryAttributeUpdateDTO.getAttributeId())
            .setMainDefiningAttribute(categoryAttributeUpdateDTO.isMainDefiningAttribute());
        existingAttributeIdCategoryAttributeMap.get(categoryAttributeUpdateDTO.getAttributeId())
            .setSequence(categoryAttributeUpdateDTO.getSequence());
        existingAttributeIdCategoryAttributeMap.get(categoryAttributeUpdateDTO.getAttributeId())
            .setMarkForDelete(false);

        updatedCategoryAttributeHistory.add(CategoryAttributeMappingUpdateHistoryEventDTO.builder()
            .attributeName(attributeIdAttributeMap.get(categoryAttributeUpdateDTO.getAttributeId()).getName())
            .attributeCode(attributeIdAttributeMap.get(categoryAttributeUpdateDTO.getAttributeId()).getAttributeCode())
            .isUSP(categoryAttributeUpdateDTO.isUsp()).build());

      } else {
        savedCategory.getCategoryAttributes().add(ConverterUtil.toCategoryAttribute(
            storeId, categoryAttributeUpdateDTO, savedCategory,
            attributeIdAttributeMap.get(categoryAttributeUpdateDTO.getAttributeId()),
            categoryMappingsUpdateDTO.getUpdatedBy(), categoryMappingsUpdateDTO.getUpdatedDate()));

        updatedCategoryAttributeHistory.add(CategoryAttributeMappingUpdateHistoryEventDTO.builder()
            .attributeName(attributeIdAttributeMap.get(categoryAttributeUpdateDTO.getAttributeId()).getName())
            .attributeCode(attributeIdAttributeMap.get(categoryAttributeUpdateDTO.getAttributeId()).getAttributeCode())
            .isUSP(categoryAttributeUpdateDTO.isUsp()).build());
      }
    }

    savedCategory.getCategoryAttributes().stream().filter(
            categoryAttribute -> deletedAttributeIds.contains(categoryAttribute.getAttribute().getId()))
        .forEach(categoryAttribute -> {
          previousCategoryAttributeHistory.add(
              ConverterUtil.convertToCategoryAttributeMappingUpdateEventModel(categoryAttribute));
          categoryAttribute.setMarkForDelete(true);
        });

    List<CategoryAttributeMappingUpdateHistoryEventDTO> updatedCategoryAttributeMapping =
        savedCategory.getCategoryAttributes().stream()
            .map(ConverterUtil::convertToCategoryAttributeMappingUpdateEventModel)
            .collect(Collectors.toList());

    return CategoryUpdateHistoryDTO.builder()
        .previousValue(objectMapper.writeValueAsString(previousCategoryAttributeHistory))
        .currentValue(objectMapper.writeValueAsString(updatedCategoryAttributeHistory)).build();
  }

  /**
   * Get attributes from ids and create a map of attributeId and attribute
   * @param storeId
   * @param allAttributeIds
   * @return
   */
  private Map<String, Attribute> getAttributeIdAttributeMapByAttributeIds(
      String storeId, List<String> allAttributeIds) {
    List<Attribute> attributes = attributeService.findByAttributeIds(storeId, allAttributeIds);
    Map<String, Attribute> attributeIdAttributeMap = attributes.stream().distinct()
        .collect(Collectors.toMap(Attribute::getId, Function.identity()));
    if(allAttributeIds.size() != attributeIdAttributeMap.keySet().size()) {
      List<String> notFoundAttributeIds = new ArrayList<>(allAttributeIds);
      notFoundAttributeIds.removeAll(attributeIdAttributeMap.keySet());
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, ATTRIBUTES_NOT_FOUND +
          notFoundAttributeIds);
    }
    return attributeIdAttributeMap;
  }

  /**
   * Update category mappings for a sales category
   * @param storeId
   * @param categoryMappingsUpdateDTO
   * @param savedCategory
   */
  private void adjustCategoryMappings(String storeId,
      CategoryMappingsUpdateDTO categoryMappingsUpdateDTO, Category savedCategory) {
    List<String> allMasterCategoryIds = Stream.concat(
        categoryMappingsUpdateDTO.getAddedMasterCategoryIds().stream(),
        categoryMappingsUpdateDTO.getDeletedMasterCategoryIds().stream()).collect(Collectors.toList());

    Map<String, Category> categoryIdCategoryMap =
        getCategoryIdCategoryMapByCategoryIds(storeId, allMasterCategoryIds);
    Map<String, CategoryReference> existingCategoryIdCategoryMap = savedCategory.getMasterCategoryReferences()
        .stream().collect(Collectors.toMap(
            categoryReference -> categoryReference.getMasterCategory().getId(), Function.identity(),
            (masterCategoryId1, masterCategoryId2) -> masterCategoryId1));

    for (String categoryId: categoryMappingsUpdateDTO.getAddedMasterCategoryIds()) {
      if(Objects.nonNull(existingCategoryIdCategoryMap.get(categoryId))) {
        existingCategoryIdCategoryMap.get(categoryId).setMarkForDelete(false);
      } else {
        savedCategory.getMasterCategoryReferences().add(ConverterUtil.toCategoryReference(storeId,
            categoryIdCategoryMap.get(categoryId), savedCategory,
            categoryMappingsUpdateDTO.getUpdatedBy(), categoryMappingsUpdateDTO.getUpdatedDate()));
      }
    }

    savedCategory.getMasterCategoryReferences().stream()
        .filter(categoryReference -> categoryMappingsUpdateDTO.getDeletedMasterCategoryIds().contains(
            categoryReference.getMasterCategory().getId()))
        .forEach(categoryReference -> categoryReference.setMarkForDelete(true));
  }

  /**
   * Get categories by ids and create a map of categoryId and category
   * @param storeId
   * @param allMasterCategoryIds
   * @return
   */
  private Map<String, Category> getCategoryIdCategoryMapByCategoryIds(String storeId,
      List<String> allMasterCategoryIds) {
    List<Category> categories = categoryService.findByCategoryIds(storeId, allMasterCategoryIds);
    Map<String, Category> categoryIdCategoryMap = categories.stream().distinct()
        .collect(Collectors.toMap(Category::getId, Function.identity()));
    if(allMasterCategoryIds.size() != categoryIdCategoryMap.keySet().size()) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, CATEGORIES_NOT_FOUND +
          new ArrayList<>(allMasterCategoryIds).removeAll(categoryIdCategoryMap.keySet()));
    }
    return categoryIdCategoryMap;
  }

  /**
   * Update the restricted keyword mapping for new categories
   *
   * @param categoryMappingsUpdateDTO
   * @param category
   */
  private List<RestrictedKeywordHistoryEventModel> updateCategoryRestrictedKeywordsForNewCategory(String storeId,
      CategoryMappingsUpdateDTO categoryMappingsUpdateDTO, Category category) throws Exception {
    List<String> deletedRestrictedKeywordId =
        categoryMappingsUpdateDTO.getDeletedKeywords().stream().map(CategoryKeywordsUpdateDTO::getKeywordId)
            .collect(Collectors.toList());

    Pair<List<RestrictedKeyword>, List<RestrictedKeywordHistoryEventModel>> addedKeywordHistoryPair =
      getRestrictedKeywordsForAddedKeywords(storeId, categoryMappingsUpdateDTO.getAddedKeywords(),
        categoryMappingsUpdateDTO.getDeletedKeywords());

    Map<String, CategoryKeywordsUpdateDTO> addedRestrictedKeywordAndRequestMap =
        categoryMappingsUpdateDTO.getAddedKeywords().stream().collect(Collectors
            .toMap(CategoryKeywordsUpdateDTO::getKeyword, categoryKeywordsUpdateDTO -> categoryKeywordsUpdateDTO,
                (a, b) -> a));

    Map<String, CategoryRestrictedKeyword> categoryRestrictedKeywordMap = new HashMap<>();
    if (Objects.nonNull(category.getParentCategory())) {
      List<CategoryRestrictedKeyword> categoryRestrictedKeywordList = categoryRestrictedKeywordService
          .findByStoreIdAndCategoryCode(storeId, category.getParentCategory().getCategoryCode());
      categoryRestrictedKeywordMap = getCategoryRestrictedKeywordMap(categoryRestrictedKeywordList);
      List<RestrictedKeyword> parentCategoryRestrictedKeywords = categoryRestrictedKeywordList.stream()
          .filter(categoryRestrictedKeyword -> !categoryRestrictedKeyword.isMarkForDelete())
          .map(CategoryRestrictedKeyword::getRestrictedKeyword)
          .collect(Collectors.toList());
      addedKeywordHistoryPair.getLeft().addAll(parentCategoryRestrictedKeywords);
    }

    log.info(
        "adding the restricted keywords mapping {} and deleting the restricted keywords with Id : {} for new category : {}",
      addedKeywordHistoryPair.getLeft(), deletedRestrictedKeywordId, category.getCategoryCode());
    addAndDeleteCategoryRestrictedKeywordsMappingsForNewCategory(storeId,
      addedKeywordHistoryPair.getLeft(), deletedRestrictedKeywordId,
        category, addedRestrictedKeywordAndRequestMap, categoryRestrictedKeywordMap);
    return addedKeywordHistoryPair.getRight();
  }

  private Map<String, CategoryRestrictedKeyword> getCategoryRestrictedKeywordMap(
      List<CategoryRestrictedKeyword> categoryRestrictedKeywordList) {
    Map<String, CategoryRestrictedKeyword> categoryRestrictedKeywordMap;
    categoryRestrictedKeywordMap = categoryRestrictedKeywordList.stream()
        .filter(categoryRestrictedKeyword -> Objects.nonNull(categoryRestrictedKeyword.getRestrictedKeyword()))
        .collect(Collectors
            .toMap(categoryRestrictedKeyword -> categoryRestrictedKeyword.getRestrictedKeyword().getKeyword(),
                categoryRestrictedKeyword -> categoryRestrictedKeyword, (a, b) -> a));
    return categoryRestrictedKeywordMap;
  }

  private void addAndDeleteCategoryRestrictedKeywordsMappingsForNewCategory(String storeId,
      List<RestrictedKeyword> addedKeywords, List<String> deletedRestrictedKeywordId, Category category,
      Map<String, CategoryKeywordsUpdateDTO> addedRestrictedKeywordAndRequestMap,
      Map<String, CategoryRestrictedKeyword> categoryRestrictedKeywordMap) {
    List<CategoryRestrictedKeyword> categoryRestrictedKeywordList = addedKeywords.stream()
        .filter(addedRestrictedKeyword -> !deletedRestrictedKeywordId.contains(addedRestrictedKeyword.getId()))
        .map(addedRestrictedKeyword -> ConverterUtil.toCategoryRestrictedKeyword(storeId, addedRestrictedKeyword, category, addedRestrictedKeywordAndRequestMap
            .getOrDefault(addedRestrictedKeyword.getKeyword(), getCategoryRestrictedKeyword(categoryRestrictedKeywordMap, addedRestrictedKeyword))))
        .collect(Collectors.toList());

    log.info("Adding and Deleting the new category : {} with category restricted keywords mappings : {}",
        category.getCategoryCode(), categoryRestrictedKeywordList);
    category.setCategoryRestrictedKeywords(categoryRestrictedKeywordList);
  }

  private CategoryKeywordsUpdateDTO getCategoryRestrictedKeyword(
      Map<String, CategoryRestrictedKeyword> categoryRestrictedKeywordMap, RestrictedKeyword addedRestrictedKeyword) {
    CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO = new CategoryKeywordsUpdateDTO();
    if (categoryRestrictedKeywordMap.containsKey(addedRestrictedKeyword.getKeyword())) {
      CategoryRestrictedKeyword categoryRestrictedKeyword =
          categoryRestrictedKeywordMap.get(addedRestrictedKeyword.getKeyword());
      categoryKeywordsUpdateDTO.setType(categoryRestrictedKeyword.getType());
      categoryKeywordsUpdateDTO.setAction(categoryRestrictedKeyword.getAction());
      categoryKeywordsUpdateDTO.setMessage(categoryRestrictedKeyword.getMessage());
      categoryKeywordsUpdateDTO.setDestinationCategory(categoryRestrictedKeyword.getDestinationCategory());
    }
    return categoryKeywordsUpdateDTO;
  }

  @Override
  @Transactional(readOnly = false)
  public List<RestrictedKeywordHistoryEventModel> updateCategoriesWithRestrictedKeywords(
    String categoryCode, CategoryKeywordsUpdateListDTO categoryKeywordsUpdateListDTO)
    throws Exception {
    List<RestrictedKeywordHistoryEventModel> keywordHistoryEventModelList = new ArrayList<>();
    Category savedCategory =
        categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(categoryKeywordsUpdateListDTO.getStoreId(), categoryCode);
    if (Objects.isNull(savedCategory) || Constants.SALES_CATALOG.equals(
        savedCategory.getCatalog().getCatalogType().name())) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, CATEGORY_NOT_FOUND + categoryCode);
    }
    if (CollectionUtils.isNotEmpty(categoryKeywordsUpdateListDTO.getAddedRestrictedKeywords()) || CollectionUtils
        .isNotEmpty(categoryKeywordsUpdateListDTO.getDeletedRestrictedKeywords())) {
      keywordHistoryEventModelList =
        adjustKeywordMappingsForCategories(categoryKeywordsUpdateListDTO, savedCategory);
    }
    return keywordHistoryEventModelList;
  }

  /**
   * Update the restricted keyword mapping with categories
   *
   * @param categoryKeywordsUpdateListDTO
   * @param category
   */
  private List<RestrictedKeywordHistoryEventModel> adjustKeywordMappingsForCategories(
    CategoryKeywordsUpdateListDTO categoryKeywordsUpdateListDTO, Category category)
    throws Exception {
    String storeId = categoryKeywordsUpdateListDTO.getStoreId();
    restrictedKeywordService.getDeletedRestrictedKeywordId(categoryKeywordsUpdateListDTO);
    Pair<List<RestrictedKeyword>, List<RestrictedKeywordHistoryEventModel>>
      restrictedKeywordHistoryPair = getRestrictedKeywordsForAddedKeywords(storeId,
      categoryKeywordsUpdateListDTO.getAddedRestrictedKeywords(),
      categoryKeywordsUpdateListDTO.getDeletedRestrictedKeywords());
    Map<String, CategoryKeywordsUpdateDTO> addedRestrictedKeywordAndRequestMap =
        categoryKeywordsUpdateListDTO.getAddedRestrictedKeywords().stream().collect(
            Collectors.toMap(keyword -> keyword.getKeyword().toLowerCase(),
                categoryKeywordsUpdateDTO -> categoryKeywordsUpdateDTO, (a, b) -> a));
    log.info(
        "adding the restricted keywords mapping {} and deleting the restricted keywords with Id : {} for CategoryCode : {}",
      restrictedKeywordHistoryPair.getLeft(), categoryKeywordsUpdateListDTO.getDeletedRestrictedKeywords(),
        category.getCategoryCode());
    categoryRestrictedKeywordService.addAndDeleteCategoryRestrictedKeywordsMappings(storeId,
      restrictedKeywordHistoryPair.getLeft(),
        category, categoryKeywordsUpdateListDTO.getDeletedRestrictedKeywords(), addedRestrictedKeywordAndRequestMap);
    categoryRestrictedKeywordService.addAndDeleteCategoryRestrictedKeywordsForChildCategories(storeId,
      restrictedKeywordHistoryPair.getLeft(), category,
      categoryKeywordsUpdateListDTO.getDeletedRestrictedKeywords(),
        addedRestrictedKeywordAndRequestMap);
    return restrictedKeywordHistoryPair.getRight();
  }

  private Pair<List<RestrictedKeyword>, List<RestrictedKeywordHistoryEventModel>> getRestrictedKeywordsForAddedKeywords(
    String storeId, List<CategoryKeywordsUpdateDTO> categoryKeywordsUpdateDTOS,
    List<CategoryKeywordsUpdateDTO> deletedCategoryKeywordsUpdateDTOs) {
    HashMap<String, Boolean> newKeywordsAddedToCategoryValidateDsMap = new HashMap<>();
    Pair<List<RestrictedKeyword>, List<RestrictedKeywordHistoryEventModel>>
      newlySavedRestrictedKeywordHistoryPair = Pair.of(new ArrayList<>(), new ArrayList<>());
    if (markRestrictedKeywordEnable) {
      categoryKeywordsUpdateDTOS.stream().filter(
          categoryKeywordsUpdateDTO -> StringUtils.isNotBlank(categoryKeywordsUpdateDTO.getKeyword()))
        .forEach(categoryKeywordsUpdateDTO -> newKeywordsAddedToCategoryValidateDsMap.put(
          categoryKeywordsUpdateDTO.getKeyword(), categoryKeywordsUpdateDTO.getValidateByDs()));
      deletedCategoryKeywordsUpdateDTOs.stream().filter(
          categoryKeywordsUpdateDTO -> StringUtils.isNotBlank(categoryKeywordsUpdateDTO.getKeyword()))
        .forEach(categoryKeywordsUpdateDTO -> newKeywordsAddedToCategoryValidateDsMap.put(
          categoryKeywordsUpdateDTO.getKeyword(), categoryKeywordsUpdateDTO.getValidateByDs()));
    } else {
      categoryKeywordsUpdateDTOS.stream().filter(
          categoryKeywordsUpdateDTO -> StringUtils.isEmpty(categoryKeywordsUpdateDTO.getKeywordId()))
        .filter(categoryKeywordsUpdateDTO -> StringUtils.isNotBlank(
          categoryKeywordsUpdateDTO.getKeyword())).forEach(
          categoryKeywordsUpdateDTO -> newKeywordsAddedToCategoryValidateDsMap.put(
            categoryKeywordsUpdateDTO.getKeyword(), categoryKeywordsUpdateDTO.getValidateByDs()));
    }
    Set<String> addedRestrictedKeywordId = new HashSet<>();
    if (MapUtils.isNotEmpty(newKeywordsAddedToCategoryValidateDsMap)) {
      newlySavedRestrictedKeywordHistoryPair =
        saveRestrictedKeywords(newKeywordsAddedToCategoryValidateDsMap, storeId);
      if (removeDeletedKeywordsFromAddList) {
        List<RestrictedKeyword> savedRestrictedKeywordHistoryPairLeft =
            newlySavedRestrictedKeywordHistoryPair.getLeft();
        List<String> deletedKeywords =
            deletedCategoryKeywordsUpdateDTOs.stream().map(CategoryKeywordsUpdateDTO::getKeyword)
                .filter(StringUtils::isNotBlank).toList();
        List<RestrictedKeyword> newlyAddedRestrictedKeywords =
            savedRestrictedKeywordHistoryPairLeft.stream().filter(
                    restrictedKeyword -> !deletedKeywords.contains(restrictedKeyword.getKeyword()))
                .toList();
        newlySavedRestrictedKeywordHistoryPair = Pair.of(newlyAddedRestrictedKeywords,
            newlySavedRestrictedKeywordHistoryPair.getRight());
      }
      addedRestrictedKeywordId.addAll(
        newlySavedRestrictedKeywordHistoryPair.getLeft().stream().map(RestrictedKeyword::getId)
          .collect(Collectors.toList()));
    }
    List<String> keywordsAddedToCategory = categoryKeywordsUpdateDTOS.stream()
        .filter(categoryKeywordsUpdateDTO -> StringUtils.isNotBlank(categoryKeywordsUpdateDTO.getKeywordId()))
        .map(CategoryKeywordsUpdateDTO::getKeywordId).collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(keywordsAddedToCategory)) {
      addedRestrictedKeywordId.addAll(keywordsAddedToCategory);
    }
    List<RestrictedKeyword> restrictedKeywordsMap = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(addedRestrictedKeywordId)) {
      restrictedKeywordsMap = getRestrictedKeywordsList(storeId, new ArrayList<>(addedRestrictedKeywordId));
    }
    return Pair.of(restrictedKeywordsMap, newlySavedRestrictedKeywordHistoryPair.getRight());
  }

  private List<RestrictedKeyword> getRestrictedKeywordsList(String storeId, List<String> addedKeywordId) {
    List<RestrictedKeyword> restrictedKeywordList = restrictedKeywordService.findByKeywordId(storeId, addedKeywordId);
    if (addedKeywordId.size() != restrictedKeywordList.size()) {
      List<String> notFoundKeywordsIds = new ArrayList<>(addedKeywordId);
      notFoundKeywordsIds.removeAll(
          restrictedKeywordList.stream().map(RestrictedKeyword::getId).collect(Collectors.toList()));
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, KEYWORDS_NOT_FOUND + notFoundKeywordsIds);
    }
    return restrictedKeywordList;
  }

  private Pair<List<RestrictedKeyword>, List<RestrictedKeywordHistoryEventModel>> saveRestrictedKeywords(
    Map<String, Boolean> keywordValidateDsMap, String storeId) {
    List<String> firstTimeKeywords = new ArrayList<>();
    List<RestrictedKeyword> alreadySavedKeywords = new ArrayList<>();
    List<RestrictedKeyword> newlySavedRestrictedKeywords = new ArrayList<>();
    List<RestrictedKeyword> updateRestrictedKeywordFlag = new ArrayList<>();
    List<RestrictedKeyword> allSavedRestrictedKeywordFlag;
    List<RestrictedKeywordHistoryEventModel> keywordHistoryEventModelList = new ArrayList<>();
    for (Map.Entry<String, Boolean> keywordValidateByDs : keywordValidateDsMap.entrySet()) {
      RestrictedKeyword savedKeyword =
        restrictedKeywordService.findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(storeId,
          keywordValidateByDs.getKey());
      if (Objects.isNull(savedKeyword)) {
        firstTimeKeywords.add(keywordValidateByDs.getKey());
      } else {
        alreadySavedKeywords.add(savedKeyword);
        populateRestrictedKeywordUpdateList(savedKeyword,  keywordValidateByDs.getValue(),
          updateRestrictedKeywordFlag, keywordHistoryEventModelList);
      }
    }
    if (CollectionUtils.isNotEmpty(firstTimeKeywords) || CollectionUtils.isNotEmpty(
      updateRestrictedKeywordFlag)) {
      List<RestrictedKeyword> restrictedKeywordList =
        ConverterUtil.toRestrictedKeywords(firstTimeKeywords, storeId, keywordValidateDsMap);
      restrictedKeywordList.addAll(updateRestrictedKeywordFlag);
      log.info("Saving the newRestrictedKeywords : {}", restrictedKeywordList);
      allSavedRestrictedKeywordFlag = restrictedKeywordService.saveRestrictedKeywords(restrictedKeywordList);
      if (!restrictedKeywordListChange) {
        allSavedRestrictedKeywordFlag.removeAll(alreadySavedKeywords);
      }
      newlySavedRestrictedKeywords.addAll(allSavedRestrictedKeywordFlag);
    }
    if (restrictedKeywordListChange) {
      newlySavedRestrictedKeywords.addAll(alreadySavedKeywords);
    }
    return Pair.of(newlySavedRestrictedKeywords, keywordHistoryEventModelList);
  }

  @Override
  public Page<RestrictedKeywordsResponse> findCategoryRestrictedKeywords(String storeId,
      CategoryRestrictedKeywordsRequest categoryRestrictedKeywordsRequest, Pageable pageable) {
    List<RestrictedKeywordsResponse> restrictedKeywordsResponseList = new ArrayList<>();
    long totalCount = 0;
    Set<String> destinationCategoryCodes = new HashSet<>();
    if (StringUtils.isNotEmpty(categoryRestrictedKeywordsRequest.getKeyword())) {
      Page<RestrictedKeyword> restrictedKeywordsPage = restrictedKeywordService
          .getRestrictedKeywordSuggestions(storeId, categoryRestrictedKeywordsRequest.getKeyword(), pageable);
      totalCount = restrictedKeywordsPage.getTotalElements();
      restrictedKeywordsResponseList = restrictedKeywordsPage.getContent().stream()
          .map(ConverterUtil::toRestrictedKeywordsResponseFromRestrictedKeyword).collect(Collectors.toList());
    }
    if (StringUtils.isNotEmpty(categoryRestrictedKeywordsRequest.getCategoryCode()) && StringUtils
        .isEmpty(categoryRestrictedKeywordsRequest.getKeyword())) {
      Page<CategoryRestrictedKeyword> categoryRestrictedKeywords = categoryRestrictedKeywordService
          .findByStoreIdAndCategoryCode(storeId, categoryRestrictedKeywordsRequest.getCategoryCode(), pageable);
      totalCount = categoryRestrictedKeywords.getTotalElements();
      restrictedKeywordsResponseList = categoryRestrictedKeywords.getContent().stream()
          .map(ConverterUtil::toRestrictedKeywordsResponseFromCategoryRestrictedKeyword).collect(Collectors.toList());
      destinationCategoryCodes = restrictedKeywordsResponseList.stream().filter(
          restrictedKeywordsResponse -> StringUtils.isNotBlank(restrictedKeywordsResponse.getDestinationCategory()))
          .map(RestrictedKeywordsResponse::getDestinationCategory).collect(Collectors.toSet());
    } else if (StringUtils.isNotEmpty(categoryRestrictedKeywordsRequest.getCategoryCode()) && StringUtils
        .isNotEmpty(categoryRestrictedKeywordsRequest.getKeyword())) {
      Map<String, CategoryRestrictedKeyword> restrictedKeywordIdCategoryRestrictedKeywordMap =
          categoryRestrictedKeywordService
              .findByStoreIdAndCategoryCode(storeId, categoryRestrictedKeywordsRequest.getCategoryCode()).stream()
              .filter(categoryRestrictedKeyword -> !categoryRestrictedKeyword.isMarkForDelete())
              .collect(Collectors.toMap(CategoryRestrictedKeyword::getRestrictedKeywordId, Function.identity()));
      for (RestrictedKeywordsResponse restrictedKeywordsResponse : restrictedKeywordsResponseList) {
        if (restrictedKeywordIdCategoryRestrictedKeywordMap.containsKey(restrictedKeywordsResponse.getKeywordId())) {
          restrictedKeywordsResponse.setSelected(true);
          CategoryRestrictedKeyword categoryRestrictedKeyword =
              restrictedKeywordIdCategoryRestrictedKeywordMap.get(restrictedKeywordsResponse.getKeywordId());
          restrictedKeywordsResponse.setAction(categoryRestrictedKeyword.getAction());
          restrictedKeywordsResponse.setMessage(categoryRestrictedKeyword.getMessage());
          restrictedKeywordsResponse.setType(categoryRestrictedKeyword.getType());
          restrictedKeywordsResponse.setDestinationCategory(categoryRestrictedKeyword.getDestinationCategory());
          setValidateByDs(restrictedKeywordsResponse, categoryRestrictedKeyword);
          if (StringUtils.isNotBlank(categoryRestrictedKeyword.getDestinationCategory())) {
            destinationCategoryCodes.add(categoryRestrictedKeyword.getDestinationCategory());
          }
        } else {
          restrictedKeywordsResponse.setSelected(false);
        }
      }
    }
    populateNameForDestinationCategory(storeId, restrictedKeywordsResponseList, destinationCategoryCodes);
    return new PageImpl<>(restrictedKeywordsResponseList, pageable, totalCount);
  }

  private static void setValidateByDs(RestrictedKeywordsResponse restrictedKeywordsResponse,
      CategoryRestrictedKeyword categoryRestrictedKeyword) {
    if (Objects.nonNull(categoryRestrictedKeyword.getRestrictedKeyword())) {
      restrictedKeywordsResponse.setValidateByDs(
          categoryRestrictedKeyword.getRestrictedKeyword().getValidateByDs());
    }
  }

  private void populateNameForDestinationCategory(String storeId,
      List<RestrictedKeywordsResponse> restrictedKeywordsResponseList, Set<String> destinationCategoryCodes) {
    if (CollectionUtils.isNotEmpty(destinationCategoryCodes)) {
      List<CategoryCodeAndNameDTO> categoryNameByCategoryCodes =
          categoryService.findNameByStoreIdAndCategoryCodes(storeId, new ArrayList<>(destinationCategoryCodes));
      Map<String, CategoryCodeAndNameDTO> categoryCodeAndNameDTOMap = categoryNameByCategoryCodes.stream().collect(
          Collectors.toMap(CategoryCodeAndNameDTO::getCategoryCode, categoryCodeAndNameDTO -> categoryCodeAndNameDTO,
              (a, b) -> a));
      for (RestrictedKeywordsResponse restrictedKeywordsResponse : restrictedKeywordsResponseList) {
        if (categoryCodeAndNameDTOMap.containsKey(restrictedKeywordsResponse.getDestinationCategory())) {
          CategoryCodeAndNameDTO categoryCodeAndNameDTO =
              categoryCodeAndNameDTOMap.get(restrictedKeywordsResponse.getDestinationCategory());
          restrictedKeywordsResponse.setDestinationCategoryName(categoryCodeAndNameDTO.getDestinationCategoryName());
          restrictedKeywordsResponse
              .setDestinationCategoryEnglishName(categoryCodeAndNameDTO.getDestinationCategoryEnglishName());
        }
      }

    }
  }

  private void updateWholesalePriceConfigForNewCategory(String storeId,
      CategoryMappingsUpdateDTO categoryMappingsUpdateDTO, Category category, String categoryId) throws Exception {
    log.debug("updateWholesalePriceConfigForNewCategory :: CategoryMappingsUpdateDTO {}", categoryMappingsUpdateDTO);
    WholesalePriceConfiguration wholeSalePriceConfiguration = ConverterUtil
        .getWholesalePriceConfiguration(storeId, categoryMappingsUpdateDTO.getWholesaleMapping(), category);
    wholeSalePriceConfiguration.setCategoryId(categoryId);
    log.debug("adding the wholesale price configuration mapping {} with Id : {} for new category : {}",
        wholeSalePriceConfiguration, category.getCategoryCode());
    categoryWholesaleConfigService.updateCategoryWholesaleConfiguration(storeId, wholeSalePriceConfiguration);
  }

  @Override
  @Transactional(readOnly = false)
  public CategoryUpdateHistoryDTO updateCategoriesWithWholesaleConfig(String storeId,
      String categoryId, WholesaleMappingDTO wholesaleMappingDTO) throws Exception {
    Category category = categoryService.findByStoreIdAndId(storeId, categoryId);
    if (Objects.isNull(category)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, CATEGORY_NOT_FOUND + categoryId);
    }
    WholesalePriceConfiguration wholesalePriceConfiguration = ConverterUtil
        .getWholesalePriceConfiguration(storeId, wholesaleMappingDTO, category);
    log.debug("adding the wholesale config : {} for CategoryId : {}",
        wholesaleMappingDTO, category.getId());
    CategoryUpdateHistoryDTO categoryUpdateHistoryDTO =
        categoryWholesaleConfigService.updateCategoryWholesaleMappings(storeId,
        wholesalePriceConfiguration, category, true);
    categoryWholesaleConfigService.updateWholesaleConfigForChildCategories(storeId,
        wholesalePriceConfiguration, category);
    return categoryUpdateHistoryDTO;
  }

  @Override
  public void publishHistoryEventForWholesaleConfigUpdate(String storeId,
      CategoryUpdateHistoryDTO categoryUpdateHistoryDTO) {
    domainEventPublisherService.publishCategoryUpdateHistory(Collections.singletonList(
        com.gdn.x.productcategorybase.util.ConverterUtil.constructCategoryHistoryEventModel(
            categoryUpdateHistoryDTO.getPreviousValue(),
            categoryUpdateHistoryDTO.getCurrentValue(),
            categoryUpdateHistoryDTO.getCategory().getCategoryCode(), storeId,
            mandatoryParameterHelper.getUsername(),
            CategoryUpdateHistoryActivity.WHOLESALE_CONFIGURATION.name())));
  }

  @Override
  public String getFinalParentCategoryCached(String storeId, String categoryId) throws Exception {
    Category category = categoryService.findByStoreIdAndId(storeId, categoryId);
    if (Objects.isNull(category)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, CATEGORY_NOT_FOUND + categoryId);
    }
    List<Category> categoryHierarchy =
        categoryService.findCategoryHierarchyByCategoryCode(storeId, category.getCategoryCode());
    Category finalParent = categoryHierarchy.get(categoryHierarchy.size() - 1);
    return finalParent.getCategoryCode();
  }

  @Override
  public List<CategoryTreeDTO> getActiveCategoryTree(
      String catalogName, String storeId, String nonInventoryCode) {
    if (StringUtils.isEmpty(catalogName)) {
      catalogName = CatalogType.MASTER_CATALOG.toString();
    }
    List<CategoryTreeDTO> catList = categoryService.getActiveCategoryTree(catalogName, storeId).stream()
        .filter(categoryTreeDTO -> !nonInventoryCode.equals(categoryTreeDTO.getCategoryCode()))
        .collect(Collectors.toList());
    return categoryService.buildCategoryTree(catList, StringUtils.EMPTY);
  }

  @Override
  public CategoryErrorDto getCategoryDetailByCategoryId(String storeId, String categoryId) {
    String errorCode = StringUtils.EMPTY;
    String errorMessage = StringUtils.EMPTY;
    Category category = this.categoryService.findByStoreIdAndIdInitCategoryAttribute(storeId, categoryId);
    return new CategoryErrorDto(category, errorCode, errorMessage);
  }

  @Override
  public List<Category> getCategoryAndDefiningAttributes(String storeId,
      List<String> categoryCodes) {
    List<Category> result = null;
    if (CollectionUtils.isNotEmpty(categoryCodes)) {
      result =
          categoryService.getCategoryAttributesByStoreIdAndCategoryCodes(storeId, categoryCodes);
    }
    GdnPreconditions.checkArgument(Objects.nonNull(result) && CollectionUtils.isNotEmpty(result),
        ErrorMessage.CATEGORY_NOT_FOUND.getMessage() + categoryCodes);
    return result;
  }

  @Override
  public CategoryErrorDto validateCategory(String storeId, String categoryId) {
    Category category = categoryService.findByStoreIdAndId(storeId, categoryId);
    if (Objects.isNull(category)) {
      return new CategoryErrorDto(category, ErrorMessage.INVALID_CATEGORY_ERROR_CODE.getMessage(),
          ErrorMessage.INVALID_CATEGORY_ERROR_MESSAGE.getMessage());
    } else {
      long childCounts = categoryService.findActiveChildCountForParent(storeId, category);
      if (!category.isActivated() || childCounts > 0) {
        return new CategoryErrorDto(category, ErrorMessage.INVALID_CATEGORY_ERROR_CODE.getMessage(),
            ErrorMessage.INVALID_CATEGORY_ERROR_MESSAGE.getMessage());
      } else {
        return new CategoryErrorDto(category, StringUtils.EMPTY, StringUtils.EMPTY);
      }
    }
  }

  @Override
  public List<CategoryErrorResponse> validateCategoryForRestrictedKeywordCategoryChange(String storeId,
      List<String> categoryCodes) {
    List<CategoryErrorResponse> categoryErrorDtoList = new ArrayList<>();
    List<Category> categoryList =
        categoryService.findCategoriesByStoreIdAndCategoryCodes(storeId, new ArrayList<>(categoryCodes));
    if (CollectionUtils.isEmpty(categoryList)) {
      categoryCodes.forEach(categoryCode -> categoryErrorDtoList.add(new CategoryErrorResponse(categoryCode,
          String.format(ErrorMessage.CATEGORY_DO_NOT_EXISTS.getMessage(), categoryCode))));
    } else if (categoryList.size() < categoryCodes.size()) {
      Set<String> foundCategoryCodes = categoryList.stream().map(Category::getCategoryCode).collect(Collectors.toSet());
      categoryCodes.removeAll(foundCategoryCodes);
      categoryCodes.forEach(categoryCode -> categoryErrorDtoList.add(new CategoryErrorResponse(categoryCode,
          String.format(ErrorMessage.CATEGORY_DO_NOT_EXISTS.getMessage(), categoryCode))));
      categoryIsLeafAndMasterCategory(storeId, categoryList, categoryErrorDtoList);
    } else {
      categoryIsLeafAndMasterCategory(storeId, categoryList, categoryErrorDtoList);
    }
    return categoryErrorDtoList;
  }

  private void categoryIsLeafAndMasterCategory(String storeId, List<Category> categoryList,
      List<CategoryErrorResponse> categoryErrorResponseList) {
    for (Category category : categoryList) {
      if (!category.isActivated()) {
        categoryErrorResponseList.add(new CategoryErrorResponse(category.getCategoryCode(),
            String.format(ErrorMessage.CATEGORY_IS_NOT_ACTIVATED.getMessage(), category.getCategoryCode())));
      } else if (Constants.SALES_CATALOG.equals(category.getCatalog().getCatalogType().name())) {
        categoryErrorResponseList.add(new CategoryErrorResponse(category.getCategoryCode(),
            String.format(ErrorMessage.CATEGORY_IS_NOT_MASTER_CATEGORY.getMessage(), category.getCategoryCode())));
      } else if (categoryService.findActiveChildCountForParent(storeId, category) > 0) {
        categoryErrorResponseList.add(new CategoryErrorResponse(category.getCategoryCode(),
            String.format(ErrorMessage.CATEGORY_IS_NOT_LEAF.getMessage(), category.getCategoryCode())));
      }
    }
  }

  @Override
  @Cacheable(cacheManager = Constants.RESTRICTED_KEYWORD_CACHE_MANAGER, value =
      CacheNames.RESTRICTED_KEYWORDS_CACHE, key = "#storeId +'_'+ #categoryCode", unless =
      "#result == null")
  public List<RestrictedKeywordsMappedToCategoryResponse> getRestrictedKeywordMappedToCategory(String storeId, String categoryCode) {
    List<RestrictedKeywordsMappedToCategoryResponse> restrictedKeywordsMappedToCategoryResponses = new ArrayList<>();
    for (CategoryRestrictedKeyword categoryRestrictedKeyword : this.categoryRestrictedKeywordService
        .findByStoreIdAndCategoryCode(storeId, categoryCode)) {
      if (!categoryRestrictedKeyword.isMarkForDelete()) {
        RestrictedKeyword restrictedKeyword = categoryRestrictedKeyword.getRestrictedKeyword();
        restrictedKeywordsMappedToCategoryResponses.add(
            new RestrictedKeywordsMappedToCategoryResponse(categoryRestrictedKeyword.getId(),
                restrictedKeyword.getKeyword(), categoryRestrictedKeyword.getAction(),
                categoryRestrictedKeyword.getDestinationCategory(),
                Optional.ofNullable(restrictedKeyword.getValidateByDs()).orElse(false),
                categoryRestrictedKeyword.getRestrictedKeywordId(),
                categoryRestrictedKeyword.getType()));
      }
    }
    return restrictedKeywordsMappedToCategoryResponses;
  }

  public void updateB2bExclusiveFlag(Category category, Category parentCategory, boolean b2bExclusive) {
    category.setB2bExclusive(b2bExclusive);
    if (Objects.nonNull(parentCategory) && parentCategory.isB2bExclusive()) {
      category.setB2bExclusive(true);
    }
  }

  public void updateHalalFlag(String storeId, Category category, Category parentCategory, boolean halalCategory, boolean newCategory) {
    if (halalCategory) {
      if (Objects.isNull(parentCategory) || parentCategory.isHalalCategory())
        category.setHalalCategory(true);
    } else {
      category.setHalalCategory(false);
      if (Objects.nonNull(parentCategory) && !newCategory) {
        long countOfHalalChildCategories =
                categoryService.findCountByParentCategoryAndHalalCategory(storeId, parentCategory, true);
        if (countOfHalalChildCategories <= 1) {
          updateHalalFlag(storeId, parentCategory, parentCategory.getParentCategory(), false, false);
        }
      }
    }
  }

  public static void populateRestrictedKeywordUpdateList(RestrictedKeyword restrictedKeyword,
    Boolean validateByDs, List<RestrictedKeyword> updateRestrictedKeywordList,
    List<RestrictedKeywordHistoryEventModel> keywordHistoryEventModelList) {
    if (!Objects.equals(validateByDs, restrictedKeyword.getValidateByDs())) {
      keywordHistoryEventModelList.add(
        CommonUtil.populateHistoryModel(restrictedKeyword, validateByDs));
      restrictedKeyword.setValidateByDs(validateByDs);
      updateRestrictedKeywordList.add(restrictedKeyword);
    }
  }

  private void populateCategoryHistoryChange(Category savedCategory,
    CategoryInfoUpdateDTO categoryInfoUpdateDTO,
    List<CategoryHistoryEventModel> historyEventModelList, String categoryCode, String storeId,
    String username, boolean statusChangeFlag) {
    if (!Objects.equals(savedCategory.getDocumentType(), categoryInfoUpdateDTO.getDocumentType())) {
      historyEventModelList.add(
        ConverterUtil.constructCategoryHistoryEventModel(savedCategory.getDocumentType(),
          categoryInfoUpdateDTO.getDocumentType(), categoryCode, storeId, username,
          CategoryUpdateHistoryActivity.SUPPORT_DOCUMENT_CHANGE.name()));
    }
    if (!savedCategory.getName().equals(categoryInfoUpdateDTO.getName())) {
      historyEventModelList.add(
        ConverterUtil.constructCategoryHistoryEventModel(savedCategory.getName(),
          categoryInfoUpdateDTO.getName(), categoryCode, storeId, username,
          CategoryUpdateHistoryActivity.CATEGORY_NAME_CHANGED.name()));
    }
    if (StringUtils.isBlank(savedCategory.getNameEnglish())) {
      savedCategory.setNameEnglish(StringUtils.EMPTY);
    }
    if (!savedCategory.getNameEnglish().equals(categoryInfoUpdateDTO.getNameEnglish())) {
      historyEventModelList.add(
        ConverterUtil.constructCategoryHistoryEventModel(savedCategory.getNameEnglish(),
          categoryInfoUpdateDTO.getNameEnglish(), categoryCode, storeId, username,
          CategoryUpdateHistoryActivity.CATEGORY_NAME_ENGLISH_CHANGED.name()));
    }
    if (savedCategory.isGenericTemplateEligible()
      != categoryInfoUpdateDTO.isGenericTemplateEligible()) {
      historyEventModelList.add(ConverterUtil.constructCategoryHistoryEventModel(
        String.valueOf(savedCategory.isGenericTemplateEligible()),
        String.valueOf(categoryInfoUpdateDTO.isGenericTemplateEligible()), categoryCode, storeId,
        username, CategoryUpdateHistoryActivity.GENERIC_TEMPLATE_FLAG_CHANGED.name()));
    }
    if (!Arrays.equals(savedCategory.getDefaultDescription(),
      categoryInfoUpdateDTO.getDefaultDescription())) {
      String savedDescription = fetchDescription(savedCategory.getDefaultDescription());
      historyEventModelList.add(ConverterUtil.constructCategoryHistoryEventModel(savedDescription,
        new String(categoryInfoUpdateDTO.getDefaultDescription(), StandardCharsets.UTF_8),
        categoryCode, storeId, username, CategoryUpdateHistoryActivity.DEFAULT_DESCRIPTION.name()));
    }
    if (!Arrays.equals(savedCategory.getDescriptionEnglish(),
      categoryInfoUpdateDTO.getDescriptionEnglish())) {
      String savedDescription = fetchDescription(savedCategory.getDescriptionEnglish());
      historyEventModelList.add(ConverterUtil.constructCategoryHistoryEventModel(savedDescription,
        new String(categoryInfoUpdateDTO.getDescriptionEnglish(), StandardCharsets.UTF_8),
        categoryCode, storeId, username, CategoryUpdateHistoryActivity.DESCRIPTION_ENGLISH.name()));
    }
    if (savedCategory.getDangerousGoodsLevel() != categoryInfoUpdateDTO.getDangerousGoodsLevel()) {
      historyEventModelList.add(ConverterUtil.constructCategoryHistoryEventModel(
        String.valueOf(savedCategory.getDangerousGoodsLevel()),
        String.valueOf(categoryInfoUpdateDTO.getDangerousGoodsLevel()), categoryCode, storeId,
        username, CategoryUpdateHistoryActivity.DANGEROUS_GOOD_LEVEL.name()));
    }
    if (Objects.nonNull(categoryInfoUpdateDTO.getB2bExclusive()) && (savedCategory.isB2bExclusive()
      != categoryInfoUpdateDTO.getB2bExclusive())) {
      historyEventModelList.add(ConverterUtil.constructCategoryHistoryEventModel(
        String.valueOf(savedCategory.isB2bExclusive()),
        String.valueOf(categoryInfoUpdateDTO.getB2bExclusive()), categoryCode, storeId, username,
        CategoryUpdateHistoryActivity.BFB_FLAG_CHANGED.name()));
    }
    if (statusChangeFlag && savedCategory.isActivated() != categoryInfoUpdateDTO.isActivated()) {
      String status = savedCategory.isActivated() ?
        CategoryUpdateHistoryActivity.CATEGORY_DEACTIVATED.name() :
        CategoryUpdateHistoryActivity.CATEGORY_ACTIVATED.name();
      historyEventModelList.add(ConverterUtil.constructCategoryHistoryEventModel(
        String.valueOf(savedCategory.isActivated()),
        String.valueOf(categoryInfoUpdateDTO.isActivated()), categoryCode, storeId, username,
        status));
    }
    if (!Objects.equals(savedCategory.getLogisticAdjustment(),
      categoryInfoUpdateDTO.getLogisticAdjustment())) {
      historyEventModelList.add(ConverterUtil.constructCategoryHistoryEventModel(
        String.valueOf(savedCategory.getLogisticAdjustment()),
        String.valueOf(categoryInfoUpdateDTO.getLogisticAdjustment()), categoryCode, storeId, username,
        CategoryUpdateHistoryActivity.LOGISTIC_ADJUSTMENT.name()));
    }
    if (savedCategory.isBopisEligible() != categoryInfoUpdateDTO.isBopisEligible()) {
      historyEventModelList.add(ConverterUtil.constructCategoryHistoryEventModel(
        String.valueOf(savedCategory.isBopisEligible()),
        String.valueOf(categoryInfoUpdateDTO.isBopisEligible()), categoryCode, storeId, username,
        CategoryUpdateHistoryActivity.BOPIS_ELIGIBLE_CHANGED.name()));
    }
  }

  private static void populateHistoryForWholeSaleConfigToggle(Category savedCategory, CategoryInfoUpdateDTO categoryInfoUpdateDTO,
      List<CategoryHistoryEventModel> historyEventModelList, String categoryCode, String storeId,
      String username) {
    if (savedCategory.isWholesalePriceConfigEnabled()
        != categoryInfoUpdateDTO.isWholesalePriceConfigEnabled()) {
      String status = savedCategory.isWholesalePriceConfigEnabled() ?
          CategoryUpdateHistoryActivity.WHOLESALE_CONFIG_DEACTIVATED.name() :
          CategoryUpdateHistoryActivity.WHOLESALE_CONFIG_ACTIVATED.name();
      historyEventModelList.add(ConverterUtil.constructCategoryHistoryEventModel(
          String.valueOf(savedCategory.isWholesalePriceConfigEnabled()),
          String.valueOf(categoryInfoUpdateDTO.isWholesalePriceConfigEnabled()), categoryCode,
          storeId, username, status));
    }
  }

  private void populateOscChangeHistory(String categoryCode, String previousValue,
    String currentValue, String username, String storeId,
    List<CategoryHistoryEventModel> historyEventModelList) {
    historyEventModelList.add(
      ConverterUtil.constructCategoryHistoryEventModel(previousValue, currentValue, categoryCode,
        storeId, username, CategoryUpdateHistoryActivity.OSC_ID_CHANGED.name()));
  }

  private void populateParentCategoryChangeHistory(String categoryCode, String previousValue,
    String currentValue, String username, String storeId,
    List<CategoryHistoryEventModel> historyEventModelList) {
    historyEventModelList.add(
      ConverterUtil.constructCategoryHistoryEventModel(previousValue, currentValue, categoryCode,
        storeId, username, CategoryUpdateHistoryActivity.PARENT_CATEGORY.name()));
  }

  private void populateShippingChangeHistory(ShippingRequest shippingRequest,
    CategoryInfoUpdateDTO categoryInfoUpdateDTO, String storeId, String username,
    List<CategoryHistoryEventModel> historyEventModelList) {
    if (!Objects.equals(shippingRequest.getAgeLimit(), categoryInfoUpdateDTO.getAgeLimit())) {
      historyEventModelList.add(ConverterUtil.constructCategoryHistoryEventModel(
        String.valueOf(shippingRequest.getAgeLimit()),
        String.valueOf(categoryInfoUpdateDTO.getAgeLimit()),
        categoryInfoUpdateDTO.getCategoryCode(), storeId, username,
        CategoryUpdateHistoryActivity.AGE_LIMIT_CHANGED.name()));
    }
    if (shippingRequest.isSpecialHandling() != categoryInfoUpdateDTO.isSpecialHandling()) {
      historyEventModelList.add(ConverterUtil.constructCategoryHistoryEventModel(
        String.valueOf(shippingRequest.isSpecialHandling()),
        String.valueOf(categoryInfoUpdateDTO.isSpecialHandling()),
        categoryInfoUpdateDTO.getCategoryCode(), storeId, username,
        CategoryUpdateHistoryActivity.SPECIAL_HANDLING_CHANGED.name()));
    }
    if (shippingRequest.isDirectFlight() != categoryInfoUpdateDTO.isDirectFlight()) {
      historyEventModelList.add(ConverterUtil.constructCategoryHistoryEventModel(
        String.valueOf(shippingRequest.isDirectFlight()),
        String.valueOf(categoryInfoUpdateDTO.isDirectFlight()),
        categoryInfoUpdateDTO.getCategoryCode(), storeId, username,
        CategoryUpdateHistoryActivity.DIRECT_FLIGHT_CHANGED.name()));
    }
    if (shippingRequest.isSizeChartRequired() != categoryInfoUpdateDTO.isSizeChartRequired()) {
      historyEventModelList.add(ConverterUtil.constructCategoryHistoryEventModel(
        String.valueOf(shippingRequest.isSizeChartRequired()),
        String.valueOf(categoryInfoUpdateDTO.isSizeChartRequired()),
        categoryInfoUpdateDTO.getCategoryCode(), storeId, username,
        CategoryUpdateHistoryActivity.SIZE_CHART_REQUIRED_CHANGED.name()));
    }
  }
}
