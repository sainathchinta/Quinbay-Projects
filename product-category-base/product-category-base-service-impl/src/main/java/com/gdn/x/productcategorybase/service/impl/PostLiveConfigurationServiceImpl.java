package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.MerchantSearchResponse;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.response.BulkCategoryConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;
import com.gdn.x.productcategorybase.dto.response.BulkMerchantConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.ConfigurationCountResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryConfiguration;
import com.gdn.x.productcategorybase.entity.CategoryConfigurationHistory;
import com.gdn.x.productcategorybase.entity.MerchantConfiguration;
import com.gdn.x.productcategorybase.entity.MerchantConfigurationHistory;
import com.gdn.x.productcategorybase.repository.CategoryConfigurationRepository;
import com.gdn.x.productcategorybase.repository.MerchantConfigurationRepository;
import com.gdn.x.productcategorybase.service.BusinessPartner.BusinessPartnerService;
import com.gdn.x.productcategorybase.service.CategoryConfigurationHistoryService;
import com.gdn.x.productcategorybase.service.CategoryConfigurationService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.MerchantConfigurationHistoryService;
import com.gdn.x.productcategorybase.service.MerchantConfigurationService;
import com.gdn.x.productcategorybase.service.PostLiveConfigurationService;
import com.gdn.x.productcategorybase.util.ConverterUtil;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@Transactional(readOnly = true)
public class PostLiveConfigurationServiceImpl implements PostLiveConfigurationService {

  private static String MERCHANT = "merchant";
  private static String CATEGORY = "category";
  private static String ACTIVE = "ACTIVE";

  @Autowired
  private CategoryConfigurationRepository categoryConfigurationRepository;

  @Autowired
  private MerchantConfigurationRepository merchantConfigurationRepository;

  @Autowired
  private MerchantConfigurationService merchantConfigurationService;

  @Autowired
  private MerchantConfigurationHistoryService merchantConfigurationHistoryService;

  @Autowired
  private CategoryConfigurationService categoryConfigurationService;

  @Autowired
  private CategoryConfigurationHistoryService categoryConfigurationHistoryService;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  private static final int PAGE_SIZE = 50;
  private static final int FIRST_PAGE = 0;

  @Value("${review.configuration.category.default}")
  private String categoryDefault;

  @Value("${review.configuration.merchant.default}")
  private String merchantDefault;

  @Override
  public List<MerchantSearchResponse> fetchMerchantConfiguration(
      List<MerchantConfigurationRequest> merchantConfigurationRequestList) {
    List<MerchantSearchResponse> merchantSearchResponseList = new ArrayList<>();
    List<String> businessPartnerCodeList =
        Optional.ofNullable(merchantConfigurationRequestList).orElse(new ArrayList<>()).stream()
            .map(MerchantConfigurationRequest::getBusinessPartnerCode).collect(Collectors.toList());
    Map<String, String> businessPartnerCodeAndNameMap =
        Optional.ofNullable(merchantConfigurationRequestList).orElse(new ArrayList<>()).stream().collect(Collectors
            .toMap(MerchantConfigurationRequest::getBusinessPartnerCode,
                MerchantConfigurationRequest::getBusinessPartnerName));
    List<MerchantConfiguration> merchantConfigurationList =
        this.merchantConfigurationRepository.findByMerchantCodeInAndMarkForDeleteFalse(businessPartnerCodeList);
    for (MerchantConfiguration merchantConfiguration : merchantConfigurationList) {
      merchantSearchResponseList.add(
          new MerchantSearchResponse(merchantConfiguration.getMerchantCode(), merchantConfiguration.getMerchantName(),
              merchantConfiguration.getReviewConfig()));
    }
    defaultValuesForUnrecordedMerchants(merchantSearchResponseList, businessPartnerCodeList,
        businessPartnerCodeAndNameMap);
    return merchantSearchResponseList;
  }

  @Override
  public ConfigurationCountResponse fetchConfigurationCounts(String storeId) {
    return new ConfigurationCountResponse(
        this.categoryConfigurationService.getCategoryConfigurationCount(storeId),
        this.merchantConfigurationService.getMerchantConfigurationCount(storeId));
  }

  @Override
  public Page<CategoryConfigurationFilterResponse> getCategoryConfigurationList(String storeId,
      ConfigurationFilterRequest configurationFilterRequest, int page, int size) {
    Pageable pageable = PageRequest.of(page, size);
    Page<CategoryConfiguration> categoryConfigurationPage =
        this.categoryConfigurationService.getCategoryConfigurationList(storeId, configurationFilterRequest, pageable);
    return new PageImpl<>(
        ConverterUtil.toCategoryConfigurationFilterResponseList(categoryConfigurationPage.getContent()), pageable,
        categoryConfigurationPage.getTotalElements());
  }

  @Override
  public Page<MerchantConfigurationFilterResponse> getMerchantConfigurationList(String storeId,
      ConfigurationFilterRequest configurationFilterRequest, int page, int size) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    Page<MerchantConfiguration> merchantConfigurationPage =
        this.merchantConfigurationService.getMerchantConfigurationPage(storeId, configurationFilterRequest, pageable);
    return new PageImpl<>(
        ConverterUtil.toMerchantConfigurationFilterResponseList(merchantConfigurationPage.getContent()), pageable,
        merchantConfigurationPage.getTotalElements());
  }

  @Override
  public Page<CategoryConfigurationHistoryResponse> getCategoryConfigurationHistory(String storeId, String categoryCode,
      int page, int size) {
    Page<CategoryConfigurationHistory> categoryConfigurationHistoryPage =
        this.categoryConfigurationHistoryService.getCategoryConfigurationHistory(storeId, categoryCode, page, size);
    return new PageImpl<>(Optional.ofNullable(categoryConfigurationHistoryPage.getContent()).orElse(new ArrayList<>()).stream().map(
        ConverterUtil::toCategoryConfigurationHistoryResponse)
        .collect(Collectors.toList()), PageRequest.of(page, size), categoryConfigurationHistoryPage.getTotalElements());
  }

  private void defaultValuesForUnrecordedMerchants(List<MerchantSearchResponse> merchantSearchResponseList,
      List<String> businessPartnerCodeList, Map<String, String> businessPartnerCodeAndNameMap) {
    List<String> remainderBusinessPartnerCodeList = new ArrayList<>(businessPartnerCodeList);
    remainderBusinessPartnerCodeList.removeAll(
        Optional.ofNullable(merchantSearchResponseList).orElse(new ArrayList<>()).stream()
            .map(MerchantSearchResponse::getBusinessPartnerCode)
            .collect(Collectors.toList()));
    for (String remainderBusinessPartnerCode : remainderBusinessPartnerCodeList) {
      merchantSearchResponseList.add(new MerchantSearchResponse(remainderBusinessPartnerCode,
          businessPartnerCodeAndNameMap.get(remainderBusinessPartnerCode), merchantDefault));
    }
  }

  @Override
  @Transactional(readOnly = false)
  public void addCategoryConfiguration(String storeId, List<CategoryConfigurationRequest> requests) throws Exception {
    List<String> categoryCodeList =
        requests.stream().map(CategoryConfigurationRequest::getCategoryCode).collect(Collectors.toList());
    List<Category> categoryList = this.categoryService.findByStoreIdAndCategoryCodes(storeId, categoryCodeList);
    Map<String, CategoryConfiguration> categoryConfigurationMap =
        this.categoryConfigurationService.getCategoryConfigurationsByCategoryList(storeId, categoryList).stream()
            .collect(Collectors.toMap(categoryConfiguration -> categoryConfiguration.getCategory().getCategoryCode(),
                Function.identity()));
    Map<String, Category> categoryMap =
        categoryList.stream().collect(Collectors.toMap(Category::getCategoryCode, Function.identity()));

    List<CategoryConfiguration> categoryConfigurations = new ArrayList<>();
    List<CategoryConfigurationHistory> categoryConfigurationHistories = new ArrayList<>();

    for (CategoryConfigurationRequest request : requests) {
      checkCategoryReviewConfigFlag(request.getReviewConfig(), request.getCategoryCode());
      if (Objects.nonNull(categoryMap.get(request.getCategoryCode())) && !categoryMap.get(request.getCategoryCode())
          .isActivated()) {
        throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
            ErrorMessage.IN_ACTIVE_CATEGORY_ADD_ERROR.getMessage() + request.getCategoryCode());
      }
      if (!categoryConfigurationMap.containsKey(request.getCategoryCode())) {
        CategoryConfiguration categoryEntity =
            setNewCategoryConfiguration(categoryMap.get(request.getCategoryCode()), request.getReviewConfig(), storeId,
                GdnMandatoryParameterUtil.getUsername());
        categoryConfigurations.add(categoryEntity);
        categoryConfigurationHistories.add(ConverterUtil
            .generateCategoryConfigurationHistory(GdnMandatoryParameterUtil.getUsername(),
                request.getCategoryCode(), categoryMap.get(request.getCategoryCode()).getName(), categoryDefault,
                request.getReviewConfig(), Constants.ACTIVITY, storeId));
      } else {
        if (!categoryConfigurationMap.get(request.getCategoryCode()).getReviewConfig().equals(request.getReviewConfig())
            || categoryConfigurationMap.get(request.getCategoryCode()).isMarkForDelete()) {
          String oldValue = categoryConfigurationMap.get(request.getCategoryCode()).getReviewConfig();
          CategoryConfiguration existingCategory =
              setExistingCategoryConfiguration(categoryConfigurationMap.get(request.getCategoryCode()),
                  request.getReviewConfig(), GdnMandatoryParameterUtil.getUsername());
          categoryConfigurations.add(existingCategory);
          categoryConfigurationHistories.add(ConverterUtil
              .generateCategoryConfigurationHistory(GdnMandatoryParameterUtil.getUsername(),
                  request.getCategoryCode(), request.getCategoryName(), oldValue, request.getReviewConfig(),
                  Constants.UPDATE_ACTIVITY, storeId));
        }
      }
    }
    this.categoryConfigurationService.saveCategoryConfigurations(categoryConfigurations);
    this.categoryConfigurationHistoryService.saveCategoryHistoryConfigurations(categoryConfigurationHistories);
  }

  @Override
  @Transactional(readOnly = false)
  public void updateCategoryConfiguration(String storeId, CategoryConfigurationRequest request) {
    Category category = categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
        storeId, request.getCategoryCode());
    if (!category.isActivated()) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
          ErrorMessage.IN_ACTIVE_CATEGORY_UPDATE_ERROR.getMessage() + request.getCategoryCode());
    }
    CategoryConfiguration categoryConfiguration =
        this.categoryConfigurationService.getCategoryConfigurationByCategoryAndMarkForDeleteFalse(storeId, category);

    if (Objects.isNull(categoryConfiguration)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          ErrorMessage.CATEGORY_CONFIGURATION_NOT_FOUND.getMessage() + request.getCategoryCode());
    }
    checkCategoryReviewConfigFlag(request.getReviewConfig(), request.getCategoryCode());

    if (!categoryConfiguration.getReviewConfig().equals(request.getReviewConfig())) {
      String oldValue = categoryConfiguration.getReviewConfig();
      categoryConfiguration.setReviewConfig(request.getReviewConfig());
      categoryConfiguration.setUpdatedBy(GdnMandatoryParameterUtil.getUsername());
      this.categoryConfigurationService.saveCategoryConfiguration(categoryConfiguration);
      saveHistoryForCategory(GdnMandatoryParameterUtil.getUsername(), request.getCategoryCode(),
          category.getName(), oldValue, request.getReviewConfig(), Constants.UPDATE_ACTIVITY, storeId);
    }
  }

  @Override
  @Transactional(readOnly = false)
  public void deleteCategoryConfiguration(String storeId, String categoryCode) throws Exception {
    Category category = categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(storeId, categoryCode);
    CategoryConfiguration categoryConfiguration =
        this.categoryConfigurationService.getCategoryConfigurationByCategory(storeId, category);

    if (Objects.isNull(categoryConfiguration)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          ErrorMessage.CATEGORY_CONFIGURATION_NOT_FOUND.getMessage() + categoryCode);
    }
    if (category.isMarkForDelete()) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
          ErrorMessage.CATEGORY_DELETE_STATE.getMessage() + categoryCode);
    }
    String oldValue = categoryConfiguration.getReviewConfig();
    categoryConfiguration.setReviewConfig(categoryDefault);
    categoryConfiguration.setMarkForDelete(true);

    categoryConfiguration.setUpdatedBy(GdnMandatoryParameterUtil.getUsername());
    this.categoryConfigurationService.saveCategoryConfiguration(categoryConfiguration);
    saveHistoryForCategory(GdnMandatoryParameterUtil.getUsername(), categoryCode, category.getName(), oldValue,
        categoryDefault, Constants.UPDATE_ACTIVITY, storeId);
  }

  @Override
  @Transactional(readOnly = false)
  public void addMerchantConfiguration(String storeId, List<MerchantConfigurationRequest> requests) throws Exception {
    List<String> merchantCodeList =
        requests.stream().map(MerchantConfigurationRequest::getBusinessPartnerCode).collect(Collectors.toList());
    Map<String, MerchantConfiguration> merchantConfigurationMap =
        this.merchantConfigurationService.getMerchantConfigurationsByMerchantCodeList(storeId, merchantCodeList)
            .stream().collect(Collectors.toMap(MerchantConfiguration::getMerchantCode, Function.identity()));

    List<MerchantConfiguration> merchantConfigurations = new ArrayList<>();
    List<MerchantConfigurationHistory> merchantConfigurationHistories = new ArrayList<>();

    for (MerchantConfigurationRequest request : requests) {
      checkMerchantReviewConfigFlag(request.getReviewConfig(), request.getBusinessPartnerCode());
      ProfileResponse profileResponse =
          businessPartnerService.getBusinessPartnerProfile(request.getBusinessPartnerCode());
      if (!ACTIVE.equalsIgnoreCase(profileResponse.getMerchantStatus())) {
        throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
            ErrorMessage.IN_ACTIVE_MERCHANT_ADD_ERROR.getMessage() + request.getBusinessPartnerCode());
      }
      if (!merchantConfigurationMap.containsKey(request.getBusinessPartnerCode())) {
        MerchantConfiguration merchantEntity =
            setNewMerchantConfiguration(request, storeId, profileResponse.getCompany().getTypeOfBusiness(),
                profileResponse.getCompany().getBusinessPartnerName(), GdnMandatoryParameterUtil.getUsername());
        merchantConfigurations.add(merchantEntity);
        merchantConfigurationHistories.add(ConverterUtil
            .generateMerchantConfigurationHistory(GdnMandatoryParameterUtil.getUsername(),
                profileResponse.getCompany().getBusinessPartnerName(),
                request.getBusinessPartnerCode(), merchantDefault, request.getReviewConfig(),
                Constants.ACTIVITY, storeId));
      } else {
        if (!merchantConfigurationMap.get(request.getBusinessPartnerCode()).getReviewConfig()
            .equals(request.getReviewConfig()) || merchantConfigurationMap.get(request.getBusinessPartnerCode()).isMarkForDelete()) {
          String oldValue = merchantConfigurationMap.get(request.getBusinessPartnerCode()).getReviewConfig();
          MerchantConfiguration existingMerchant =
              setExistingMerchantConfiguration(merchantConfigurationMap.get(request.getBusinessPartnerCode()),
                  request.getReviewConfig(), GdnMandatoryParameterUtil.getUsername());
          merchantConfigurations.add(existingMerchant);
          merchantConfigurationHistories.add(ConverterUtil
              .generateMerchantConfigurationHistory(GdnMandatoryParameterUtil.getUsername(),
                  profileResponse.getCompany().getBusinessPartnerName(), request.getBusinessPartnerCode(), oldValue,
                  request.getReviewConfig(), Constants.UPDATE_ACTIVITY, storeId));
        }
      }
    }
    this.merchantConfigurationService.saveMerchantConfigurations(merchantConfigurations);
    this.merchantConfigurationHistoryService.saveMerchantHistoryConfigurations(merchantConfigurationHistories);
  }

  @Override
  @Transactional(readOnly = false)
  public void updateMerchantConfiguration(String storeId, MerchantConfigurationRequest request) throws Exception {
    ProfileResponse profileResponse =
        businessPartnerService.getBusinessPartnerProfile(request.getBusinessPartnerCode());
    if (Objects.isNull(profileResponse) || !ACTIVE.equalsIgnoreCase(profileResponse.getMerchantStatus())
        || profileResponse.isMarkForDelete()) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
          ErrorMessage.IN_ACTIVE_MERCHANT_UPDATE_ERROR.getMessage() + request.getBusinessPartnerCode());
    }
    MerchantConfiguration merchantConfiguration = this.merchantConfigurationService
        .getMerchantConfigurationByMerchantCodeAndMarkForDeleteFalse(storeId, request.getBusinessPartnerCode());
    if (Objects.isNull(merchantConfiguration)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          ErrorMessage.MERCHANT_CONFIGURATION_NOT_FOUND.getMessage() + request.getBusinessPartnerCode());
    }
    checkMerchantReviewConfigFlag(request.getReviewConfig(), request.getBusinessPartnerCode());
    if (!merchantConfiguration.getReviewConfig().equals(request.getReviewConfig())) {
      String oldValue = merchantConfiguration.getReviewConfig();
      merchantConfiguration.setReviewConfig(request.getReviewConfig());
      merchantConfiguration.setUpdatedBy(GdnMandatoryParameterUtil.getUsername());
      this.merchantConfigurationService.saveMerchantConfiguration(merchantConfiguration);
      saveHistoryForMerchant(GdnMandatoryParameterUtil.getUsername(), request.getBusinessPartnerName(),
        request.getBusinessPartnerCode(), oldValue, request.getReviewConfig(), Constants.UPDATE_ACTIVITY,
          storeId);
    }
  }

  @Override
  @Transactional(readOnly = false)
  public void deleteMerchantConfiguration(String storeId, String merchantCode) {
    MerchantConfiguration merchantConfiguration =
        this.merchantConfigurationService.getMerchantConfigurationByMerchantCode(storeId, merchantCode);
    if (Objects.isNull(merchantConfiguration)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          ErrorMessage.MERCHANT_CONFIGURATION_NOT_FOUND.getMessage() + merchantCode);
    }
    if (merchantConfiguration.isMarkForDelete()) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
          ErrorMessage.MERCHANT_DELETE_STATE.getMessage() + merchantCode);
    }
    String oldValue = merchantConfiguration.getReviewConfig();
    merchantConfiguration.setReviewConfig(merchantDefault);
    merchantConfiguration.setMarkForDelete(true);
    merchantConfiguration.setUpdatedBy(GdnMandatoryParameterUtil.getUsername());
    this.merchantConfigurationService.saveMerchantConfiguration(merchantConfiguration);
    saveHistoryForMerchant(GdnMandatoryParameterUtil.getUsername(), merchantConfiguration.getMerchantName(),
        merchantCode, oldValue, merchantDefault, Constants.UPDATE_ACTIVITY, storeId);
  }

  @Override
  public List<ConfigurationStatusResponse> getConfigurations(String storeId,
      List<ConfigurationStatusRequest> configurationStatusRequestList) throws Exception {
    List<ConfigurationStatusResponse> configurationStatusResponseList = new ArrayList<>();
    for (ConfigurationStatusRequest request : configurationStatusRequestList) {
      String categoryConfigurationStatus = categoryDefault;
      String merchantConfigurationStatus = merchantDefault;
      Category category = categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
          storeId, request.getCategoryCode());
      if (Objects.isNull(category)) {
        throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
            ErrorMessage.CATEGORY_NOT_FOUND.getMessage() + request.getCategoryCode());
      }
      CategoryConfiguration categoryConfiguration =
          this.categoryConfigurationService.getCategoryConfigurationByCategoryAndMarkForDeleteFalse(storeId, category);
      if (Objects.nonNull(categoryConfiguration)) {
        categoryConfigurationStatus = categoryConfiguration.getReviewConfig();
      }
      businessPartnerService.getBusinessPartnerProfile(request.getBusinessPartnerCode());
      MerchantConfiguration merchantConfiguration = this.merchantConfigurationService
          .getMerchantConfigurationByMerchantCode(storeId, request.getBusinessPartnerCode());
      if (Objects.nonNull(merchantConfiguration)) {
        merchantConfigurationStatus = merchantConfiguration.getReviewConfig();
      }
      String status = getStatus(categoryConfigurationStatus, merchantConfigurationStatus);
      if (Objects.nonNull(status)) {
        ConfigurationStatusResponse configurationStatusResponse =
            ConfigurationStatusResponse.builder().categoryCode(request.getCategoryCode())
                .merchantCode(request.getBusinessPartnerCode()).reviewConfig(status).build();
        configurationStatusResponseList.add(configurationStatusResponse);
      }
    }
    return configurationStatusResponseList;
  }

  private String getStatus(String categoryStatus, String merchantStatus) {
    return merchantDefault.equals(merchantStatus) ? categoryStatus : merchantStatus;
  }

  private CategoryConfiguration setNewCategoryConfiguration(Category category, String reviewConfig, String storeId,
      String username) {
    CategoryConfiguration categoryConfiguration = new CategoryConfiguration();
    categoryConfiguration.setCategory(category);
    categoryConfiguration.setReviewConfig(reviewConfig);
    categoryConfiguration.setStoreId(storeId);
    categoryConfiguration.setCreatedBy(username);
    categoryConfiguration.setUpdatedBy(username);
    categoryConfiguration.setMarkForDelete(false);
    return categoryConfiguration;
  }

  private CategoryConfiguration setExistingCategoryConfiguration(CategoryConfiguration categoryConfiguration,
      String reviewConfig, String username) {
    categoryConfiguration.setReviewConfig(reviewConfig);
    categoryConfiguration.setMarkForDelete(false);
    categoryConfiguration.setUpdatedBy(username);
    categoryConfiguration.setCreatedBy(username);
    return categoryConfiguration;
  }

  private void saveHistoryForCategory(String username, String categoryCode, String categoryName, String oldValue,
      String newValue, String activity, String storeId) {
    CategoryConfigurationHistory categoryConfigurationHistory = ConverterUtil
        .generateCategoryConfigurationHistory(username, categoryCode, categoryName, oldValue, newValue, activity,
            storeId);
    categoryConfigurationHistoryService.saveCategoryHistoryConfiguration(categoryConfigurationHistory);
  }

  private MerchantConfiguration setNewMerchantConfiguration(MerchantConfigurationRequest request, String storeId,
      String mainCategory, String merchantName, String username) {
    MerchantConfiguration merchantConfiguration = new MerchantConfiguration();
    merchantConfiguration.setMerchantCode(request.getBusinessPartnerCode());
    merchantConfiguration.setMerchantName(merchantName);
    merchantConfiguration.setReviewConfig(request.getReviewConfig());
    merchantConfiguration.setStoreId(storeId);
    merchantConfiguration.setCategoryName(mainCategory);
    merchantConfiguration.setUpdatedBy(username);
    merchantConfiguration.setCreatedBy(username);
    merchantConfiguration.setStoreId(storeId);
    merchantConfiguration.setMarkForDelete(false);
    return merchantConfiguration;
  }

  private MerchantConfiguration setExistingMerchantConfiguration(MerchantConfiguration merchantConfiguration,
      String reviewConfig, String username) {
    merchantConfiguration.setReviewConfig(reviewConfig);
    merchantConfiguration.setMarkForDelete(false);
    merchantConfiguration.setUpdatedBy(username);
    merchantConfiguration.setCreatedBy(username);
    return merchantConfiguration;
  }

  private void saveHistoryForMerchant(String username, String merchantName, String merchantCode, String oldValue,
      String newValue, String activity, String storeId) {
    MerchantConfigurationHistory merchantConfigurationHistory = ConverterUtil
        .generateMerchantConfigurationHistory(username, merchantName, merchantCode, oldValue, newValue, activity,
            storeId);
    merchantConfigurationHistoryService.saveMerchantHistoryConfiguration(merchantConfigurationHistory);
  }

  private void checkCategoryReviewConfigFlag(String reviewConfig, String categoryCode) {
    if (!categoryDefault.equals(reviewConfig) && !Constants.POST_LIVE_STATUS.equals(reviewConfig)) {
      log.error("review config flag is invalid : {}", reviewConfig);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.REVIEW_CONFIG_FLAG_INVALID.getMessage() + categoryCode);
    }
  }

  private void checkMerchantReviewConfigFlag(String reviewConfig, String merchantCode) {
    if (!Constants.PRE_LIVE_FLAG.equals(reviewConfig) && !Constants.POST_LIVE_STATUS.equals(reviewConfig)
        && !merchantDefault.equals(reviewConfig)) {
      log.error("review config flag is invalid : {}", reviewConfig);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.REVIEW_CONFIG_FLAG_INVALID_FOR_MERCHANT.getMessage() + merchantCode);
    }
  }

  @Transactional(readOnly = false)
  public List<BulkMerchantConfigUploadResponse> bulkMerchantConfigUpload(String storeId,
      List<MerchantConfigurationRequest> merchantConfigurationRequestList) throws Exception {
    log.info("Bulk merchant configuration upload with request : {}", merchantConfigurationRequestList);
    List<BulkMerchantConfigUploadResponse> merchantConfigUploadResponseList = new ArrayList<>();
    List<MerchantConfigurationRequest> successData = new ArrayList<>();
    ProfileResponse profileResponse;
    for (MerchantConfigurationRequest request : merchantConfigurationRequestList) {
      BulkMerchantConfigUploadResponse response = new BulkMerchantConfigUploadResponse();
      BeanUtils.copyProperties(request, response);
      try {
        profileResponse = businessPartnerService.getBusinessPartnerProfile(request.getBusinessPartnerCode());
      } catch (ApplicationRuntimeException e) {
        log.error("Not able to find the business partner {} to add in configuration", request.getBusinessPartnerCode(), e);
        response.setErrorMessage(ErrorMessage.NOT_VALID_MERCHANT.getMessage());
        merchantConfigUploadResponseList.add(response);
        continue;
      }
      request.setBusinessPartnerName(profileResponse.getCompany().getBusinessPartnerName());
      successData.add(request);
      try {
        addMerchantConfiguration(storeId, successData);
      } catch (ApplicationRuntimeException e) {
        log.error("Not able to add merchant {} in merchant configuration", request.getBusinessPartnerCode(), e);
        response.setErrorMessage(e.getErrorCodes().getMessage());
        merchantConfigUploadResponseList.add(response);
        continue;
      } catch (Exception e) {
        log.error("Not able to add merchant {} in merchant configuration", request.getBusinessPartnerCode(), e);
        response.setErrorMessage(ErrorMessage.ERROR_UPDATING_MERCHANT_CONFIG.getMessage());
        merchantConfigUploadResponseList.add(response);
        continue;
      }
      response.setErrorMessage(Constants.SUCCESS);
      merchantConfigUploadResponseList.add(response);
    }
    return merchantConfigUploadResponseList;
  }

  @Override
  @Transactional(readOnly = false)
  public List<BulkCategoryConfigUploadResponse> bulkCategoryConfigUpload(String storeId,
      List<CategoryConfigurationRequest> categoryConfigurationRequestList) throws Exception {
    log.info("Bulk category configuration upload with request : {}", categoryConfigurationRequestList);
    List<String> categoryCodeList =
        categoryConfigurationRequestList.stream().map(CategoryConfigurationRequest::getCategoryCode)
            .collect(Collectors.toList());
    List<Category> categoryList = null;
    try {
      categoryList = this.categoryService.findByStoreIdAndCategoryCodes(storeId, categoryCodeList);
    } catch (ApplicationRuntimeException e) {
      log.error("No Category Codes are not valid to add in category configuration.", e);
      throw e;
    }
    Map<String, Category> categoryMap =
        categoryList.stream().collect(Collectors.toMap(Category::getCategoryCode, Function.identity()));
    List<BulkCategoryConfigUploadResponse> categoryConfigUploadResponseList = new ArrayList<>();
    for (CategoryConfigurationRequest request : categoryConfigurationRequestList) {
      BulkCategoryConfigUploadResponse response = new BulkCategoryConfigUploadResponse();
      BeanUtils.copyProperties(request, response);
      if (Objects.nonNull(categoryMap.get(request.getCategoryCode()))) {
        request.setCategoryName(categoryMap.get(request.getCategoryCode()).getName());
      } else {
        response.setErrorMessage(ErrorMessage.NOT_VALID_CATEGORY.getMessage());
        categoryConfigUploadResponseList.add(response);
        continue;
      }
      try {
        addCategoryConfiguration(storeId, Arrays.asList(request));
      } catch (ApplicationRuntimeException e) {
        log.error("Not able to add category {} in category configuration ", request.getCategoryCode(), e);
        response.setErrorMessage(e.getErrorCodes().getMessage());
        categoryConfigUploadResponseList.add(response);
        continue;
      } catch (Exception e) {
        log.error("Not able to add category {} in category configuration ", request.getCategoryCode(), e);
        response.setErrorMessage(ErrorMessage.ERROR_UPDATING_CATEGORY_CONFIG.getMessage());
        categoryConfigUploadResponseList.add(response);
        continue;
      }
      response.setErrorMessage(Constants.SUCCESS);
      categoryConfigUploadResponseList.add(response);
    }
    return categoryConfigUploadResponseList;
  }

  @Override
  public Page<MerchantConfigurationHistoryResponse> getMerchantConfigurationHistory(String storeId, String merchantCode,
      int page, int size) {
    Pageable pageable = PageRequest.of(page, size);
    Page<MerchantConfigurationHistory> merchantConfigurationHistoryPage =
        this.merchantConfigurationHistoryService.getMerchantConfigurationHistoryPage(storeId, merchantCode, pageable);
    return new PageImpl<>(Optional.ofNullable(merchantConfigurationHistoryPage.getContent()).orElse(new ArrayList<>()).stream().map(
        ConverterUtil::toMerchantConfigurationHistoryResponse)
        .collect(Collectors.toList()), pageable, merchantConfigurationHistoryPage.getTotalElements());
  }

  @Override
  public List<BulkConfigDataResponse> fetchConfigDetailsByConfigTypeForCodes(String storeId, String configType,
      List<String> codes) throws Exception {
    List<BulkConfigDataResponse> dataResponses = new ArrayList<>();
    if (MERCHANT.equalsIgnoreCase(configType)) {
      List<MerchantConfiguration> merchantConfigurationList =
          merchantConfigurationService.getMerchantConfigurationsByMerchantCodeList(storeId, codes);
      getBulkConfigDataResponseFromMerchantConfig(dataResponses, merchantConfigurationList);
    } else {
      List<Category> categoryList = categoryService.findByStoreIdAndCategoryCodes(storeId, codes);
      Map<String, CategoryConfiguration> categoryConfigurationMap =
          this.categoryConfigurationService.getCategoryConfigurationsByCategoryList(storeId, categoryList).stream()
              .collect(Collectors.toMap(categoryConfiguration -> categoryConfiguration.getCategory().getCategoryCode(),
                  Function.identity()));
      getBulkConfigDataResponseFromCategoryConfig(dataResponses, categoryList, categoryConfigurationMap);
    }
    return dataResponses;
  }

  private void getBulkConfigDataResponseFromCategoryConfig(List<BulkConfigDataResponse> dataResponses,
      List<Category> categoryList, Map<String, CategoryConfiguration> categoryConfigurationMap) {
    for (Category category : categoryList) {
      CategoryConfiguration categoryConfiguration = categoryConfigurationMap.get(category.getCategoryCode());
      BulkConfigDataResponse bulkConfigDataResponse =
          BulkConfigDataResponse.builder().code(category.getCategoryCode()).name(category.getName())
              .reviewConfig(categoryConfiguration.getReviewConfig()).build();
      dataResponses.add(bulkConfigDataResponse);
    }
  }

  private void getBulkConfigDataResponseFromMerchantConfig(List<BulkConfigDataResponse> dataResponses,
      List<MerchantConfiguration> merchantConfigurationList) {
    for (MerchantConfiguration merchantConfiguration : merchantConfigurationList) {
      BulkConfigDataResponse bulkConfigDataResponse =
          BulkConfigDataResponse.builder().name(merchantConfiguration.getMerchantName())
              .code(merchantConfiguration.getMerchantCode()).reviewConfig(merchantConfiguration.getReviewConfig())
              .build();
      dataResponses.add(bulkConfigDataResponse);
    }
  }

  @Override
  public List<ConfigurationStatusResponse> getConfigurationChangesByDate(String storeId, Date fromDate) {
    List<ConfigurationStatusResponse> configurationStatusResponses = new ArrayList<>();
    addCategoryConfigChanges(storeId, fromDate, configurationStatusResponses);
    addMerchantConfigChanges(storeId, fromDate, configurationStatusResponses);
    return configurationStatusResponses;
  }

  private void addMerchantConfigChanges(
      String storeId, Date fromDate, List<ConfigurationStatusResponse> configurationStatusResponses) {
    Pageable pageable = PageRequest.of(FIRST_PAGE, PAGE_SIZE);
    Page<MerchantConfiguration> merchantConfigurationChangesPage;
    do {
      merchantConfigurationChangesPage =
          merchantConfigurationService.getMerchantConfigurationByUpdatedDateGreaterThan(storeId, fromDate, pageable);
      for (MerchantConfiguration merchantConfiguration : merchantConfigurationChangesPage.getContent()) {
        ConfigurationStatusResponse configurationStatusResponse;
        if(merchantConfiguration.isMarkForDelete()) {
          configurationStatusResponse = ConfigurationStatusResponse.builder()
              .merchantCode(merchantConfiguration.getMerchantCode())
              .reviewConfig(Constants.NEUTRAL_STATUS).build();
        } else {
          configurationStatusResponse = ConfigurationStatusResponse.builder()
              .merchantCode(merchantConfiguration.getMerchantCode())
              .reviewConfig(merchantConfiguration.getReviewConfig()).build();
        }
        configurationStatusResponses.add(configurationStatusResponse);
      }
      pageable = pageable.next();
    } while (merchantConfigurationChangesPage.hasNext());
  }

  private void addCategoryConfigChanges(
      String storeId, Date fromDate, List<ConfigurationStatusResponse> configurationStatusResponses) {
    Pageable pageable = PageRequest.of(FIRST_PAGE, PAGE_SIZE);
    Page<CategoryConfiguration> categoryConfigurationChangesPage;
    do {
      categoryConfigurationChangesPage =
          categoryConfigurationService.getCategoryConfigurationByUpdatedDateGreaterThan(storeId, fromDate, pageable);
      for (CategoryConfiguration categoryConfiguration : categoryConfigurationChangesPage.getContent()) {
        ConfigurationStatusResponse configurationStatusResponse;
        if(categoryConfiguration.isMarkForDelete()) {
          configurationStatusResponse = ConfigurationStatusResponse.builder()
              .categoryCode(categoryConfiguration.getCategory().getCategoryCode())
              .reviewConfig(Constants.PRE_LIVE_STATUS).build();
        } else {
          configurationStatusResponse = ConfigurationStatusResponse.builder()
              .categoryCode(categoryConfiguration.getCategory().getCategoryCode())
              .reviewConfig(categoryConfiguration.getReviewConfig()).build();
        }
        configurationStatusResponses.add(configurationStatusResponse);
      }
      pageable = pageable.next();
    } while (categoryConfigurationChangesPage.hasNext());
  }
}
