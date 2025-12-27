package com.gdn.partners.pcu.internal.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.product.BulkConfigurationUpdateRequest;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.client.feign.XBPFeign;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.PostLiveConfigurationService;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.service.model.BulkInternalProcessType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.internal.streaming.model.bulk.CategoryConfigurationDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.DownloadType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.FileType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.MerchantConfigurationDownloadRequest;
import com.gdn.partners.pcu.internal.web.model.request.ConfigurationFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ConfigurationWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.CategoryConfigurationFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.CategoryConfigurationHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ConfigurationsStatusWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantConfigurationFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantConfigurationHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantWebSearchResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.MerchantSearchResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.ConfigurationCountResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationHistoryResponse;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class PostLiveConfigurationServiceImpl implements PostLiveConfigurationService {

  private static final String CONFIG_FLAG_EMPTY_ERROR= " Config flag must not be blank";
  private static final String EMPTY_REQUEST_ERROR = "request list can not be empty";
  private static final String DELIMETER = ".";
  private static final String LANGUAGE = "in";
  private static final String MERCHANT = "MERCHANT";

  @Autowired
  private XBPFeign xbpFeign;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Override
  public Page<MerchantWebSearchResponse> getMerchantsBySearchKeyword(String searchKeyword, int page, int size) {
    GdnRestListResponse<ProfileResponse> activeMerchantList = fetchActiveMerchantList(searchKeyword, page, size);
    List<MerchantConfigurationRequest> merchantConfigurationRequestList =
        RequestHelper.toMerchantConfigurationRequestList(activeMerchantList.getContent());
    log.info("Fetching configuration for following merchants : {}", merchantConfigurationRequestList);
    GdnRestListResponse<MerchantSearchResponse> merchantSearchResponsePage =
        this.pcbFeign.fetchMerchantSearchResult(merchantConfigurationRequestList);
    ResponseHelper.validateResponse(merchantSearchResponsePage);
    return new PageImpl<>(ResponseHelper.toMerchantWebSearchResponse(merchantSearchResponsePage.getContent()),
        PageRequest.of(page, size), activeMerchantList.getPageMetaData().getTotalRecords());
  }

  @Override
  public ConfigurationCountResponse getConfigurationCounts() {
    GdnRestSingleResponse<ConfigurationCountResponse> configurationCountResponse = pcbFeign.fetchConfigurationCounts();
    ResponseHelper.validateResponse(configurationCountResponse);
    return configurationCountResponse.getValue();
  }

  @Override
  public Page<CategoryConfigurationFilterWebResponse> filterCategoryConfiguration(
      ConfigurationFilterWebRequest configurationFilterWebRequest, int page, int size) {
    GdnRestListResponse<CategoryConfigurationFilterResponse> response = this.pcbFeign.getCategoryConfigurationList(
        RequestHelper.toConfigurationFilterRequest(configurationFilterWebRequest), page, size);
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(ResponseHelper.toCategoryConfigurationFilterWebResponseList(response.getContent()),
        PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
  }

  @Override
  public Page<MerchantConfigurationFilterWebResponse> filterMerchantConfiguration(
      ConfigurationFilterWebRequest configurationFilterWebRequest, int page, int size) {
    GdnRestListResponse<MerchantConfigurationFilterResponse> response = this.pcbFeign
        .getMerchantConfigurationList(RequestHelper.toConfigurationFilterRequest(configurationFilterWebRequest),
            page, size);
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(ResponseHelper.toMerchantConfigurationFilterWebResponseList(response.getContent()),
        PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
  }

  @Override
  public Page<CategoryConfigurationHistoryWebResponse> getCategoryConfigurationHistory(String categoryCode, int page,
      int size) {
    GdnRestListResponse<CategoryConfigurationHistoryResponse> response =
        this.pcbFeign.getCategoryConfigurationHistory(categoryCode, page, size);
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(ResponseHelper.toCategoryConfigurationHistoryWebResponseList(response.getContent()),
        PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
  }

  @Override
  public Page<MerchantConfigurationHistoryWebResponse> getMerchantConfigurationHistory(String merchantCode, int page,
      int size) {
    GdnRestListResponse<MerchantConfigurationHistoryResponse> response =
        this.pcbFeign.getMerchantConfigurationHistory(merchantCode, page, size);
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(Optional.ofNullable(response.getContent()).orElse(new ArrayList<>()).stream().map(
        merchantConfigurationHistoryResponse -> ResponseHelper
            .toMerchantConfigurationHistoryWebResponse(merchantConfigurationHistoryResponse))
        .collect(Collectors.toList()), PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
  }

  private GdnRestListResponse<ProfileResponse> fetchActiveMerchantList(String searchKeyword, int page, int size) {
    BusinessPartnerFilterRequest businessPartnerFilterRequest =
        RequestHelper.getBusinessPartnerFilterFromKeyword(searchKeyword);
    GdnRestListResponse<ProfileResponse> activeMerchantList =
        xbpFeign.getAllActiveMerchantList(businessPartnerFilterRequest, page, size);
    ResponseHelper.validateResponse(activeMerchantList);
    return activeMerchantList;
  }

  @Override
  public void addCategoryConfiguration(List<CategoryConfigurationRequest> requests) {
    if (CollectionUtils.isEmpty(requests)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, EMPTY_REQUEST_ERROR);
    }
    log.info("adding configuration for following categories : {}", requests);
    GdnBaseRestResponse gdnBaseRestResponse = pcbFeign.addCategoryConfigurationStatus(requests);
    ResponseHelper.validateResponse(gdnBaseRestResponse);
  }

  @Override
  public void updateCategoryConfiguration(String categoryCode, String reviewConfig) {
    if (StringUtils.isBlank(categoryCode)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.CATEGORY_CODE_MUST_NOT_BE_BLANK.getMessage());
    }
    if (StringUtils.isBlank(reviewConfig)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          CONFIG_FLAG_EMPTY_ERROR + reviewConfig);
    }
    log.info("updating configuration for categoryCode : {}", categoryCode);
    CategoryConfigurationRequest request = new CategoryConfigurationRequest();
    request.setCategoryCode(categoryCode);
    request.setReviewConfig(reviewConfig);
    GdnBaseRestResponse gdnBaseRestResponse = pcbFeign.updateCategoryConfigurationStatus(request);
    ResponseHelper.validateResponse(gdnBaseRestResponse);
  }

  @Override
  public void deleteCategoryConfiguration(String categoryCode) {
    if (StringUtils.isEmpty(categoryCode)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.CATEGORY_CODE_MUST_NOT_BE_BLANK.getMessage());
    }
    log.info("deleting configuration for categoryCode : {}", categoryCode);
    GdnBaseRestResponse gdnBaseRestResponse = pcbFeign.deleteCategoryConfigurationStatus(categoryCode);
    ResponseHelper.validateResponse(gdnBaseRestResponse);
  }

  @Override
  public void addMerchantConfiguration(List<MerchantConfigurationRequest> requests) {
    if (CollectionUtils.isEmpty(requests)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, EMPTY_REQUEST_ERROR);
    }
    log.info("adding configuration for following merchants : {}", requests);
    GdnBaseRestResponse gdnBaseRestResponse = pcbFeign.addMerchantConfigurationStatus(requests);
    ResponseHelper.validateResponse(gdnBaseRestResponse);
  }

  @Override
  public void updateMerchantConfiguration(String merchantCode, String reviewConfig) {
    if (StringUtils.isBlank(merchantCode)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.MERCHANT_CODE_ERROR.getMessage());
    }
    if (StringUtils.isBlank(reviewConfig)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          CONFIG_FLAG_EMPTY_ERROR);
    }
    MerchantConfigurationRequest request = new MerchantConfigurationRequest();
    request.setBusinessPartnerCode(merchantCode);
    request.setReviewConfig(reviewConfig);
    log.info("updating configuration for merchantCode : {}", merchantCode);
    GdnBaseRestResponse gdnBaseRestResponse = pcbFeign.updateMerchantConfigurationStatus(request);
    ResponseHelper.validateResponse(gdnBaseRestResponse);
  }

  @Override
  public void deleteMerchantConfiguration(String merchantCode) {
    if (StringUtils.isEmpty(merchantCode)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.MERCHANT_CODE_ERROR.getMessage());
    }
    log.info("deleting configuration for merchantCode : {}", merchantCode);
    GdnBaseRestResponse gdnBaseRestResponse = pcbFeign.deleteMerchantConfigurationStatus(merchantCode);
    ResponseHelper.validateResponse(gdnBaseRestResponse);
  }

  @Override
  public List<ConfigurationsStatusWebResponse> getConfigurationsStatus(List<ConfigurationWebRequest> request) throws Exception {
    List<ConfigurationStatusRequest> configurationStatusRequestList =
        RequestHelper.toConfigurationStatusRequestList(request);
    log.info("Fetching configurations status for following request : {}", configurationStatusRequestList);
    GdnRestListResponse<ConfigurationStatusResponse> response =
        this.pcbFeign.getConfigurationsStatusByMerchantAndCategoryCode(configurationStatusRequestList);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toConfigurationsStatusWebResponse(response.getContent());
  }

  @Override
  public void uploadBulkConfiguration(MultipartFile multipartFile, String type, String requestId, String storeId,
      String username) throws Exception {
    String baseDirPath = fileStorageService.uploadFilePath(multipartFile, requestId,
        BulkInternalProcessType.INTERNAL_BULK_CONFIGURATION.getValue());
    BulkConfigurationUpdateRequest bulkConfigurationUpdateRequest = RequestHelper
        .toBulkConfigurationUpdateRequest(storeId,
            new StringBuilder(baseDirPath).append(multipartFile.getOriginalFilename()).toString(), type,
            requestId, username);
    kafkaPublisher.send(DomainEventName.BULK_CONFIGURATION_UPDATE, username, bulkConfigurationUpdateRequest);
  }

  @Override
  public void downloadBulkConfiguration(ConfigurationFilterWebRequest configurationFilterWebRequest, String type,
      String storeId, String username) {
    String requestId = UUID.randomUUID().toString();
    log.info("invoking of Bulk Configuration Download for requestId: {} and type : {}", requestId, type);
    String fileName =
        new StringBuilder().append(requestId).append(DELIMETER).append(FileType.XLSX.name().toLowerCase()).toString();
    if (MERCHANT.equalsIgnoreCase(type)) {
      MerchantConfigurationDownloadRequest merchantRequest =
          MerchantConfigurationDownloadRequest.MerchantConfigurationDownloadRequestBuilder().storeId(storeId)
              .categoryCode(configurationFilterWebRequest.getCategoryCode()).configType(type)
              .dataList(configurationFilterWebRequest.getDataList())
              .reviewConfig(configurationFilterWebRequest.getReviewConfig())
              .searchKey(configurationFilterWebRequest.getSearchKey())
              .sortOrder(configurationFilterWebRequest.getSortOrder()).downloadType(DownloadType.ALL)
              .fileType(FileType.XLSX).bulkProcessEntity(BulkProcessEntity.CONFIGURATION_MERCHANT_SUMMARY)
              .directDownload(false).filename(fileName).emailTo(username).username(username).language(LANGUAGE)
              .requestId(requestId).build();
      this.kafkaPublisher.send(DomainEventName.BULK_DOWNLOAD_ALL_EVENT, username,merchantRequest);
    } else {
      CategoryConfigurationDownloadRequest categoryRequest =
          CategoryConfigurationDownloadRequest.CategoryConfigurationDownloadRequestBuilder().storeId(storeId)
              .categoryCode(configurationFilterWebRequest.getCategoryCode()).configType(type)
              .dataList(configurationFilterWebRequest.getDataList())
              .reviewConfig(configurationFilterWebRequest.getReviewConfig())
              .searchKey(configurationFilterWebRequest.getSearchKey())
              .sortOrder(configurationFilterWebRequest.getSortOrder()).downloadType(DownloadType.ALL)
              .fileType(FileType.XLSX).bulkProcessEntity(BulkProcessEntity.CONFIGURATION_CATEGORY_SUMMARY)
              .directDownload(false).filename(fileName).emailTo(username).username(username).language(LANGUAGE)
              .requestId(requestId).build();
      this.kafkaPublisher.send(DomainEventName.BULK_DOWNLOAD_ALL_EVENT, username,categoryRequest);
    }
  }
}
