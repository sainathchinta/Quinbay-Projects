package com.gdn.mta.bulk.service;

import java.util.List;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.dto.product.BulkConfigurationUpdateRequest;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryConfigurationRequestList;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.request.MerchantConfigurationRequestList;
import com.gdn.x.productcategorybase.dto.response.BulkCategoryConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;
import com.gdn.x.productcategorybase.dto.response.BulkMerchantConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;

@Service
public class BulkConfigurationServiceBean implements BulkConfigurationService {

  private static final Logger LOGGER = LoggerFactory.getLogger(BulkConfigurationServiceBean.class);

  @Autowired
  private PCBOutboundService pcbOutboundService;

  @Override
  public List<BulkMerchantConfigUploadResponse> bulkMerchantConfigUpload(List<MerchantConfigurationRequest> request,
      BulkConfigurationUpdateRequest bulkConfigurationUpdateRequest) throws Exception {
    LOGGER.info(
        "Calling PCB for bulk merchant configuration upload with request : {} and bulkConfigurationUpdateRequest : {}",
        request, bulkConfigurationUpdateRequest);
    MerchantConfigurationRequestList merchantConfigurationRequestList =
        MerchantConfigurationRequestList.builder().merchantConfigurationRequestList(request).build();
    GdnRestListResponse<BulkMerchantConfigUploadResponse> response = pcbOutboundService
        .bulkMerchantConfigUpload(bulkConfigurationUpdateRequest.getStoreId(), Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkConfigurationUpdateRequest.getRequestId(), bulkConfigurationUpdateRequest.getUpdatedBy(),
            merchantConfigurationRequestList);
    if (Objects.isNull(response) || !response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "PCB error for bulk merchant configuration upload: " + response);
    }
    return response.getContent();
  }

  @Override
  public List<BulkCategoryConfigUploadResponse> bulkCategoryConfigUpload(List<CategoryConfigurationRequest> request,
      BulkConfigurationUpdateRequest bulkConfigurationUpdateRequest) throws Exception {
    LOGGER.info(
        "Calling PCB for bulk category configuration upload with request : {} and bulkConfigurationUpdateRequest : {}",
        request, bulkConfigurationUpdateRequest);
    CategoryConfigurationRequestList categoryConfigurationRequestList =
        CategoryConfigurationRequestList.builder().categoryConfigurationRequestList(request).build();
    GdnRestListResponse<BulkCategoryConfigUploadResponse> response = pcbOutboundService
        .bulkCategoryConfigUpload(bulkConfigurationUpdateRequest.getStoreId(), Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkConfigurationUpdateRequest.getRequestId(), bulkConfigurationUpdateRequest.getUpdatedBy(),
            categoryConfigurationRequestList);
    if (Objects.isNull(response) || !response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "PCB error for bulk category configuration upload: " + response);
    }
    return response.getContent();
  }

  @Override
  public List<BulkConfigDataResponse> fetchConfigDetailsByCodes(String storeId, String configType,
      BulkDownloadRequest bulkDownloadRequest, List<String> codeList) throws Exception {
    AttributeCodesRequest listRequest = new AttributeCodesRequest();
    listRequest.setAttributeCodes(codeList);
    GdnRestListResponse<BulkConfigDataResponse> response = pcbOutboundService
        .fetchConfigDetailsByCodes(storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID, bulkDownloadRequest.getRequestId(),
            bulkDownloadRequest.getUsername(), configType, listRequest);
    if (Objects.isNull(response) || !response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "PCB error for multiple configuration download: " + response);
    }
    return response.getContent();
  }

  @Override
  public GdnRestListResponse<CategoryConfigurationFilterResponse> getCategoryConfigurationList(String storeId,
      String channelId, String clientId, String requestId, String username,
      ConfigurationFilterRequest configurationFilterRequest, int pageNumber, int maxSize) throws ApplicationException {

    GdnRestListResponse<CategoryConfigurationFilterResponse> response = pcbOutboundService
        .getCategoryConfigurationList(storeId, channelId, clientId, requestId, username, configurationFilterRequest,
            pageNumber, maxSize);
    if (Objects.isNull(response) || !response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "PCB error for fetching category configuration list : " + response);
    }
    return response;
  }

  @Override
  public GdnRestListResponse<MerchantConfigurationFilterResponse> getMerchantConfigurationList(String storeId,
      String channelId, String clientId, String requestId, String username,
      ConfigurationFilterRequest configurationFilterRequest, int pageNumber, int maxSize) throws ApplicationException {
    GdnRestListResponse<MerchantConfigurationFilterResponse> response = pcbOutboundService
        .getMerchantConfigurationList(storeId, channelId, clientId, requestId, username, configurationFilterRequest,
            pageNumber, maxSize);
    if (Objects.isNull(response) || !response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "PCB error for fetching merchant configuration list :" + response);
    }
    return response;
  }
}
