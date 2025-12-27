package com.gdn.mta.bulk.repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

import com.gdn.partners.pbp.dto.common.ListRequestDTO;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;

import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.ProductBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerResponse;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.ProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.response.ItemBulkArchiveResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.PBPFeign;
import com.gdn.mta.bulk.models.AuditTrailInfo;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemRequest;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.ListRequest;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemRequest;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemResponse;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Repository
public class ProductBusinessPartnerRepositoryBean implements ProductBusinessPartnerRepository {

  @Autowired
  private PBPFeign pbpFeign;

  @Override
  public String saveWithActivatedFalseReturnId(ProductBusinessPartnerRequest productBusinessPartner)
      throws Exception {
    GdnRestSimpleResponse<String> response =
        pbpFeign.saveProductBusinessPartnerWithActivatedFalseReturnId(
            GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getRequestId()) ? UUID
                .randomUUID().toString() : GdnMandatoryRequestParameterUtil.getRequestId(),
            StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getUsername()) ? "Merchant API"
                : GdnMandatoryRequestParameterUtil.getUsername(), productBusinessPartner);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.valueOf(response.getErrorCode()),
          response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public ProductBusinessPartnerResponse getProductBusinessPartnerById(String id) throws Exception {
    GdnRestSingleResponse<ProductBusinessPartnerResponse> response =
        pbpFeign.getProductBusinessPartner(
            GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            StringUtils
            .isEmpty(GdnMandatoryRequestParameterUtil.getRequestId()) ? UUID.randomUUID()
            .toString() : GdnMandatoryRequestParameterUtil.getRequestId(), StringUtils
            .isEmpty(GdnMandatoryRequestParameterUtil.getUsername()) ? "system"
            : GdnMandatoryRequestParameterUtil.getUsername(), id);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.valueOf(response.getErrorCode()),
          response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public List<UpsertOfflineItemFailedResponse> upsertOfflineItems(
      List<UpsertOfflineItemRequest> requests, AuditTrailInfo auditTrailInfo) throws Exception {
    log.info("Calling PBP api for upserting offline items : {}", requests);
    ListRequestDTO<UpsertOfflineItemRequest> listRequest = new ListRequestDTO<>(requests);
    GdnRestSingleResponse<UpsertOfflineItemResponse> response =
        this.pbpFeign.upsertOfflineItems(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            auditTrailInfo.getRequestId(), auditTrailInfo.getUsername(),
            auditTrailInfo.getBusinessPartnerCode(), listRequest);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
          response.getErrorMessage());
    }
    return response.getValue().getFailedProducts();
  }

  @Override
  public List<DeleteOfflineItemDetailResponse> deleteOfflineItems(String storeId,
      List<DeleteOfflineItemRequest> requests, AuditTrailInfo auditTrailInfo) throws ApplicationException {
    log.info("Calling PBP api for deleting offline items : {}", requests);
    ListRequest<DeleteOfflineItemRequest> listRequest = new ListRequest<>(requests);
    GdnRestSingleResponse<DeleteOfflineItemResponse> response = pbpFeign.bulkDeleteOfflineItem(
        storeId, Constant.CHANNEL_ID, Constant.CLIENT_ID, auditTrailInfo.getRequestId(),
        auditTrailInfo.getUsername(), auditTrailInfo.getBusinessPartnerCode(), listRequest);
    if (Objects.isNull(response) || !response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE, response.getErrorMessage());
    }
    return response.getValue().getFailedProducts();
  }

  @Override
  public List<String> bulkArchiveProductSkus(String userName, String requestId, boolean archive,
      SimpleListStringRequest request, Map<String, String> productSkuErrorMessageMap) throws ApplicationException {
    GdnRestSingleResponse<ItemBulkArchiveResponse> response = pbpFeign
        .bulkArchiveProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, requestId, userName,
            archive, request);
    if (Objects.isNull(response) || !response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE, "x-product error");
    }
    if (Objects.nonNull(response.getValue()) && !CollectionUtils.isEmpty(response.getValue().getFailedItemSkus())) {
      return response.getValue().getFailedItemSkus();
    }
    if (Objects.nonNull(response.getValue()) && !CollectionUtils.isEmpty(
      response.getValue().getProductSkuErrorMessageMap())) {
      productSkuErrorMessageMap.putAll(response.getValue().getProductSkuErrorMessageMap());
    }
    return new ArrayList<>();
  }

  @Override
  public GdnRestSingleResponse<ProductLevel3Response> updateAndReturn(ProductLevel3Request productLevel3Request,
      boolean hasOrder, String requestId, String username, String clientHost) throws ApplicationException {
    GdnRestSingleResponse<ProductLevel3Response> response = pbpFeign
        .updateAndReturn(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, requestId, username, clientHost,
            true, hasOrder, productLevel3Request);
    if (Objects.isNull(response)) {
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE, "x-product error");
    }
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response;
  }

  @Override
  public String updateProductCategory(String productCode, String categoryCode, String username) {
    GdnBaseRestResponse response = pbpFeign
        .updateProductCategory(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            username, productCode, categoryCode);
    if (!response.isSuccess() && ErrorCategory.VALIDATION.getCode().equals(response.getErrorCode())) {
      log.error("Recat-product error when update category, productCode : {} error {} ", productCode,
          response.getErrorMessage());
      return response.getErrorMessage();
    } else if (!response.isSuccess() && !ErrorCategory.VALIDATION.getCode().equals(response.getErrorCode())) {
      log.error("Recat-product error when update category, productCode : {} error {} ", productCode,
          response.getErrorMessage());
      return Constant.SYSTEM_ERROR;
    }
    return StringUtils.EMPTY;
  }

  @Override
  public BulkDownloadProductLevel3Response bulkDownloadSummary(ProductLevel3SummaryRequest request,
      String businessPartnerCode, String fetchViewConfigByChannel) throws ApplicationException {
    GdnRestSingleResponse<BulkDownloadProductLevel3Response> response =
        pbpFeign.bulkDownloadSummaryFromDb(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, businessPartnerCode, true, request,
            fetchViewConfigByChannel);

    if (Objects.isNull(response)) {
      log.error("PBP error : {} ", ErrorCategory.COMMUNICATION_FAILURE);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE, "PBP error");
    }
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      log.error("error message: {} ", response.getErrorMessage());
      throw  new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }


}
