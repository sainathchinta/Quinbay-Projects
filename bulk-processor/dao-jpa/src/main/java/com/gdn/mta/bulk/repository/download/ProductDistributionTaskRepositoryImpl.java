package com.gdn.mta.bulk.repository.download;

import com.gdn.mta.bulk.models.download.responsedata.DistributionProductResponse;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.feignConfig.PDTFeign;
import com.gdn.mta.bulk.models.BrandReport;
import com.gdn.mta.bulk.models.download.IPRProductListRequest;
import com.gdn.mta.bulk.models.download.responsedata.IprProductsResponse;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.mta.bulk.models.IprActionRequest;
import com.gdn.x.mta.distributiontask.model.enums.ProductStateIPR;
import com.gdn.x.mta.distributiontask.rest.model.request.BoostedProductFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import org.apache.commons.collections.CollectionUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.response.ProductCodeResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkVendorProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.BulkVendorProductActionsResponse;
import lombok.extern.slf4j.Slf4j;

import java.util.Objects;

@Slf4j
@Repository
public class ProductDistributionTaskRepositoryImpl implements ProductDistributionTaskRepository {

  @Autowired
  private PDTFeign pdtFeign;

  @Override
  public Page<DistributionProductResponse> getProductsForVendor(String requestId, String username, Pageable pageable,
      ProductListRequest request) throws Exception {
    GdnRestListResponse<DistributionProductResponse> response =
        pdtFeign.getProductListingForVendorCode(Constant.STORE_ID, requestId, Constant.CHANNEL_ID,
            Constant.CLIENT_ID, username, pageable.getPageNumber(),
            pageable.getPageSize(), request);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), pageable, response.getPageMetaData().getTotalRecords());
  }

  @Override
  public GdnRestListResponse<DistributionProductResponse> getVendorFilteredProducts(String username, String requestId,
      Pageable pageable, FilterSummaryRequest request) throws Exception {
    try {
      GdnRestListResponse<DistributionProductResponse> response = pdtFeign
          .getProductList(Constant.STORE_ID, requestId, Constant.CHANNEL_ID,
              Constant.CLIENT_ID, username, pageable.getPageNumber(), pageable.getPageSize(), request);
      if (!response.isSuccess()) {
        throw new ApplicationException(ErrorCategory.DATA_ACCESS, response.getErrorMessage());
      }
      return response;
    } catch (Exception e) {
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE, "PDT client error: " + e.getMessage());
    }
  }

  @Override
  public BulkVendorProductActionsResponse bulkVendorProductActions(BulkVendorProductActionsRequest request)
      throws Exception {
    try {
      GdnRestSingleResponse<BulkVendorProductActionsResponse> response =
          pdtFeign.bulkVendorProductActions(Constant.STORE_ID, request.getRequestId(), Constant.CHANNEL_ID,
              Constant.CLIENT_ID, request.getUserName(), request);
      if (!response.isSuccess()) {
        throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
      }
      return response.getValue();
    } catch (Exception e) {
      log.error("Error when vendor bulk assign request {} ", request, e);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE, "PDT client error: " + e.getMessage());
    }
  }

  public GdnRestListResponse<ProductCodeResponse> fetchProductsForAutoAssignment(String requestId
    , String username, Integer page, Integer size, BoostedProductFilterRequest boostedProductFilterRequest)
    throws ApplicationException {
    try {
      GdnRestListResponse<ProductCodeResponse> response =
          pdtFeign.fetchProductsForAutoAssignment(Constant.STORE_ID, requestId, Constant.CHANNEL_ID,
              Constant.CLIENT_ID, username, page, size, boostedProductFilterRequest);
      if (!response.isSuccess() || CollectionUtils.isEmpty(response.getContent())) {
        throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
      }
      return response;
    } catch (Exception e) {
      log.error("Error when vendor bulk assign request {} ", requestId, e);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE, "PDT client error: " + e.getMessage());
    }
  }

  @Override
  public String vendorRejection(String storeId, String username,
         String vendorCode, RejectProductVendorRequest rejectProductVendorRequest) {
    GdnBaseRestResponse response = pdtFeign.vendorRejection(storeId,
      MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
      MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
      MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER), username, vendorCode,
      rejectProductVendorRequest);
    if (Objects.isNull(response)) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, "null response from PDT");
    } else if (!response.isSuccess()) {
      return response.getErrorMessage();
    } else {
      return null;
    }
  }

  @Override
  public GdnRestSingleResponse<VendorQuickApprovalResponse> vendorQuickApproval(String storeId,
    String userName,
    VendorQuickApprovalRequest vendorQuickApprovalRequest) {
    GdnRestSingleResponse<VendorQuickApprovalResponse> response =
      pdtFeign.vendorQuickApproval(storeId,
        MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
        MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
        MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER), userName,
        vendorQuickApprovalRequest);
    if (Objects.isNull(response)) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, "null response from PDT");
    } else {
      return response;
    }
  }

  @Override
  public GdnBaseRestResponse performIprAction(String storeId, String userName,
      IprActionRequest iprActionRequest) {
    GdnBaseRestResponse response;
    if (ProductStateIPR.IN_REVIEW.name().equals(iprActionRequest.getAction())) {
      if (Objects.isNull(iprActionRequest.getBrandReport())) {
        response = pdtFeign.addProductToIPR(storeId,
            MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER), Constant.REQUEST_ID,
            userName, iprActionRequest.getProductSku(), iprActionRequest.getSource(),
            iprActionRequest.getAssignee(), new BrandReport());
      } else {
        response = pdtFeign.addProductToIPR(storeId,
            MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER), Constant.REQUEST_ID,
            userName, iprActionRequest.getProductSku(), iprActionRequest.getSource(),
            iprActionRequest.getAssignee(), iprActionRequest.getBrandReport());
      }
    } else {
      response = pdtFeign.performIprAction(storeId,
          MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
          MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER), Constant.REQUEST_ID,
          userName, iprActionRequest);
    }
    if (Objects.isNull(response)) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, "null response from PDT");
    } else if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION ,response.getErrorMessage());
    } else {
      return response;
    }
  }

  @Override
  public GdnRestListResponse<IprProductsResponse> getIprProductsList(String requestId,
    IPRProductListRequest request, int page, int size) {
    GdnRestListResponse<IprProductsResponse> response =
      pdtFeign.getIprProductsList(Constant.STORE_ID, requestId, Constant.CHANNEL_ID,
        Constant.CLIENT_ID, Constant.USER_NAME, request, page, size);
    validateResponse(response, request);
    return response;
  }

  private void validateResponse(GdnRestListResponse<IprProductsResponse> response,
      IPRProductListRequest request) {
    if (Objects.isNull(response)) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    if (!response.isSuccess()) {
      log.error("Error fetching ipr products download for request {}, " + "error : {}", request,
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }
}
